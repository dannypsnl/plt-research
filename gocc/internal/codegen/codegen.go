package codegen

import (
	"reflect"

	. "github.com/dannypsnl/gocc/internal/ast"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

type Codegen struct {
	mod *ir.Module
}

func NewCodegen() *Codegen {
	return &Codegen{
		mod: ir.NewModule(),
	}
}

func (c *Codegen) getTypeByName(tName string) types.Type {
	switch tName {
	case "int":
		return types.I64
	case "float":
		return types.Float
	default:
		panic("unsupported type")
	}
}

func (c *Codegen) Compile(cprogram *CProgram) {
	for _, t := range cprogram.Tops {
		c.compileStruct(t.Struct)
		c.compileFunc(t.Func)
	}
}

func (c *Codegen) compileFunc(f *Func) {
	if f == nil {
		return
	}
	rt := c.getTypeByName(*f.ReturnT.Normal)
	paramTypes := make([]*ir.Param, 0)
	for _, p := range f.Parameters {
		pt := c.getTypeByName(*p.Type.Normal)
		paramTypes = append(paramTypes, ir.NewParam(p.Name, pt))
	}
	newFunc := c.mod.NewFunc(f.Name, rt, paramTypes...)
	newFunc.Sig.Variadic = f.IsVAArg()

	block := newFunc.NewBlock("entry")
	for _, s := range f.Stmts {
		c.compileStmt(s, block, *f.ReturnT.Normal)
	}
}

func (c *Codegen) compileStmt(s *Stmt, builder *ir.Block, retT string) {
	if s.Return != nil {
		v, t := c.compileExpr(s.Return.Expr, builder)
		if t != retT {
			panic("unexpected return type")
		}
		builder.NewRet(v)
	}
}

func (c *Codegen) compileExpr(e *Expr, builder *ir.Block) (value value.Value, cType string) {
	left, lt := c.compileTerm(e.Left, builder)
	for _, opTerm := range e.Right {
		right, rt := c.compileTerm(opTerm.Term, builder)
		operation := getOperation(builder, opTerm.Op, lt, rt)
		left = operation(left, right)
	}
	return left, lt
}

func (c *Codegen) compileTerm(t *Term, builder *ir.Block) (value value.Value, cType string) {
	left, lt := c.compileValue(t.Left)
	for _, opFactor := range t.Right {
		right, rt := c.compileValue(opFactor.Factor)
		operation := getOperation(builder, opFactor.Op, lt, rt)
		left = operation(left, right)
	}
	return left, lt
}

func getOperation(builder *ir.Block, op Operator, lt, rt string) func(value.Value, value.Value) value.Value {
	if lt != rt {
		panic("left & right contains different type")
	}
	switch lt {
	case "int":
		switch op {
		case OpAdd:
			return binaryOperator(builder.NewAdd)
		case OpSub:
			return binaryOperator(builder.NewSub)
		case OpMul:
			return binaryOperator(builder.NewMul)
		case OpDiv:
			return binaryOperator(builder.NewSDiv)
		}
	case "float":
		switch op {
		case OpAdd:
			return binaryOperator(builder.NewFAdd)
		case OpSub:
			return binaryOperator(builder.NewFSub)
		case OpMul:
			return binaryOperator(builder.NewFMul)
		case OpDiv:
			return binaryOperator(builder.NewFDiv)
		}
	}
	panic("unsupported yet")
}

func (c *Codegen) compileValue(v *Value) (value value.Value, cType string) {
	switch {
	case v.Int != nil:
		return constant.NewInt(types.I64, int64(*v.Int)), "int"
	case v.Float != nil:
		return constant.NewFloat(types.Float, *v.Float), "float"
	}
	panic("unsupported yet")
}

func (c *Codegen) compileStruct(s *Struct) {
	if s == nil {
		return
	}
	fields := []types.Type{}
	for _, f := range s.Fields {
		fields = append(fields, c.getTypeByName(*f.Type.Normal))
	}
	c.mod.NewTypeDef(s.Name, types.NewStruct(fields...))
}

func (c *Codegen) String() string {
	return c.mod.String()
}

func binaryOperator(f interface{}) func(value.Value, value.Value) value.Value {
	return func(left, right value.Value) value.Value {
		rf := reflect.ValueOf(f)
		return rf.Call(
			[]reflect.Value{reflect.ValueOf(left), reflect.ValueOf(right)},
		)[0].Interface().(value.Value)
	}
}
