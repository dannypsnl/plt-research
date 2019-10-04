package ast

import (
	"fmt"
	"testing"

	"github.com/alecthomas/participle"
	"github.com/stretchr/testify/assert"
)

func mustBuild(rule interface{}) *participle.Parser {
	parser, err := participle.Build(rule)
	if err != nil {
		panic("failed at create parser")
	}
	return parser
}

func mustParse(rule interface{}, input string) {
	parser := mustBuild(rule)
	err := parser.ParseString(input, rule)
	if err != nil {
		panic(fmt.Errorf("failed at parsing input string, error: %s", err))
	}
}

func TestParse(t *testing.T) {
	t.Run("Type", func(t *testing.T) {
		typ := &Type{}
		mustParse(typ, `struct Car*`)
		assert.Equal(t, "Car", *typ.Struct)
		assert.Equal(t, true, typ.IsPointer)
	})
	t.Run("Struct", func(t *testing.T) {
		s := &Struct{}
		mustParse(s, `
		struct Car {
			char *name;
			int price;
		};
		`)
		assert.Equal(t, s.Name, "Car")
		assert.Equal(t, *s.Fields[0].Type.Normal, "char")
		assert.Equal(t, s.Fields[0].Type.IsPointer, true)
		assert.Equal(t, s.Fields[0].Name, "name")
		assert.Equal(t, *s.Fields[1].Type.Normal, "int")
		assert.Equal(t, s.Fields[1].Type.IsPointer, false)
		assert.Equal(t, s.Fields[1].Name, "price")
	})
	t.Run("Func", func(t *testing.T) {
		f := &Func{}
		mustParse(f, `int add(int x, int y) {
			return x + y;
		}`)
		assert.Equal(t, *f.ReturnT.Normal, "int")
		assert.Equal(t, f.Name, "add")
		assert.Equal(t, f.Parameters[0].Name, "x")
		assert.Equal(t, f.Parameters[1].Name, "y")

		mustParse(f, `int printf(char *format, va_arg);`)
		assert.Equal(t, f.Name, "printf")
		assert.Equal(t, f.IsDeclared, true)
		assert.Equal(t, f.Parameters[1].IsVarg, true)
	})
	t.Run("FuncCall", func(t *testing.T) {
		fc := &FuncCall{}
		mustParse(fc, `printf("hello, world!")`)
		assert.Equal(t, fc.Name, "printf")
		assert.Equal(t, *fc.Args[0].Left.Left.String, "hello, world!")
	})
	t.Run("Expr", func(t *testing.T) {
		e := &Expr{}
		mustParse(e, `x + y`)
		assert.Equal(t, *e.Left.Left.Var, "x")
		assert.Equal(t, e.Right[0].Op, OpAdd)
		assert.Equal(t, *e.Right[0].Term.Left.Var, "y")
	})
}
