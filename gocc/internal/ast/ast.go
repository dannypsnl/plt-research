package ast

type Operator int

const (
	OpMul Operator = iota
	OpDiv
	OpAdd
	OpSub
)

var operatorMap = map[string]Operator{
	"+": OpAdd,
	"-": OpSub,
	"*": OpMul,
	"/": OpDiv,
}

func (o *Operator) Capture(s []string) error {
	*o = operatorMap[s[0]]
	return nil
}

type (
	CProgram struct {
		Tops []*Top `(@@)*`
	}
	Top struct {
		Struct *Struct `@@`
		Func   *Func   `| @@`
	}
	// struct car {
	//   char *name;
	//   int price;
	// };
	Struct struct {
		Name   string   `"struct" @Ident`
		Fields []*Field `"{" (@@)* "}" ";"`
	}
	Field struct {
		Type *Type  `@@`
		Name string `@Ident ";"`
	}
	Type struct {
		Struct    *string `("struct" @Ident`
		Normal    *string `| @Ident)`
		IsPointer bool    `(@"*")?`
	}
	// int add(int x, int y) { return x + y; }
	Func struct {
		ReturnT    *Type        `@@`
		Name       string       `@Ident`
		Parameters []*Parameter `"(" ( @@ ("," @@)* )? ")"`
		IsDeclared bool         `( @";" |`
		Stmts      []*Stmt      `"{" (@@)* "}" )`
	}
	Parameter struct {
		IsVarg bool   `@"va_arg"`
		Type   *Type  `| ( @@`
		Name   string `@Ident )`
	}
	Stmt struct {
		Return   *Return   `( @@`
		FuncCall *FuncCall `| @@ )`
		End      bool      `@";"`
	}
	Return struct {
		Expr *Expr `"return" @@`
	}
	FuncCall struct {
		Name string  `@Ident`
		Args []*Expr `"(" (@@ ("," @@)*)? ")"`
	}
	// 1, 2
	// a, bc
	// (1 + 2)
	Value struct {
		Int     *int     `@Int`
		Float   *float64 `| @Float`
		String  *string  `| @String`
		Var     *string  `| @Ident`
		SubExpr *Expr    `| "( @@ )"`
	}
	// * y
	// / x
	OpFactor struct {
		Op     Operator `@("*"|"/")`
		Factor *Value   `@@`
	}
	Term struct {
		Left  *Value      `@@`
		Right []*OpFactor `{ @@ }`
	}
	// + x
	// - y
	OpTerm struct {
		Op   Operator `@("+"|"-")`
		Term *Term    `@@`
	}
	Expr struct {
		Left  *Term     `@@`
		Right []*OpTerm `(@@)*`
	}
)

func (f *Func) IsVAArg() bool {
	return f.Parameters[len(f.Parameters)-1].IsVarg
}
