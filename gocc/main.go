package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"

	. "github.com/dannypsnl/gocc/internal/ast"
	"github.com/dannypsnl/gocc/internal/codegen"

	"github.com/alecthomas/participle"
)

func main() {
	cprogram := &CProgram{}
	parser, err := participle.Build(cprogram)
	if err != nil {
		panic(fmt.Errorf("Failed at create parser! Error: %s", err))
	}
	f, err := os.Open(os.Args[1])
	if err != nil {
		panic(fmt.Errorf("Failed at open input file, error: %s", err))
	}
	defer f.Close()
	err = parser.Parse(f, cprogram)
	if err != nil {
		panic(fmt.Errorf("Failed at parsing input file, error: %s", err))
	}
	c := codegen.NewCodegen()
	c.Compile(cprogram)

	compileTarget := f.Name()
	compileFileBase := strings.TrimSuffix(compileTarget, "c")
	llvmIRFile, err := os.OpenFile(compileFileBase+"ll", os.O_CREATE|os.O_TRUNC|os.O_RDWR, 0644)
	if err != nil {
		panic(fmt.Errorf("Failed at create .ll file, error: %s", err))
	}
	defer os.Remove(llvmIRFile.Name())
	llvmIRFile.WriteString(c.String())
	err = exec.Command("llc", llvmIRFile.Name()).Run()
	if err != nil {
		panic(fmt.Errorf("Failed at compile input file, error: %s", err))
	}
	err = exec.Command("as", compileFileBase+"s", "-o", compileFileBase+"o").Run()
	if err != nil {
		panic(fmt.Errorf("Failed at generate object file, error: %s", err))
	}
	os.Remove(compileFileBase + "s")
}
