package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/ioutil"
	"os"
)

/*
TODO: write a program that finds all calls to multiconfig.Load(s interface{}),
then gets the type of all "s", gets all their field names and returns
a list of all ENV variables this would produce.

Resources:
http://goast.yuroyoro.net/

https://zupzup.org/go-ast-traversal/
https://zupzup.org/ast-manipulation-go/
https://medium.com/justforfunc/understanding-go-programs-with-go-parser-c4e88a6edb87
https://arslan.io/2017/09/14/the-ultimate-guide-to-writing-a-go-tool/

TODO: replace ParseFile with https://golang.org/pkg/go/parser/#ParseDir (or offer both)
 */


type FileAST struct {
	FileName string
	FileSource []byte
	Offset token.Pos
	AST *ast.File
}

// getFuncs returns all functions (and "methods") of the AST.
func (f *FileAST) getFuncs() []*ast.FuncDecl {
	funcs := []*ast.FuncDecl{}
	for _, d := range f.AST.Decls {
		if fn, isFn := d.(*ast.FuncDecl); isFn {
			funcs = append(funcs, fn)
		}
	}
	return funcs
}

// FIXME: printFunctionInfo works for functions, but not for methods (i.e. functions w/ receivers)
func (f *FileAST) printFunctionInfo(fun *ast.FuncDecl) {
	fmt.Printf("fun: %#v\n", fun)
	fmt.Printf("Function: %s, parameters:\n", fun.Name)
	for _, param := range fun.Type.Params.List {
		fmt.Printf("  Name: %s\n", param.Names[0])
		fmt.Printf("    ast type          : %T\n", param.Type)
		fmt.Printf("    type desc         : %+v\n", param.Type)
		fmt.Printf("    type name from src: %s\n",
			f.FileSource[param.Type.Pos()-f.Offset:param.Type.End()-f.Offset])
	}
}

func parseFile(inputFilepath string) {
	src, err := ioutil.ReadFile(inputFilepath)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	set := token.NewFileSet()
	parsedFile, err := parser.ParseFile(set, inputFilepath, src, 0)
	if err != nil {
		fmt.Println("Failed to parse package:", err)
		os.Exit(1)
	}

	fileAST := &FileAST{
		FileName: inputFilepath,
		FileSource: src,
		AST: parsedFile,
		Offset: parsedFile.Pos(),
	}

	funcs := fileAST.getFuncs()
	for _, fun := range funcs {
		fileAST.printFunctionInfo(fun)

		ast.Inspect(fun, func(node ast.Node) bool {
			switch n := node.(type) {
			case *ast.CallExpr:
				//fmt.Printf("%#v\n", n) // prints every func call expression

				selExpr := n.Fun.(*ast.SelectorExpr)
				//fmt.Printf("SelExpr: %#v\n", selExpr)

				ident := selExpr.X.(*ast.Ident)
				fmt.Printf("%v.%v\n", ident.Name, selExpr.Sel.Name)

				fmt.Printf("selExpr.Sel.Obj: %v\n", selExpr.Sel.Obj)
			}
			return true
		})
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("No input files given.")
		os.Exit(1)
	}

	for _, inputFile := range os.Args[1:] {
		parseFile(inputFile)
	}
}