package main

import "fmt"

func eval(opbytes *byte) int64

func main() {
	const (
		opExit = iota
		opAdd1
		opSub1
		opZero
	)
	prog := []byte{
		opZero, // start with 0
		opAdd1, // 0+1 = 1
		opAdd1, // 1+1 = 2
		opSub1, // 2-1 = 1
		opAdd1, // 1+1 = 2
		opExit, // result is 2
	}
	fmt.Println(eval(&prog[0])) // Should print 2
}
