package main

import (
	"aoc/utils"
	"fmt"
)

var m = map[byte]int{
	'A': 0,
	'B': 1,
	'C': 2,
	'X': 0,
	'Y': 1,
	'Z': 2,
}

// return the score
func play1(a, b int) int {
	switch {
	case a == b:
		return b + 1 + 3 // draw
	case a == ((b + 1) % 3):
		return b + 1 + 0 // defeat
	case a == ((b + 2) % 3):
		return b + 1 + 6 // victory
	default:
		panic("Should not be here")
	}
}

func play2(a int, b byte) int {
	switch b {
	case 'X':
		return ((a + 2) % 3) + 1 + 0 // defeat
	case 'Y':
		return a + 1 + 3 // draw
	case 'Z':
		return ((a + 1) % 3) + 1 + 6 // victory
	default:
		panic("Should not be here")
	}
}

func main() {
	s := utils.ReadFileLines("input")

	fmt.Println("Part 1: ", utils.Sum(utils.Map(s, func(v string) int { return play1(m[v[0]], m[v[2]]) })))
	fmt.Println("Part 2: ", utils.Sum(utils.Map(s, func(v string) int { return play2(m[v[0]], v[2]) })))
}
