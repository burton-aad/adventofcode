package main

import (
	"aoc/utils"
	"fmt"
	"strings"
)

func commonLetter(a string, b ...string) rune {
	for _, l := range a {
		res := utils.Map(b, func(bs string) bool { return strings.Contains(bs, string(l)) })
		if utils.All(res) {
			return l
		}
	}
	panic("invalid input")
}

func priority(a rune) int {
	if a >= 'a' {
		return int(a - 'a' + 1)
	} else {
		return int(a - 'A' + 27)
	}
}

func main() {
	s := utils.ReadFileLines("input")
	cl := make([]rune, 0, len(s))

	for _, v := range s {
		v1, v2 := v[:len(v)/2], v[len(v)/2:]
		cl = append(cl, commonLetter(v1, v2))
	}

	fmt.Println("Part1: ", utils.Sum(utils.Map(cl, func(a rune) int { return priority(a) })))

	cl = cl[:0]
	for i := 0; i < len(s); i += 3 {
		cl = append(cl, commonLetter(s[i], s[i+1:i+3]...))
	}

	fmt.Println("Part2: ", utils.Sum(utils.Map(cl, func(a rune) int { return priority(a) })))
}
