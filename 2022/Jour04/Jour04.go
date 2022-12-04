package main

import (
	"aoc/utils"
	"fmt"
	"strconv"
	"strings"
)

type rg struct {
	min int
	max int
}

func make_rg(r string) rg {
	s := strings.Split(r, "-")
	a, _ := strconv.Atoi(s[0])
	b, _ := strconv.Atoi(s[1])
	return rg{a, b}
}

func complete_overlap(ra, rb rg) bool {
	return (ra.min <= rb.min && ra.max >= rb.max) || (rb.min <= ra.min && rb.max >= ra.max)
}

func partial_overlap(ra, rb rg) bool {
	return (ra.min <= rb.min && ra.max >= rb.min) || (rb.min <= ra.min && rb.max >= ra.min)
}

func main() {
	s := utils.ReadFileLines("input")
	rgs := utils.Map(s, func(v string) []rg { ss := strings.Split(v, ","); return []rg{make_rg(ss[0]), make_rg(ss[1])} })

	fmt.Println("Part 1: ", utils.Sum(utils.Map(rgs, func(v []rg) int { return utils.B2i(complete_overlap(v[0], v[1])) })))
	fmt.Println("Part 2: ", utils.Sum(utils.Map(rgs, func(v []rg) int { return utils.B2i(partial_overlap(v[0], v[1])) })))
}
