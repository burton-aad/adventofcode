package main

import (
	"aoc/utils"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

type Crate[T any] struct {
	s []T
}

func (c *Crate[T]) push(val T) {
	c.s = append(c.s, val)
}

func (c *Crate[T]) front() T {
	return c.s[len(c.s)-1]
}

func (c *Crate[T]) pop() (r T) {
	r = c.s[len(c.s)-1]
	c.s = c.s[:len(c.s)-1]
	return
}

type Cargo[T any] struct {
	crates []Crate[T]
}

func (c *Cargo[T]) move(s string, v9001 bool) {
	re := regexp.MustCompile(`move (\d+) from (\d+) to (\d+)`)
	match := re.FindStringSubmatch(s)
	num, _ := strconv.Atoi(match[1])
	fc, _ := strconv.Atoi(match[2])
	tc, _ := strconv.Atoi(match[3])
	from, to := &c.crates[fc-1], &c.crates[tc-1]
	if v9001 {
		to.s = append(to.s, from.s[len(from.s)-num:]...)
		from.s = from.s[:len(from.s)-num]
	} else {
		for i := 0; i < num; i++ {
			to.push(from.pop())
		}
	}
}

func fill_stack(fs []string) (st Cargo[string]) {
	last_line := len(fs) - 1
	for i := 0; i < len(fs[last_line]); i++ {
		_, err := strconv.Atoi(string(fs[last_line][i]))
		if err != nil {
			continue
		}
		st.crates = append(st.crates, Crate[string]{})
		for j := len(fs) - 2; j >= 0 && fs[j][i] != ' '; j-- {
			st.crates[len(st.crates)-1].push(string(fs[j][i]))
		}
	}
	return
}

func main() {
	s := utils.ReadFileLines("input")
	lsep := utils.IndexOf(s, "")

	carg := fill_stack(s[:lsep])
	for _, mv := range s[lsep+1:] {
		carg.move(mv, false)
	}
	fmt.Println("Part 1: ", strings.Join(utils.Map(carg.crates, func(c Crate[string]) string { return c.front() }), ""))

	carg = fill_stack(s[:lsep])
	for _, mv := range s[lsep+1:] {
		carg.move(mv, true)
	}
	fmt.Println("Part 2: ", strings.Join(utils.Map(carg.crates, func(c Crate[string]) string { return c.front() }), ""))
}
