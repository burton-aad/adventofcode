package main

import (
	"fmt"
	"sort"
	"strconv"
	"aoc/utils"
)

type elf struct {
	cals []int
}

func (e elf) sum_cals() int {
	return utils.Sum(e.cals)
}

func main() {
	elves := make([]elf, 1)
	cur_elf := &elves[len(elves)-1]

	s := utils.ReadFileLines("input")
	for _, val := range s {
		if val == "" {
			elves = append(elves, elf{})
			cur_elf = &elves[len(elves)-1]
		} else {
			i, err := strconv.Atoi(val)
			if err != nil {
				panic(err)
			}
			cur_elf.cals = append(cur_elf.cals, i)
		}
	}

	sort.Slice(elves, func(i, j int) bool { return elves[i].sum_cals() < elves[j].sum_cals() })
	fmt.Println("Part 1:", elves[len(elves)-1].sum_cals())
	fmt.Println("Part 2:", utils.Sum(utils.Map(elves[len(elves)-3:], func(e elf) int { return e.sum_cals() })))
}
