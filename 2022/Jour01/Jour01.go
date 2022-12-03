package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func readFile(path string) (s []string) {
	readFile, err := os.Open(path)
	if err != nil {
		panic(err)
	}
	defer readFile.Close()

	fileScanner := bufio.NewScanner(readFile)
	fileScanner.Split(bufio.ScanLines)

	for fileScanner.Scan() {
		s = append(s, fileScanner.Text())
	}

	return
}

func mapSlice[T any, M any](a []T, f func(T) M) []M {
	n := make([]M, 0, len(a))
	for _, e := range a {
		n = append(n, f(e))
	}
	return n
}

func sum(arr []int) (sum int) {
	for _, value := range arr {
		sum += value
	}
	return
}

type elf struct {
	cals []int
}

func (e elf) sum_cals() int {
	return sum(e.cals)
}

func main() {
	elves := make([]elf, 1)
	cur_elf := &elves[len(elves)-1]

	s := readFile("input")
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
	fmt.Println("Part 2:", sum(mapSlice(elves[len(elves)-3:], func(e elf) int { return e.sum_cals() })))
}
