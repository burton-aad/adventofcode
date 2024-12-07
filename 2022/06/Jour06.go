package main

import (
	"aoc/utils"
	"fmt"
)

const BLOCK_SIZE_1 int = 4
const BLOCK_SIZE_2 int = 14

func main() {
	s := utils.ReadFileLines("input")[0]

	for part, BLOCK_SIZE := range []int{BLOCK_SIZE_1, BLOCK_SIZE_2} {
		for i := 0; i < len(s); i++ {
			set := utils.NewSet[byte]()
			set.Add([]byte(s[i : i+BLOCK_SIZE])...)
			if set.Len() == BLOCK_SIZE {
				fmt.Printf("Part %d: %d\n", part+1, i+BLOCK_SIZE)
				break
			}
		}
	}
}
