package main

import (
	"aoc/utils"
	"fmt"
)

type direction int

const (
	up direction = iota
	down
	left
	right
)

type Tree struct {
	size             int
	visible_from     []direction
	viewing_distance [4]int
}

func (t *Tree) visible() bool {
	return len(t.visible_from) > 0
}

func (t *Tree) scenic_score() int {
	return int(t.viewing_distance[0] * t.viewing_distance[1] * t.viewing_distance[2] * t.viewing_distance[3])
}

type Forest struct {
	trees [][]Tree
}

const MAX_TREE_SIZE = 9

func NewForest(s []string) *Forest {
	f := &Forest{}
	for _, l := range s {
		tline := utils.Map([]byte(l), func(b byte) Tree { return Tree{int(b) - '0', []direction{}, [4]int{}} })
		f.trees = append(f.trees, tline)
	}

	init_t := func() (t []int) {
		for i := 0; i <= MAX_TREE_SIZE; i++ {
			t = append(t, -1)
		}
		return
	}

	setup_tree := func(t *Tree, trav []int, idx int, dir direction) {
		prox_i := -1
		for i := t.size; i < len(trav); i++ {
			if trav[i] > prox_i {
				prox_i = trav[i]
			}
		}
		if prox_i == -1 {
			t.visible_from = append(t.visible_from, dir)
			t.viewing_distance[dir] = idx
		} else {
			t.viewing_distance[dir] = idx - prox_i
		}
		trav[t.size] = idx
	}

	for _, l := range f.trees {
		t, tr := init_t(), init_t()
		for i, j := 0, len(l)-1; i < len(l); i, j = i+1, j-1 {
			setup_tree(&l[i], t, i, left)
			setup_tree(&l[j], tr, i, right)
		}
	}

	for c := 0; c < len(f.trees[0]); c++ {
		t, tr := init_t(), init_t()
		for i, j := 0, len(f.trees)-1; i < len(f.trees); i, j = i+1, j-1 {
			setup_tree(&f.trees[i][c], t, i, up)
			setup_tree(&f.trees[j][c], tr, i, down)
		}
	}

	return f
}

func print_forest(f *Forest) {
	for _, tline := range f.trees {
		for _, t := range tline {
			if t.visible() {
				fmt.Printf("%d", t.size)
			} else {
				fmt.Printf("X")
			}
		}
		fmt.Printf("\n")
	}
}

func main() {
	s := utils.ReadFileLines("input")
	forest := NewForest(s)
	//print_forest(forest)

	fmt.Println("Part 1: ", utils.Sum(utils.Map(forest.trees, func(tline []Tree) int {
		return utils.Sum(utils.Map(tline, func(t Tree) int {
			return utils.B2i(t.visible())
		}))
	})))

	fmt.Println("Part 2: ", utils.Max(utils.Map(forest.trees, func(tline []Tree) int {
		return utils.Max(utils.Map(tline, func(t Tree) int {
			return t.scenic_score()
		}))
	})))
}
