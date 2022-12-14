package main

import (
	"aoc/utils"
	"fmt"
	"strconv"
	"strings"
)

const (
	ROCK        byte = '#'
	AIR              = '.'
	SAND             = 'o'
	START_POINT      = '+'
	START_X     int  = 500
)

type Pos struct {
	x, y int
}

type Cave struct {
	rocks            utils.Set[Pos]
	xmin, xmax, ymax int
	start            Pos
	sand             utils.Set[Pos]
}

func parse_input(s []string) (c Cave) {
	c.rocks = *utils.NewSet[Pos]()
	c.sand = *utils.NewSet[Pos]()
	c.start = Pos{START_X, 0}
	c.xmin, c.xmax, c.ymax = START_X, START_X, 0

	for i := range s {
		l := strings.Split(s[i], " -> ")
		for i := 0; i < len(l)-1; i++ {
			lpos_s := utils.Map(strings.Split(l[i], ","), func(s string) int {
				if x, e := strconv.Atoi(s); e != nil {
					panic(e)
				} else {
					return x
				}
			})
			lpos_e := utils.Map(strings.Split(l[i+1], ","), func(s string) int {
				if x, e := strconv.Atoi(s); e != nil {
					panic(e)
				} else {
					return x
				}
			})
			c.update_size(lpos_s[0], lpos_s[1])
			c.update_size(lpos_e[0], lpos_e[1])
			xstart, xend := utils.MinMax(lpos_s[0], lpos_e[0])
			ystart, yend := utils.MinMax(lpos_s[1], lpos_e[1])
			for x := xstart; x <= xend; x++ {
				for y := ystart; y <= yend; y++ {
					c.rocks.Add(Pos{x, y})
				}
			}
		}
	}
	return
}

func (c *Cave) update_size(x, y int) {
	// Pour simplifier on laisse toujours une colonne sur les coté et une ligne en plus en bas.
	if x >= c.xmax {
		c.xmax = x + 1
	}
	if x <= c.xmin {
		c.xmin = x - 1
	}
	if y >= c.ymax {
		c.ymax = y + 1
	}
}

func (c *Cave) print2d() {
	for y := 0; y <= c.ymax; y++ {
		line := utils.MakeSlice[byte](c.xmax-c.xmin+1, AIR)
		for i, x := 0, c.xmin; x < c.xmax; i, x = i+1, x+1 {
			p := Pos{x, y}
			if p == c.start {
				line[i] = START_POINT
			} else if c.rocks.Contains(p) {
				line[i] = ROCK
			} else if c.sand.Contains(p) {
				line[i] = SAND
			}
		}
		fmt.Println(string(line))
	}
}

func (c *Cave) fall_sand(endless_void bool) bool {
	x, y, rest := c.start.x, c.start.y, false
	for ; y < c.ymax && !rest; y++ {
		rest = true
		for _, n_pos := range []int{x, x - 1, x + 1} {
			p := Pos{n_pos, y + 1}
			if !c.rocks.Contains(p) && !c.sand.Contains(p) {
				x = n_pos
				rest = false
				break
			}
		}
	}

	if !endless_void && c.start.x == x && c.start.y == y-1 {
		rest = false // Sand is blocked
	} else if rest {
		c.sand.Add(Pos{x, y - 1})
	} else if !endless_void {
		c.sand.Add(Pos{x, y})
		c.update_size(x, c.ymax-1)
		rest = true
	}
	return rest
}

func main() {
	s := utils.ReadFileLines("input")
	detect := parse_input(s)

	i := 0
	for detect.fall_sand(true) {
		i++
	}
	// detect.print2d()
	fmt.Println("Part 1: ", i)

	for detect.fall_sand(false) {
		i++
	}
	// detect.print2d()
	fmt.Println("Part 2: ", i+1)
}
