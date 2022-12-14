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
)

type Cave struct {
	r                [][]byte
	xmin, xmax, ymax int
	start            int
}

func parse_input(s []string) (c Cave) {
	c.r = append(c.r, make([]byte, 1))
	c.r[0][0] = START_POINT
	c.xmin, c.xmax, c.ymax = 500, 500, 0

	update_size := func(x, y int) {
		// Pour simplifier on laisse toujours une colonne sur les coté et une ligne en plus en bas.
		if x >= c.xmax {
			nb := utils.MakeSlice[byte](x-c.xmax+1, AIR)
			for i := range c.r {
				c.r[i] = append(c.r[i], nb...)
			}
			c.xmax = x + 1
		}
		if x <= c.xmin {
			for i := range c.r {
				c.r[i] = append(utils.MakeSlice[byte](c.xmin-x+1, AIR), c.r[i]...)
			}
			c.xmin = x - 1
		}
		if y >= c.ymax {
			for ; c.ymax <= y; c.ymax++ {
				c.r = append(c.r, utils.MakeSlice[byte](len(c.r[0]), AIR))
			}
		}
	}

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
			update_size(lpos_s[0], lpos_s[1])
			update_size(lpos_e[0], lpos_e[1])
			xstart, xend := utils.MinMax(lpos_s[0], lpos_e[0])
			ystart, yend := utils.MinMax(lpos_s[1], lpos_e[1])
			for x := xstart; x <= xend; x++ {
				for y := ystart; y <= yend; y++ {
					c.r[y][x-c.xmin] = ROCK
				}
			}
		}
	}

	c.start = utils.IndexOf(c.r[0], START_POINT)
	return
}

func (c *Cave) print2d() {
	for i := range c.r {
		fmt.Println(string(c.r[i]))
	}
}

func (c *Cave) fall_sand() bool {
	x, y, rest := c.start, 0, false
	for ; y < len(c.r)-1 && !rest; y++ {
		rest = true
		for _, n_pos := range []int{x, x - 1, x + 1} {
			if c.r[y+1][n_pos] == AIR {
				x = n_pos
				rest = false
				break
			}
		}
	}
	if rest {
		c.r[y-1][x] = SAND
	}
	return rest
}

func main() {
	s := utils.ReadFileLines("test")
	detect := parse_input(s)
	// detect.print2d()

	i := 0
	for detect.fall_sand() {
		i++
	}
	detect.print2d()
	fmt.Println("Part 1: ", i)
}
