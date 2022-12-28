package main

import (
	"aoc/utils"
	"fmt"
)

type Pos struct {
	x, y int
}

type AdjacentPos int

const (
	N AdjacentPos = iota
	NE
	NW
	E
	W
	S
	SE
	SW
)

type Direction int

const (
	north Direction = iota
	south
	west
	east
)

func neighbor8(p Pos) []Pos {
	return []Pos{
		{p.x, p.y - 1},
		{p.x + 1, p.y - 1},
		{p.x - 1, p.y - 1},
		{p.x + 1, p.y},
		{p.x - 1, p.y},
		{p.x, p.y + 1},
		{p.x + 1, p.y + 1},
		{p.x - 1, p.y + 1},
	}
}

type Elves struct {
	s     utils.Set[Pos]
	looks []Direction
}

func (e Elves) minmax() (min Pos, max Pos) {
	it := e.s.Iter()
	val := <-it
	min, max = val, val

	for p := range it {
		if p.x < min.x {
			min.x = p.x
		}
		if p.y < min.y {
			min.y = p.y
		}
		if p.x > max.x {
			max.x = p.x
		}
		if p.y > max.y {
			max.y = p.y
		}
	}
	return
}

func (e Elves) draw() {
	min, max := e.minmax()
	lc := max.x - min.x + 1
	for y := min.y; y <= max.y; y++ {
		s := make([]byte, lc)
		for x := min.x; x <= max.x; x++ {
			if e.s.Contains(Pos{x, y}) {
				s[x-min.x] = '#'
			} else {
				s[x-min.x] = '.'
			}
		}
		fmt.Println(string(s))
	}
}

func (e Elves) proposal() (map[Pos][]Pos, int) {
	m := make(map[Pos][]Pos)
	move_cnt := 0
	for p := range e.s.Iter() {
		v := utils.Map(neighbor8(p), e.s.Contains)
		n := make([]Pos, 0, 4)
		for _, d := range e.looks {
			switch d {
			case north:
				if !v[N] && !v[NE] && !v[NW] {
					n = append(n, Pos{p.x, p.y - 1})
				}
			case south:
				if !v[S] && !v[SE] && !v[SW] {
					n = append(n, Pos{p.x, p.y + 1})
				}
			case west:
				if !v[W] && !v[NW] && !v[SW] {
					n = append(n, Pos{p.x - 1, p.y})
				}
			case east:
				if !v[E] && !v[NE] && !v[SE] {
					n = append(n, Pos{p.x + 1, p.y})
				}
			}
		}
		if len(n) > 0 && len(n) < 4 {
			m[n[0]] = append(m[n[0]], p)
			move_cnt++
		} else {
			m[p] = append(m[p], p)
		}
	}

	return m, move_cnt
}

func (e Elves) move(proposal map[Pos][]Pos) (r Elves) {
	r.looks = append(e.looks[1:], e.looks[0])
	r.s = *utils.NewSet[Pos]()
	for dest, src := range proposal {
		if len(src) == 1 {
			r.s.Add(dest)
		} else {
			r.s.Add(src...)
		}
	}
	return
}

func parse_input(s []string) (e Elves) {
	v := make([]Pos, 0)
	for i, l := range s {
		for j, c := range l {
			if c == '#' {
				v = append(v, Pos{j, i})
			}
		}
	}
	e.s = *utils.SetFrom(v)
	e.looks = []Direction{north, south, west, east}
	return e
}

func main() {
	s := utils.ReadFileLines("input")
	elves := parse_input(s)
	// elves.draw()

	round := 1
	for ; ; round++ {
		p, c := elves.proposal()
		elves = elves.move(p)
		// if round % 10 == 0 {
		// 	fmt.Println(round, ":", c)
		// }
		if c == 0 {
			break
		}
		if round == 10 {
			min, max := elves.minmax()
			fmt.Println("Part 1: ", (max.x-min.x+1)*(max.y-min.y+1)-elves.s.Len())
		}
	}

	fmt.Println("Part 2: ", round)
}
