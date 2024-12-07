package main

import (
	"aoc/utils"
	"fmt"
	"strconv"
)

type Pos struct {
	x int
	y int
}

func (p *Pos) move(dx int, dy int) {
	p.x += dx
	p.y += dy
}

func (p *Pos) dist(o *Pos) Pos {
	return Pos{p.x - o.x, p.y - o.y}
}

type Direction byte

const (
	Right Direction = 'R'
	Left            = 'L'
	Up              = 'U'
	Down            = 'D'
)

type Rope struct {
	head  Pos
	tails []Pos
}

func (r *Rope) last_tail() Pos {
	return r.tails[len(r.tails)-1]
}

func (r *Rope) move(d Direction) {
	switch d {
	case Right:
		r.head.move(1, 0)
	case Left:
		r.head.move(-1, 0)
	case Up:
		r.head.move(0, 1)
	case Down:
		r.head.move(0, -1)
	}

	// update tails
	prev_knot := &r.head
	for i := range r.tails {
		t := &r.tails[i]
		d := prev_knot.dist(t)
		if utils.Abs(d.x) > 1 || utils.Abs(d.y) > 1 {
			if d.x != 0 {
				t.x += utils.Sign(d.x)
			}
			if d.y != 0 {
				t.y += utils.Sign(d.y)
			}
		}
		prev_knot = t
	}
}

func NewRope(tail_nb int) Rope {
	return Rope{Pos{}, make([]Pos, tail_nb)}
}

func main() {
	s := utils.ReadFileLines("input")
	r := NewRope(9)
	t1_pos, t2_pos := utils.NewSet[Pos](), utils.NewSet[Pos]()

	for _, l := range s {
		d := Direction(l[0])
		i, _ := strconv.Atoi(l[2:])
		for ; i > 0; i-- {
			r.move(d)
			t1_pos.Add(r.tails[0])
			t2_pos.Add(r.last_tail())
		}
	}

	fmt.Println("Part 1: ", t1_pos.Len())
	fmt.Println("Part 2: ", t2_pos.Len())
}
