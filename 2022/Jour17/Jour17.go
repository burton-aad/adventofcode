package main

import (
	"aoc/utils"
	"fmt"
)

type Pos struct {
	x, y int
}

type Piece struct {
	xmin, xmax, y int
	height        int
	shape         []Pos
}

type PieceType int

const (
	HorzLine PieceType = iota
	VertLine
	InvertL
	Plus
	Square
)

type PieceMove byte

const (
	MoveLeft  PieceMove = '<'
	MoveRight           = '>'
)

func make_piece(typ PieceType, pos int) Piece {
	switch typ {
	case HorzLine:
		return Piece{2, 5, pos, 1, []Pos{{0, 0}, {1, 0}, {2, 0}, {3, 0}}}
	case VertLine:
		return Piece{2, 2, pos, 4, []Pos{{0, 0}, {0, 1}, {0, 2}, {0, 3}}}
	case InvertL:
		return Piece{2, 4, pos, 3, []Pos{{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}}}
	case Plus:
		return Piece{2, 4, pos, 3, []Pos{{1, 0}, {0, 1}, {1, 1}, {2, 1}, {1, 2}}}
	case Square:
		return Piece{2, 3, pos, 2, []Pos{{0, 0}, {1, 0}, {0, 1}, {1, 1}}}
	}
	panic("Invalid Piece type")
}

type Screen struct {
	width, height int
	falling       Piece
	rocks         utils.Set[Pos]
	jets          []PieceMove
	jetIdx        int
}

func MakeScreen(width int, jets []PieceMove) Screen {
	return Screen{width, 0, Piece{}, *utils.NewSet[Pos](), jets, 0}
}

func (s *Screen) fall() bool {
	if len(s.falling.shape) == 0 {
		return false
	}

	is_movable := func(pc Piece, m Pos) bool {
		for _, p := range pc.shape {
			np := Pos{pc.xmin + p.x + m.x, pc.y + p.y + m.y}
			if np.x < 0 || np.x >= s.width || np.y < 0 || s.rocks.Contains(np) {
				return false
			}
		}
		return true
	}

	// Move
	switch s.jets[s.jetIdx] {
	case MoveLeft:
		if is_movable(s.falling, Pos{-1, 0}) {
			s.falling.xmin--
			s.falling.xmax--
		}
	case MoveRight:
		if is_movable(s.falling, Pos{1, 0}) {
			s.falling.xmin++
			s.falling.xmax++
		}
	}
	s.jetIdx = (s.jetIdx + 1) % len(s.jets)

	// Fall
	if is_movable(s.falling, Pos{0, -1}) {
		s.falling.y--
		return true
	} else {
		for _, p := range s.falling.shape {
			s.height = utils.Max(s.height, s.falling.y+p.y+1)
			s.rocks.Add(Pos{s.falling.xmin + p.x, s.falling.y + p.y})
		}
		s.falling = Piece{} // reset falling piece
		return false
	}
}

func (s *Screen) floor_shape() (r string) {
	r += fmt.Sprintf("%d", s.jetIdx)
	fl, v := (1<<s.width)-1, 0
	for y := s.height - 1; y >= 0 && v != fl; y-- {
		l := s.draw_line(y, &utils.Set[Pos]{})
		r += l
		for i, c := range l[1 : len(l)-1] {
			if c == '#' {
				v = v | (1 << i)
			}
		}
	}
	return
}

func (s *Screen) draw_line(y int, fall *utils.Set[Pos]) string {
	l := utils.MakeSlice[byte](s.width+2, '.')
	l[0], l[len(l)-1] = '|', '|'
	for x := 0; x < s.width; x++ {
		p := Pos{x, y}
		if fall.Contains(p) {
			l[x+1] = '@'
		} else if s.rocks.Contains(p) {
			l[x+1] = '#'
		}
	}
	return string(l)
}

func (s *Screen) draw() {
	fall := utils.NewSet[Pos]()
	if len(s.falling.shape) > 0 {
		for _, p := range s.falling.shape {
			fall.Add(Pos{s.falling.xmin + p.x, s.falling.y + p.y})
		}
	}

	for y := s.height + s.falling.height + 2; y >= 0; y-- {
		fmt.Println(s.draw_line(y, fall))
	}
	l := utils.MakeSlice[byte](s.width+2, '-')
	l[0], l[len(l)-1] = '+', '+'
	fmt.Println(string(l))
}

const SCREEN_WIDTH = 7

var ROCKS_ORDER = []PieceType{HorzLine, Plus, InvertL, VertLine, Square}

type FloorRef struct {
	piece_count int
	height      int
}

func main() {
	s := utils.ReadFileLines("input")
	scr := MakeScreen(SCREEN_WIDTH, []PieceMove(s[0]))

	// Search loop
	loop_start, loop_size, loop_height := 0, 0, 0
	floors := make([]map[string]FloorRef, len(ROCKS_ORDER))
	for i := range floors {
		floors[i] = make(map[string]FloorRef)
	}
	for p_cnt := 0; ; p_cnt++ {
		pt := ROCKS_ORDER[p_cnt%len(ROCKS_ORDER)]
		fshape := scr.floor_shape()
		if v, ok := floors[pt][fshape]; ok {
			loop_start = p_cnt
			loop_size = p_cnt - v.piece_count
			loop_height = scr.height - v.height
			break
		} else {
			floors[pt][fshape] = FloorRef{p_cnt, scr.height}
		}
		scr.falling = make_piece(pt, scr.height+3)
		for scr.fall() {
		}
	}

	p1, p2 := 2_022, 1_000_000_000_000
	t1, t2 := p1-loop_start, p2-loop_start
	tl1, tl2 := t1/loop_size, t2/loop_size
	t1, t2 = t1-tl1*loop_size, t2-tl2*loop_size
	p_cnt := 0
	for ; p_cnt < utils.Max(t1, t2); p_cnt++ {
		if p_cnt == t1 {
			fmt.Println("Part 1: ", scr.height+tl1*loop_height)
		} else if p_cnt == t2 {
			fmt.Println("Part 2: ", scr.height+tl2*loop_height)
		}

		scr.falling = make_piece(ROCKS_ORDER[(p_cnt+loop_start)%len(ROCKS_ORDER)], scr.height+3)
		for scr.fall() {
		}
	}
	if p_cnt == t1 {
		fmt.Println("Part 1: ", scr.height+tl1*loop_height)
	} else if p_cnt == t2 {
		fmt.Println("Part 2: ", scr.height+tl2*loop_height)
	}
}
