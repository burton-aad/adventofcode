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
		s.falling = Piece{}
		return false
	}
}

func (s *Screen) draw() {
	fall := utils.NewSet[Pos]()
	if len(s.falling.shape) > 0 {
		for _, p := range s.falling.shape {
			fall.Add(Pos{s.falling.xmin + p.x, s.falling.y + p.y})
		}
	}

	for y := s.height + s.falling.height + 2; y >= 0; y-- {
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
		fmt.Println(string(l))
	}
	l := utils.MakeSlice[byte](s.width+2, '-')
	l[0], l[len(l)-1] = '+', '+'
	fmt.Println(string(l))
}

const SCREEN_WIDTH = 7

var ROCKS_ORDER = []PieceType{HorzLine, Plus, InvertL, VertLine, Square}

func main() {
	s := utils.ReadFileLines("test")
	scr := MakeScreen(SCREEN_WIDTH, []PieceMove(s[0]))

	limit := 2_022
	for p_cnt := 0; p_cnt < limit; p_cnt++ {
		scr.falling = make_piece(ROCKS_ORDER[p_cnt%len(ROCKS_ORDER)], scr.height+3)
		for scr.fall() {
		}
	}
	fmt.Println("Part 1: ", scr.height)
}
