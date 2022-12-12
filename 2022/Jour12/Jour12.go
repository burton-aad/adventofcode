package main

import (
	"aoc/utils"
	"fmt"
)

type Pos struct {
	x, y int
}

type Direction rune

const (
	Right Direction = '>'
	Left            = '<'
	Up              = '^'
	Down            = 'v'
)

func neighbor4(p Pos, w, h int) map[Direction]Pos {
	r := make(map[Direction]Pos)
	if p.x > 0 {
		r[Left] = Pos{p.x - 1, p.y}
	}
	if p.y > 0 {
		r[Up] = Pos{p.x, p.y - 1}
	}
	if p.x < w-1 {
		r[Right] = Pos{p.x + 1, p.y}
	}
	if p.y < h-1 {
		r[Down] = Pos{p.x, p.y + 1}
	}
	return r
}

func search_se(s []string) (S Pos, E Pos) {
	for i := range s {
		for j, c := range s[i] {
			if c == 'S' {
				S = Pos{j, i}
			} else if c == 'E' {
				E = Pos{j, i}
			}
		}
	}
	return
}

func search_a(s []string) (r []Pos) {
	for i := range s {
		for j, c := range s[i] {
			if c == 'a' || c == 'S' {
				r = append(r, Pos{j, i})
			}
		}
	}
	return
}

type SPath struct {
	dirs string
}

func correct_elevation(elv byte) byte {
	if elv == 'S' {
		return 'a'
	} else if elv == 'E' {
		return 'z'
	} else {
		return elv
	}
}

func dijkstra(s []string, S ...Pos) [][]SPath {
	height := len(s)
	width := len(s[0])

	djk := make([][]SPath, height)
	for i := range djk {
		djk[i] = make([]SPath, width)
	}
	q := utils.Queue[Pos]{}

	for _, start := range S {
		djk[start.y][start.x].dirs = "S"
		q.Push(start)
	}

	for !q.Empty() {
		cur_pos := q.Pop()
		cur_path := djk[cur_pos.y][cur_pos.x]
		for d, n := range neighbor4(cur_pos, width, height) {
			cur_elv := correct_elevation(s[cur_pos.y][cur_pos.x])
			n_elv := correct_elevation(s[n.y][n.x])
			if cur_elv+1 < n_elv {
				continue
			}

			n_path := &djk[n.y][n.x]
			if n_path.dirs == "" || len(cur_path.dirs)+1 < len(n_path.dirs) {
				n_path.dirs = cur_path.dirs + string(d)
				q.Push(n)
			}
		}
	}

	return djk
}

func print_djk(djk [][]SPath) {
	for i := range djk {
		for j := range djk[i] {
			d := djk[i][j].dirs
			if len(d) > 0 {
				fmt.Printf("%c", d[len(d)-1])
			} else {
				fmt.Printf(".")
			}
		}
		fmt.Printf("\n")
	}
}

func main() {
	s := utils.ReadFileLines("input")

	S, E := search_se(s)
	djk := dijkstra(s, S)
	fmt.Println("Part 1: ", len(djk[E.y][E.x].dirs)-1) // -1 because of S

	a_pos := search_a(s)
	djk = dijkstra(s, a_pos...)
	fmt.Println("Part 2: ", len(djk[E.y][E.x].dirs)-1) // -1 because of S
}
