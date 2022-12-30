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
	Right = '>'
	Left  = '<'
	Up    = '^'
	Down  = 'v'
)

type Blizzard struct {
	x, y int
	dir  Direction
}

func parse_input(s []string) Valley {
	height := len(s) - 2
	width := len(s[0]) - 2
	var bliz []Blizzard
	for i := 0; i < height; i++ {
		for j := 0; j < width; j++ {
			switch s[i+1][j+1] {
			case '<':
				bliz = append(bliz, Blizzard{j, i, Left})
			case '>':
				bliz = append(bliz, Blizzard{j, i, Right})
			case 'v':
				bliz = append(bliz, Blizzard{j, i, Down})
			case '^':
				bliz = append(bliz, Blizzard{j, i, Up})
			case '.':
			}
		}
	}
	return NewValley(bliz, width, height)
}

type Valley struct {
	bliz          []Blizzard
	b_pos         *utils.Set[Pos]
	width, height int
}

func NewValley(bliz []Blizzard, w, h int) (v Valley) {
	v.width, v.height, v.bliz = w, h, bliz
	v.b_pos = utils.NewSet[Pos]()
	for _, b := range v.bliz {
		v.b_pos.Add(Pos{b.x, b.y})
	}
	return
}

func (v Valley) repr(opt_pos ...Pos) (r []string) {
	var s [][]rune
	for i := 0; i < v.height; i++ {
		s = append(s, utils.MakeSlice(v.width, '.'))
	}

	for _, b := range v.bliz {
		switch s[b.y][b.x] {
		case '.':
			s[b.y][b.x] = rune(b.dir)
		case Right, Left, Up, Down:
			s[b.y][b.x] = '2'
		default:
			s[b.y][b.x] = s[b.y][b.x] + 1
		}
	}

	if len(opt_pos) > 0 {
		s[opt_pos[0].y][opt_pos[0].x] = 'E'
	}

	for i := 0; i < v.height; i++ {
		r = append(r, string(s[i]))
	}
	return
}

func (v Valley) str(opt_pos ...Pos) (r string) {
	repr := v.repr(opt_pos...)
	r += "#." + string(utils.MakeSlice(v.width, '#')) + "\n"
	for i := 0; i < v.height; i++ {
		r += "#" + repr[i] + "#\n"
	}
	r += string(utils.MakeSlice(v.width, '#')) + ".#"
	return
}

func (v Valley) move_bliz() Valley {
	bliz := make([]Blizzard, len(v.bliz))
	for i := range v.bliz {
		switch v.bliz[i].dir {
		case Right:
			bliz[i] = Blizzard{(v.bliz[i].x + 1) % v.width, v.bliz[i].y, Right}
		case Left:
			bliz[i] = Blizzard{(v.bliz[i].x - 1 + v.width) % v.width, v.bliz[i].y, Left}
		case Up:
			bliz[i] = Blizzard{v.bliz[i].x, (v.bliz[i].y - 1 + v.height) % v.height, Up}
		case Down:
			bliz[i] = Blizzard{v.bliz[i].x, (v.bliz[i].y + 1) % v.height, Down}
		}
	}
	return NewValley(bliz, v.width, v.height)
}

type Expedition struct {
	path string
	pos  Pos
}

func neighbors(p Pos) map[Direction]Pos {
	r := make(map[Direction]Pos)
	r[Right] = Pos{p.x + 1, p.y}
	r[Left] = Pos{p.x - 1, p.y}
	r[Down] = Pos{p.x, p.y + 1}
	r[Up] = Pos{p.x, p.y - 1}
	return r
}

func bfs(vs []Valley, start Pos, target Pos, start_time int) Expedition {
	q := utils.Queue[Expedition]{}
	q.Push(Expedition{"", start})
	visited := utils.NewSet[string]()

	h := func(p Pos, time int) string {
		return fmt.Sprintf("%d-%d+%d", p.x, p.y, time)
	}

	for !q.Empty() {
		e := q.Pop()
		time := (start_time + len(e.path) + 1) % len(vs)
		v := vs[time]

		if e.pos == target {
			return e
		}

		for d, p := range neighbors(e.pos) {
			if !visited.Contains(h(p, time)) && !v.b_pos.Contains(p) && p.x >= 0 && p.x < v.width && p.y >= 0 && p.y < v.height {
				// valid move
				visited.Add(h(p, time))
				q.Push(Expedition{e.path + string(d), p})
			}
		}
		// wait
		if !visited.Contains(h(e.pos, time)) && !v.b_pos.Contains(e.pos) {
			visited.Add(h(e.pos, time))
			q.Push(Expedition{e.path + ".", e.pos})
		}
	}

	return Expedition{}
}

func main() {
	s := utils.ReadFileLines("input")
	v := parse_input(s)
	// fmt.Println(v.str())
	// fmt.Println("valley : ", v.width, v.height)

	valleys := make([]Valley, utils.LCM(v.width, v.height))
	valleys[0] = v
	for i := 1; i < len(valleys); i++ {
		valleys[i] = valleys[i-1].move_bliz()
	}

	r1 := bfs(valleys, Pos{0, -1}, Pos{v.width - 1, v.height - 1}, 0)
	fmt.Println("Part 1: ", len(r1.path)+1)

	r2 := bfs(valleys, Pos{v.width - 1, v.height}, Pos{0, 0}, len(r1.path)+1)
	r3 := bfs(valleys, Pos{0, -1}, Pos{v.width - 1, v.height - 1}, len(r1.path)+1+len(r2.path)+1)
	fmt.Println("Part 2: ", len(r1.path)+1+len(r2.path)+1+len(r3.path)+1)
}
