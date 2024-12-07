package main

import (
	"aoc/utils"
	"fmt"
	"math"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

type Instruction int

func parse_input(s []string) (r []string, ins []Instruction) {
	m := 0
	for _, l := range s {
		if l == "" {
			break
		}
		m = utils.Max(m, len(l))
		r = append(r, l)
	}
	for i := range r {
		r[i] = r[i] + strings.Repeat(" ", m-len(r[i]))
	}

	l := s[len(s)-1]
	for i := 0; i < len(l); i++ {
		inst, s := utils.LazyAtoi(l[i:])
		i += s
		ins = append(ins, Instruction(inst))
		if i < len(l) {
			ins = append(ins, Instruction(l[i]))
		}
	}
	return
}

type Pos struct {
	x, y int
}

type Facing int

const (
	Right Facing = iota
	Down
	Left
	Up
	Facing_count
)

type Line struct {
	l          []byte
	start, end int
}

type Column struct {
	start, end int
}

type Square struct {
	ul, dr Pos // up-left, down-right
}

type Cube struct {
	m       map[string]Square
	voisins map[string][]string
}

func (c Cube) square_of(p Pos) string {
	for k, sq := range c.m {
		if p.x >= sq.ul.x && p.x <= sq.dr.x && p.y >= sq.ul.y && p.y <= sq.dr.y {
			return k
		}
	}
	return ""
}

type Device struct {
	board     []Line
	columns   []Column
	start_pos Pos
	cube_size int
	cube      Cube
}

type Path struct {
	pos    Pos
	facing Facing
}

func NewDevive(board []string) (d Device) {
	for _, l := range board {
		li := Line{[]byte(l), 0, 0}
		li.start = utils.Min(
			(utils.IndexOf(li.l, '.')+len(l))%len(l),
			(utils.IndexOf(li.l, '#')+len(l))%len(l))
		li.end = utils.Max(utils.LastIndexOf(li.l, '.'), utils.LastIndexOf(li.l, '#'))
		d.board = append(d.board, li)
	}
	d.start_pos = Pos{d.board[0].start, 0}

	d.columns = make([]Column, len(board[0]))
	for i := range d.columns {
		m := utils.Map(d.board, func(li Line) bool { return li.l[i] == '.' || li.l[i] == '#' })
		d.columns[i] = Column{utils.IndexOf(m, true), utils.LastIndexOf(m, true)}
	}

	d.cube_size = int(math.Sqrt(float64(utils.Sum(utils.Map(d.board, func(l Line) int { return l.end - l.start + 1 })) / 6)))
	d.cube.m = make(map[string]Square)
	d.cube.voisins = make(map[string][]string)

	p := d.start_pos

	add_square := func(ul, dr Pos) string {
		name := string('A' + len(d.cube.m))
		d.cube.m[name] = Square{ul, dr}
		d.cube.voisins[name] = utils.MakeSlice(int(Facing_count), "")
		return name
	}

	// Search cube faces
	q := utils.Queue[string]{}
	q.Push(add_square(p, Pos{p.x + d.cube_size - 1, p.y + d.cube_size - 1}))
	for !q.Empty() {
		sq_name := q.Pop()
		sq := d.cube.m[sq_name]
		// Right
		if p := (Pos{sq.dr.x + 1, sq.ul.y}); d.valid_tile(p) && d.cube.square_of(p) == "" {
			new_sq := add_square(p, Pos{p.x + d.cube_size - 1, p.y + d.cube_size - 1})
			q.Push(new_sq)
			d.cube.voisins[sq_name][Right], d.cube.voisins[new_sq][Left] = new_sq, sq_name
		}
		// Down
		if p := (Pos{sq.ul.x, sq.dr.y + 1}); d.valid_tile(p) && d.cube.square_of(p) == "" {
			new_sq := add_square(p, Pos{p.x + d.cube_size - 1, p.y + d.cube_size - 1})
			q.Push(new_sq)
			d.cube.voisins[sq_name][Down], d.cube.voisins[new_sq][Up] = new_sq, sq_name
		}
		// Left
		if p := (Pos{sq.ul.x - 1, sq.dr.y}); d.valid_tile(p) && d.cube.square_of(p) == "" {
			new_sq := add_square(Pos{p.x - d.cube_size + 1, p.y - d.cube_size + 1}, p)
			q.Push(new_sq)
			d.cube.voisins[sq_name][Left], d.cube.voisins[new_sq][Right] = new_sq, sq_name
		}
		// Up
		if p := (Pos{sq.dr.x, sq.ul.y - 1}); d.valid_tile(p) && d.cube.square_of(p) == "" {
			new_sq := add_square(Pos{p.x - d.cube_size + 1, p.y - d.cube_size + 1}, p)
			q.Push(new_sq)
			d.cube.voisins[sq_name][Up], d.cube.voisins[new_sq][Down] = new_sq, sq_name
		}
	}

	// Search all faces neighbor
	for k := range d.cube.m {
		q.Push(k)
	}
	for !q.Empty() {
		sq := q.Pop()
		full, v := true, d.cube.voisins[sq]
		for _, dir := range []Facing{Right, Left, Down, Up} {
			if v[dir] != "" {
				continue
			}

			l := (dir + 1) % Facing_count
			if v[l] == "" {
				full = false
				continue
			}

			vvl := d.cube.voisins[v[l]]
			r_dir := (utils.IndexOf(vvl, sq) + 1) % int(Facing_count)
			if vvl[r_dir] != "" {
				v[dir] = vvl[r_dir]
				r := (utils.IndexOf(d.cube.voisins[v[dir]], v[l]) + 1) % int(Facing_count)
				d.cube.voisins[v[dir]][r] = sq
			} else {
				full = false
			}
		}

		if !full {
			q.Push(sq)
		}
	}

	return
}

func (d Device) valid_tile(p Pos) bool {
	return p.y >= 0 && p.y < len(d.board) && p.x >= d.board[p.y].start && p.x <= d.board[p.y].end
}

func (d Device) tile(p Pos) byte {
	return d.board[p.y].l[p.x]
}

func (d Device) set_tile(p Pos, b byte) {
	d.board[p.y].l[p.x] = b
}

func (d Device) start() Path {
	return Path{d.start_pos, Right}
}

func (d Device) draw() {
	for _, l := range d.board {
		fmt.Println(string(l.l))
	}
}

type MoveFunc func(p Path, v int, draw bool) Path

func (d Device) move_map(p Path, v int, draw bool) Path {
	_draw := func(p Pos, b byte) {}
	if draw {
		_draw = d.set_tile
	}

	np, l, c := p.pos, d.board[p.pos.y], d.columns[p.pos.x]
	for i := 0; i < v; i++ {
		x, y := np.x, np.y
		switch p.facing {
		case Right:
			_draw(np, '>')
			if np.x == l.end {
				x = l.start
			} else {
				x = np.x + 1
			}
		case Left:
			_draw(np, '<')
			if np.x == l.start {
				x = l.end
			} else {
				x = np.x - 1
			}
		case Up:
			_draw(np, '^')
			if np.y == c.start {
				y = c.end
			} else {
				y = np.y - 1
			}
		case Down:
			_draw(np, 'v')
			if np.y == c.end {
				y = c.start
			} else {
				y = np.y + 1
			}
		}

		if d.tile(Pos{x, y}) == '#' {
			break
		}
		np = Pos{x, y}
	}

	return Path{np, p.facing}
}

func (d Device) move_cube(p Path, v int, draw bool) Path {
	_draw := func(p Pos, b byte) {}
	if draw {
		_draw = d.set_tile
	}

	for i := 0; i < v; i++ {
		np, l, c := p, d.board[p.pos.y], d.columns[p.pos.x]
		switch p.facing {
		case Right:
			_draw(np.pos, '>')
			if np.pos.x == l.end {
				sq := d.cube.square_of(np.pos)
				v := d.cube.voisins[sq][Right]
				switch {
				case d.cube.voisins[v][Right] == sq:
					np = Path{Pos{d.cube.m[v].dr.x, d.cube.m[v].dr.y - (np.pos.y - d.cube.m[sq].ul.y)}, Left}
				case d.cube.voisins[v][Down] == sq:
					np = Path{Pos{d.cube.m[v].ul.x + (np.pos.y - d.cube.m[sq].ul.y), d.cube.m[v].dr.y}, Up}
				case d.cube.voisins[v][Left] == sq:
					np = Path{Pos{d.cube.m[v].ul.x, d.cube.m[v].ul.y + (np.pos.y - d.cube.m[sq].ul.y)}, Right}
				case d.cube.voisins[v][Up] == sq:
					np = Path{Pos{d.cube.m[v].dr.x - (np.pos.y - d.cube.m[sq].ul.y), d.cube.m[v].ul.y}, Down}
				}
			} else {
				np.pos.x = np.pos.x + 1
			}
		case Left:
			_draw(np.pos, '<')
			if np.pos.x == l.start {
				sq := d.cube.square_of(np.pos)
				v := d.cube.voisins[sq][Left]
				switch {
				case d.cube.voisins[v][Right] == sq:
					np = Path{Pos{d.cube.m[v].dr.x, d.cube.m[v].ul.y + (np.pos.y - d.cube.m[sq].ul.y)}, Left}
				case d.cube.voisins[v][Down] == sq:
					np = Path{Pos{d.cube.m[v].dr.x - (np.pos.y - d.cube.m[sq].ul.y), d.cube.m[v].dr.y}, Up}
				case d.cube.voisins[v][Left] == sq:
					np = Path{Pos{d.cube.m[v].ul.x, d.cube.m[v].dr.y - (np.pos.y - d.cube.m[sq].ul.y)}, Right}
				case d.cube.voisins[v][Up] == sq:
					np = Path{Pos{d.cube.m[v].ul.x + (np.pos.y - d.cube.m[sq].ul.y), d.cube.m[v].ul.y}, Down}
				}
			} else {
				np.pos.x = np.pos.x - 1
			}
		case Up:
			_draw(np.pos, '^')
			if np.pos.y == c.start {
				sq := d.cube.square_of(np.pos)
				v := d.cube.voisins[sq][Up]
				switch {
				case d.cube.voisins[v][Right] == sq:
					np = Path{Pos{d.cube.m[v].dr.x, d.cube.m[v].dr.y - (np.pos.x - d.cube.m[sq].ul.x)}, Left}
				case d.cube.voisins[v][Down] == sq:
					np = Path{Pos{d.cube.m[v].ul.x + (np.pos.x - d.cube.m[sq].ul.x), d.cube.m[v].dr.y}, Up}
				case d.cube.voisins[v][Left] == sq:
					np = Path{Pos{d.cube.m[v].ul.x, d.cube.m[v].ul.y + (np.pos.x - d.cube.m[sq].ul.x)}, Right}
				case d.cube.voisins[v][Up] == sq:
					np = Path{Pos{d.cube.m[v].dr.x - (np.pos.x - d.cube.m[sq].ul.x), d.cube.m[v].dr.y}, Down}
				}
			} else {
				np.pos.y = np.pos.y - 1
			}
		case Down:
			_draw(np.pos, 'v')
			if np.pos.y == c.end {
				sq := d.cube.square_of(np.pos)
				v := d.cube.voisins[sq][Down]
				switch {
				case d.cube.voisins[v][Right] == sq:
					np = Path{Pos{d.cube.m[v].dr.x, d.cube.m[v].ul.y + (np.pos.x - d.cube.m[sq].ul.x)}, Left}
				case d.cube.voisins[v][Down] == sq:
					np = Path{Pos{d.cube.m[v].dr.x - (np.pos.x - d.cube.m[sq].ul.x), d.cube.m[v].dr.y}, Up}
				case d.cube.voisins[v][Left] == sq:
					np = Path{Pos{d.cube.m[v].ul.x, d.cube.m[v].dr.y - (np.pos.x - d.cube.m[sq].ul.x)}, Right}
				case d.cube.voisins[v][Up] == sq:
					np = Path{Pos{d.cube.m[v].ul.x + (np.pos.x - d.cube.m[sq].ul.x), d.cube.m[v].ul.y}, Down}
				}
			} else {
				np.pos.y = np.pos.y + 1
			}
		}

		if d.tile(np.pos) == '#' {
			break
		}
		p = np
	}

	return p
}

func (d Device) run_path(inst []Instruction, move MoveFunc) Path {
	p := d.start()
	// fmt.Println("start ", p)
	for i, ins := range inst {
		if i%2 == 0 {
			p = move(p, int(ins), true)
			// fmt.Println("------------------------------")
			// fmt.Println(p)
			// d.draw()
		} else {
			// turn
			switch ins {
			case 'L':
				p.facing = (p.facing - 1 + 4) % 4
			case 'R':
				p.facing = (p.facing + 1) % 4
			}
		}
	}
	return p
}

func main() {
	_, filename, _, _ := runtime.Caller(0)
	os.Chdir(filepath.Dir(filename))
	board, path := parse_input(utils.ReadFileLines("input"))
	dev := NewDevive(board)

	p := dev.run_path(path, dev.move_map)
	fmt.Println("Part 1: ", (p.pos.y+1)*1000+(p.pos.x+1)*4+int(p.facing))

	p = dev.run_path(path, dev.move_cube)
	fmt.Println("Part 2: ", (p.pos.y+1)*1000+(p.pos.x+1)*4+int(p.facing))
}
