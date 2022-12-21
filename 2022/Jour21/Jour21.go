package main

import (
	"aoc/utils"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

type Monkey interface {
	name() string
	str() string
	calc() int
	op(Monkey, Monkey) int
}

type Mnode struct {
	nam    string
	ma, mb Monkey
	oper   byte
}

func (m Mnode) name() string {
	return m.nam
}

func (m Mnode) str() string {
	return fmt.Sprintf("(%c %s %s)", m.oper, m.ma.str(), m.mb.str())
}

func (m Mnode) calc() int {
	return m.op(m.ma, m.mb)
}

func (m Mnode) op(ma Monkey, mb Monkey) int {
	switch m.oper {
	case '+':
		return ma.calc() + mb.calc()
	case '-':
		return ma.calc() - mb.calc()
	case '*':
		return ma.calc() * mb.calc()
	case '/':
		return ma.calc() / mb.calc()
	}
	panic("Invalid op")
}

type Mleaf struct {
	nam   string
	value int
}

func (m Mleaf) name() string {
	return m.nam
}

func (m Mleaf) str() string {
	return fmt.Sprintf("%d", m.value)
}

func (m Mleaf) calc() int {
	return m.value
}

func (m Mleaf) op(ma Monkey, mb Monkey) int {
	return m.value
}

func parse_input(s []string) map[string]Monkey {
	mks := make(map[string]Monkey)
	op_re := regexp.MustCompile(`(\w+): (\w+) ([-+*/]) (\w+)`)

	qn := utils.Queue[[]string]{}

	for _, l := range s {
		if a := op_re.FindStringSubmatch(l); a != nil {
			qn.Push(a)
		} else {
			a := strings.Split(l, ": ")
			i, _ := strconv.Atoi(a[1])
			mks[a[0]] = Mleaf{a[0], i}
		}
	}

	for !qn.Empty() {
		a := qn.Pop()
		name := a[1]
		ma, ok1 := mks[a[2]]
		mb, ok2 := mks[a[4]]
		if ok1 && ok2 {
			mks[name] = Mnode{name, ma, mb, a[3][0]}
		} else {
			qn.Push(a)
		}
	}

	return mks
}

func dfs(root Monkey, tgt string) ([]string, bool) {
	switch m := root.(type) {
	case Mnode:
		if s, ok := dfs(m.ma, tgt); ok {
			return append([]string{m.name()}, s...), true
		} else if s, ok := dfs(m.mb, tgt); ok {
			return append([]string{m.name()}, s...), true
		}
	case Mleaf:
		if m.name() == tgt {
			return []string{m.name()}, true
		}
	}

	return []string{}, false
}

func main() {
	s := utils.ReadFileLines("input")
	monkeys := parse_input(s)

	root := monkeys["root"]
	fmt.Println("Part 1: ", root.calc())

	path, _ := dfs(root, "humn")
	var target int
	if root.(Mnode).ma.name() == path[1] {
		target = root.(Mnode).mb.calc()
	} else {
		target = root.(Mnode).ma.calc()
	}

	for i := 1; i < len(path)-1; i++ {
		node := monkeys[path[i]].(Mnode)
		if node.ma.name() == path[i+1] {
			switch node.oper {
			case '+':
				target = target - node.mb.calc()
			case '-':
				target = target + node.mb.calc()
			case '*':
				target = target / node.mb.calc()
			case '/':
				target = target * node.mb.calc()
			}
		} else {
			switch node.oper {
			case '+':
				target = target - node.ma.calc()
			case '-':
				target = node.ma.calc() - target
			case '*':
				target = target / node.ma.calc()
			case '/':
				target = node.ma.calc() / target
			}
		}
	}
	fmt.Println("Part 2: ", target)
}
