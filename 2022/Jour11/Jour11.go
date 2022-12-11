package main

import (
	"aoc/utils"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

type Monkey struct {
	items         []int
	op            func(int) int // Operation for the worry level
	factor_test   int
	monkey_true   int
	monkey_false  int
	inspect_count int
}

func NewMonkey(s []string, idx int) (mk Monkey, i int) {
	i = idx
	var ns string

	check_line_prefix := func(prefix string) string {
		ns := strings.TrimSpace(s[i])
		if !strings.HasPrefix(ns, prefix) {
			panic("Invalid input : " + ns)
		}
		i++
		return strings.TrimSpace(strings.TrimPrefix(ns, prefix))
	}

	check_line_prefix("Monkey")

	ns = check_line_prefix("Starting items:")
	its := strings.Split(ns, ", ")
	for _, it := range its {
		it_i, e := strconv.Atoi(it)
		if e != nil {
			panic(e)
		}
		mk.items = append(mk.items, it_i)

	}

	ns = check_line_prefix("Operation: new =")
	r := regexp.MustCompile(`(old|\d+) ([+*]) (old|\d+)`)
	oper := r.FindStringSubmatch(ns)
	var f2 func(int, int) int
	switch oper[2] {
	case "*":
		f2 = func(x, y int) int { return x * y }
	case "+":
		f2 = func(x, y int) int { return x + y }
	default:
		panic("Unhandle operand : " + oper[2])
	}
	if v, e := strconv.Atoi(oper[1]); e == nil {
		mk.op = func(x int) int { return f2(v, x) }
	} else if v, e := strconv.Atoi(oper[3]); e == nil {
		mk.op = func(x int) int { return f2(x, v) }
	} else {
		mk.op = func(x int) int { return f2(x, x) }
	}

	var e error
	ns = check_line_prefix("Test: divisible by")
	mk.factor_test, e = strconv.Atoi(ns)
	if e != nil {
		panic(e)
	}
	ns = check_line_prefix("If true: throw to monkey")
	mk.monkey_true, e = strconv.Atoi(ns)
	if e != nil {
		panic(e)
	}
	ns = check_line_prefix("If false: throw to monkey")
	mk.monkey_false, e = strconv.Atoi(ns)
	if e != nil {
		panic(e)
	}

	return
}

func make_monkeys(s []string) (monkeys []Monkey) {
	for i := 0; i < len(s); {
		if len(s[i]) == 0 {
			i++ // ignore empty line
			continue
		}

		var m Monkey
		m, i = NewMonkey(s, i)
		monkeys = append(monkeys, m)
	}
	return
}

func round(monkeys []Monkey, relief_level int) {
	m_keys := utils.Prod(utils.Map(monkeys, func(mk Monkey) int { return mk.factor_test }))
	for i := range monkeys {
		mk := &monkeys[i]
		for _, item := range mk.items {
			item = mk.op(item) / relief_level // calculate worry level
			var m_thrown *Monkey
			if item%mk.factor_test == 0 {
				m_thrown = &monkeys[mk.monkey_true]
			} else {
				m_thrown = &monkeys[mk.monkey_false]
			}
			// keep worry level manageable. I think it is working because all
			// factors are prime (so prime between themselves).
			m_thrown.items = append(m_thrown.items, item % (m_keys))
			mk.inspect_count++
		}
		mk.items = make([]int, 0)
	}
}

const (
	NB_ROUNDS_1    = 20
	NB_ROUNDS_2    = 10_000
	RELIEF_LEVEL_1 = 3
	RELIEF_LEVEL_2 = 1
)

func monkey_business(monkeys []Monkey) int {
	m1, m2 := 0, 0
	for _, mk := range monkeys {
		if mk.inspect_count > m1 {
			m1, m2 = mk.inspect_count, m1
		} else if mk.inspect_count > m2 {
			m2 = mk.inspect_count
		}
	}
	return m1 * m2
}

func main() {
	s := utils.ReadFileLines("input")

	monkeys := make_monkeys(s)
	for i := 0; i < NB_ROUNDS_1; i++ {
		round(monkeys, RELIEF_LEVEL_1)
	}
	fmt.Println("Part 1: ", monkey_business(monkeys))

	monkeys = make_monkeys(s)
	for i := 0; i < NB_ROUNDS_2; i++ {
		round(monkeys, RELIEF_LEVEL_2)
	}
	fmt.Println("Part 2: ", monkey_business(monkeys))
}
