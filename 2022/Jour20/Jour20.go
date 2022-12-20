package main

import (
	"aoc/utils"
	"fmt"
	"strconv"
)

type Element struct {
	value      int
	next, prev *Element
}

func (e *Element) next_elem(v int) *Element {
	n := e
	for i := 0; i > v; i, n = i-1, n.prev {
	}
	for i := 0; i < v; i, n = i+1, n.next {
	}
	return n
}

type Crypt struct {
	head  *Element
	order []*Element
}

func (c *Crypt) add_element(i int) {
	e := &Element{i, nil, nil}
	if len(c.order) == 0 {
		e.next, e.prev = e, e
	} else {
		e.next, e.prev = c.order[0], c.order[0].prev
		e.prev.next, e.next.prev = e, e
	}
	c.order = append(c.order, e)
	if i == 0 {
		c.head = e
	}
}

func (c *Crypt) str() (s string) {
	h := c.head
	for {
		s += fmt.Sprintf("%d", h.value)
		h = h.next
		if h == c.head {
			break
		}
		s += fmt.Sprintf(", ")
	}
	return
}

func (c *Crypt) mix_one(e *Element) {
	n := e.prev
	e.next.prev, e.prev.next = e.prev, e.next
	n = n.next_elem(e.value % (len(c.order) - 1))
	e.prev, e.next = n, n.next
	n.next, n.next.prev = e, e
}

func (c *Crypt) mix() {
	for _, e := range c.order {
		c.mix_one(e)
	}
}

func (c *Crypt) grove_coordinates() (i int) {
	e := c.head.next_elem(1000 % len(c.order))
	i += e.value
	e = c.head.next_elem(2000 % len(c.order))
	i += e.value
	e = c.head.next_elem(3000 % len(c.order))
	i += e.value
	return 
}

func NewCrypt() Crypt {
	return Crypt{nil, []*Element{}}
}

const DECRYPTION_KEY = 811_589_153

func main() {
	s := utils.ReadFileLines("input")

	crypt := NewCrypt()
	for _, l := range s {
		i, _ := strconv.Atoi(l)
		crypt.add_element(i)
	}

	crypt.mix()
	// fmt.Println(crypt.str())
	fmt.Println("Part 1: ", crypt.grove_coordinates())

	crypt = NewCrypt()
	for _, l := range s {
		i, _ := strconv.Atoi(l)
		crypt.add_element(i * DECRYPTION_KEY)
	}

	for i := 0; i < 10; i++ {
		crypt.mix()
	}
	// fmt.Println(crypt.str())
	fmt.Println("Part 2: ", crypt.grove_coordinates())
}
