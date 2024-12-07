package main

import (
	"aoc/utils"
	"fmt"
	"sort"
)

type PacketType int

const (
	UNKNOWN PacketType = iota
	INTEGER
	LIST
)

type Packet struct {
	pType PacketType
	i     int
	l     []Packet
}

type PacketOrder int

const (
	RIGHT_ORDER     PacketOrder = -1
	SAME_ORDER                  = 0
	NOT_RIGHT_ORDER             = 1
)

func recParse(desc string, i int) (p Packet, r int) {
	if desc[i] != '[' {
		p.pType = INTEGER
		for r = i; r < len(desc) && desc[r] >= '0' && desc[r] <= '9'; r++ {
			p.i = p.i*10 + int(desc[r]-'0')
		}
		return
	}

	p.pType = LIST
	for r = i + 1; desc[r] != ']'; {
		p.l = append(p.l, Packet{})
		p.l[len(p.l)-1], r = recParse(desc, r)
		if desc[r] == ',' {
			r++
		}
	}
	r++ // remove ']'
	return
}

func NewPacket(desc string) Packet {
	p, _ := recParse(desc, 0)
	return p
}

func compare(left Packet, right Packet) PacketOrder {
	if left.pType != right.pType {
		if left.pType == INTEGER {
			return compare(Packet{LIST, 0, []Packet{left}}, right)
		} else {
			return compare(left, Packet{LIST, 0, []Packet{right}})
		}
	}

	if left.pType == INTEGER {
		if left.i < right.i {
			return RIGHT_ORDER
		} else if left.i > right.i {
			return NOT_RIGHT_ORDER
		} else {
			return SAME_ORDER
		}
	}

	for i := range left.l {
		if i >= len(right.l) {
			return NOT_RIGHT_ORDER
		} else {
			c := compare(left.l[i], right.l[i])
			if c != SAME_ORDER {
				return c
			}
		}
	}

	if len(right.l) > len(left.l) {
		return RIGHT_ORDER
	} else {
		return SAME_ORDER
	}
}

func (p *Packet) Str() (s string) {
	switch p.pType {
	case INTEGER:
		s = fmt.Sprintf("%d", p.i)
	case LIST:
		s = "["
		for i := range p.l {
			s += p.l[i].Str()
			if i+1 < len(p.l) {
				s += ","
			}
		}
		s += "]"
	default:
		panic("Invalid type")
	}
	return
}

type DistressSignal []Packet

func (a DistressSignal) Len() int           { return len(a) }
func (a DistressSignal) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a DistressSignal) Less(i, j int) bool { return compare(a[i], a[j]) == RIGHT_ORDER }

func main() {
	s := utils.ReadFileLines("input")
	packets := DistressSignal{}
	for _, l := range s {
		if l != "" {
			packets = append(packets, NewPacket(l))
		}
	}

	part1 := 0
	for i := 0; i < len(packets); i += 2 {
		if compare(packets[i], packets[i+1]) == RIGHT_ORDER {
			part1 += i/2 + 1
		}
	}
	fmt.Println("Part 1: ", part1)

	// Add divider packets
	dividers := []Packet{NewPacket("[[2]]"), NewPacket("[[6]]")}
	packets = append(packets, dividers...)

	sort.Sort(packets)
	fmt.Println("Part 2: ", utils.Prod(utils.Map(dividers, func(target Packet) int {
		i, _ := sort.Find(len(packets), func(i int) int { return int(compare(target, packets[i])) })
		return i + 1
	})))
}
