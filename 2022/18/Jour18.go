package main

import (
	"aoc/utils"
	"fmt"
	"strconv"
	"strings"
)

type Cube struct {
	x, y, z int
}

func CubeFromString(s string) (c Cube) {
	l := strings.Split(s, ",")
	var err error
	c.x, err = strconv.Atoi(l[0])
	if err != nil {
		panic(err)
	}
	c.y, err = strconv.Atoi(l[1])
	if err != nil {
		panic(err)
	}
	c.z, err = strconv.Atoi(l[2])
	if err != nil {
		panic(err)
	}
	return
}

func (c Cube) neighbors6() []Cube {
	r := make([]Cube, 0, 6)
	r = append(r, Cube{c.x + 1, c.y, c.z})
	r = append(r, Cube{c.x - 1, c.y, c.z})
	r = append(r, Cube{c.x, c.y + 1, c.z})
	r = append(r, Cube{c.x, c.y - 1, c.z})
	r = append(r, Cube{c.x, c.y, c.z + 1})
	r = append(r, Cube{c.x, c.y, c.z - 1})
	return r
}

func surface_area(cubes *utils.Set[Cube], out_air *utils.Set[Cube]) (surf_area int) {
	for p := range cubes.Iter() {
		for _, v := range p.neighbors6() {
			if !cubes.Contains(v) && (out_air.Len() == 0 || out_air.Contains(v)) {
				surf_area++
			}
		}
	}
	return
}

func main() {
	s := utils.ReadFileLines("input")
	cubes_l := utils.Map(s, CubeFromString)
	xmin, xmax := utils.AMinMax(utils.Map(cubes_l, func(c Cube) int { return c.x }))
	ymin, ymax := utils.AMinMax(utils.Map(cubes_l, func(c Cube) int { return c.y }))
	zmin, zmax := utils.AMinMax(utils.Map(cubes_l, func(c Cube) int { return c.z }))
	cubes := utils.SetFrom(cubes_l)

	fmt.Println("Part 1: ", surface_area(cubes, &utils.Set[Cube]{}))

	c := Cube{xmax + 1, ymax + 1, zmax + 1} // Must be extern
	out_air := utils.NewSet[Cube]()
	q := utils.Queue[Cube]{}
	q.Push(c)
	for !q.Empty() {
		next := q.Pop()
		if cubes.Contains(next) {
			continue
		}
		if !out_air.Contains(next) {
			out_air.Add(next)
			for _, v := range next.neighbors6() {
				if v.x >= xmin-1 && v.x <= xmax+1 && v.y >= ymin-1 && v.y <= ymax+1 && v.z >= zmin-1 && v.z <= zmax+1 {
					q.Push(v)
				}
			}
		}
	}
	fmt.Println("Part 2: ", surface_area(cubes, out_air))
}
