package main

import (
	"aoc/utils"
	"fmt"
	"regexp"
	"strconv"
)

type Pos struct {
	x, y int
}

func manhattan_dist(a Pos, b Pos) int {
	return utils.Abs(a.x-b.x) + utils.Abs(a.y-b.y)
}

type Sensor struct {
	pos            Pos
	closest_beacon Pos
	dist           int
}

func parse_input(s []string) (sensors []Sensor) {
	re := regexp.MustCompile(`Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)`)
	for _, l := range s {
		a := re.FindStringSubmatch(l)
		x, _ := strconv.Atoi(a[1])
		y, _ := strconv.Atoi(a[2])
		bx, _ := strconv.Atoi(a[3])
		by, _ := strconv.Atoi(a[4])
		sensors = append(sensors, Sensor{Pos{x, y}, Pos{bx, by}, manhattan_dist(Pos{x, y}, Pos{bx, by})})
	}
	return
}

const TUN_FACTOR = 4_000_000

func empty_in_line(sensors []Sensor, y int) (count int) {
	xmin := utils.AMin(utils.Map(sensors, func(s Sensor) int { return s.pos.x - s.dist }))
	xmax := utils.AMax(utils.Map(sensors, func(s Sensor) int { return s.pos.x + s.dist }))
	for x := xmin; x < xmax; x++ {
		for _, s := range sensors {
			if manhattan_dist(Pos{x, y}, s.pos) <= s.dist {
				if s.closest_beacon.x != x || s.closest_beacon.y != y {
					count++
				}
				break
			}
		}
	}
	return
}

func free_in_range(sensors []Sensor, xmax, ymax int) (r []Pos) {
	// Brute force : far too long...
	for y := 0; y < ymax; y++ {
		for x := 0; x < xmax; x++ {
			seen := false
			for _, s := range sensors {
				if manhattan_dist(Pos{x, y}, s.pos) <= s.dist {
					seen = true
					break
				}
			}
			if !seen {
				r = append(r, Pos{x, y})
			}
		}
	}
	return
}

type xRange struct {
	start, end int
}

func cut_range(r []xRange, cut xRange) []xRange {
	// r is suppose to be a sorted, non overlapping slice of ranges
	for i := 0; i < len(r); {
		if cut.start <= r[i].start {
			if cut.end >= r[i].end {
				//   r :    |---|
				// cut : |---------|
				r = utils.Remove(r, i) // i is now index of next range
			} else if cut.end >= r[i].start {
				//   r :    |-----|
				// cut : |-----|
				r[i].start = cut.end + 1
				break
			} else {
				//   r :       |---|
				// cut : |---|
				break
			}
		} else if cut.start <= r[i].end {
			if cut.end >= r[i].end {
				//   r : |-----|
				// cut :    |-----|
				r[i].end = cut.start - 1
				i++
			} else {
				//   r : |---------|
				// cut :    |---|
				s := r[i].start
				r[i].start = cut.end + 1
				r = utils.Insert(r, i, xRange{s, cut.start - 1})
				break
			}
		} else {
			//   r : |---|
			// cut :       |---|
			i++
		}
	}
	return r
}

func free_in_range2(sensors []Sensor, xmax, ymax int) (r []Pos) {
	line_ranges := make([][]xRange, ymax)
	for i := range line_ranges {
		line_ranges[i] = []xRange{{0, xmax}}
	}

	for _, s := range sensors {
		for y := 0; y <= s.dist; y++ {
			y1, y2 := s.pos.y-s.dist+y, s.pos.y+s.dist-y
			px, gx := s.pos.x-y, s.pos.x+y
			if y1 >= 0 && y1 < ymax {
				r := &line_ranges[y1]
				*r = cut_range(*r, xRange{px, gx})
			}
			if y2 != y1 && y2 >= 0 && y2 < ymax {
				r := &line_ranges[y2]
				*r = cut_range(*r, xRange{px, gx})
			}
		}
	}

	for y := range line_ranges {
		for _, lr := range line_ranges[y] {
			for x := lr.start; x <= lr.end; x++ {
				r = append(r, Pos{x, y})
			}
		}
	}
	return
}

func main() {
	s := utils.ReadFileLines("input")
	sensors := parse_input(s)

	fmt.Println("Part 1: ", empty_in_line(sensors, 2000000))

	t := free_in_range2(sensors, TUN_FACTOR, TUN_FACTOR)
	//fmt.Println(t)
	fmt.Println("Part 2: ", t[0].x*TUN_FACTOR+t[0].y)
}
