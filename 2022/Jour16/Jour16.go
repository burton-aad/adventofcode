package main

import (
	"aoc/utils"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

type Valve struct {
	name      string
	flow_rate int
	links     []string
	dist      map[string]int
}

func parse_input(s []string) map[string]Valve {
	valves := make(map[string]Valve)
	re := regexp.MustCompile(`Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)`)
	for _, l := range s {
		a := re.FindStringSubmatch(l)
		name := a[1]
		flow, _ := strconv.Atoi(a[2])
		links := strings.Split(a[3], ", ")
		valves[name] = Valve{name, flow, links, make(map[string]int)}
	}

	for n, v := range valves {
		q, i := utils.Queue[string]{}, 1
		q.Push(v.links...)
		q.Push("")
		for q.Size() > 1 {
			l := q.Pop()
			if l == "" {
				i++
				q.Push("")
			} else if l == n {
				continue
			} else if _, ok := v.dist[l]; !ok {
				v.dist[l] = i
				q.Push(valves[l].links...)
			}
		}
	}

	return valves
}

type Path struct {
	cur              string
	path             []string
	possible_next    []string
	time             int
	release_pressure int
}

func main() {
	s := utils.ReadFileLines("input")
	valves := parse_input(s)

	var nonnull_v []string
	for _, v := range valves {
		if v.flow_rate > 0 {
			nonnull_v = append(nonnull_v, v.name)
		}
	}

	q, max := utils.Queue[Path]{}, Path{"AA", []string{}, nonnull_v, 30, 0}
	q.Push(max)
	for !q.Empty() {
		cur := q.Pop()
		if len(cur.possible_next) == 0 && max.release_pressure < cur.release_pressure {
			max = cur
		}

		cur_path := make([]string, 0, len(cur.path)+1)
		cur_path = append(cur_path, cur.path...)
		cur_path = append(cur_path, cur.cur)
		for i, n := range cur.possible_next {
			time_spent := valves[cur.cur].dist[n] + 1
			if time_spent < cur.time {
				q.Push(
					Path{
						n, cur_path,
						utils.RemoveCopy(cur.possible_next, i),
						cur.time - time_spent,
						cur.release_pressure + (cur.time-time_spent)*valves[n].flow_rate})
			} else if max.release_pressure < cur.release_pressure {
				max = cur
			}
		}
	}
	fmt.Println("Part 1: ", max.release_pressure)
}
