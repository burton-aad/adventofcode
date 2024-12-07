package main

import (
	"aoc/utils"
	"fmt"
	"os"
	"regexp"
	"sort"
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
	path []string
	time int
}

type MultiPath struct {
	paths            []Path
	possible_next    []string
	release_pressure int
}

func next_paths(p MultiPath, valves map[string]Valve) (r []MultiPath) {
	// Brute force
	cur := 0
	for i := range p.paths {
		if p.paths[i].time > p.paths[cur].time {
			cur = i
		}
	}

	cur_pos := p.paths[cur].path[len(p.paths[cur].path)-1]
	for i, n := range p.possible_next {
		time_spent := valves[cur_pos].dist[n] + 1
		if time_spent < p.paths[cur].time {
			cur_path := make([]string, 0, len(p.paths[cur].path)+1)
			cur_path = append(cur_path, p.paths[cur].path...)
			r = append(r,
				MultiPath{
					append(utils.RemoveCopy(p.paths, cur),
						Path{append(cur_path, n), p.paths[cur].time - time_spent}),
					utils.RemoveCopy(p.possible_next, i),
					p.release_pressure + (p.paths[cur].time-time_spent)*valves[n].flow_rate})
		}
	}
	return
}

func next_paths2(p MultiPath, valves map[string]Valve) (r []MultiPath) {
	// Brute force mais un peu intelligent

	// Tri du plus de temps
	var rgn []int
	for i := range p.paths {
		rgn = append(rgn, i)
	}
	sort.Slice(rgn, func(i, j int) bool { return p.paths[i].time > p.paths[j].time })

	for _, cur := range rgn {
		cur_pos := p.paths[cur].path[len(p.paths[cur].path)-1]
		m_gain, m_next := 0, ""
		for _, n := range p.possible_next {
			time_spent := valves[cur_pos].dist[n] + 1
			flow_rate := valves[n].flow_rate
			gain := (p.paths[cur].time - time_spent) * flow_rate
			if time_spent < p.paths[cur].time && gain > m_gain {
				m_gain = gain
				m_next = n
			}
		}

		// Choisi la solution optimal ou une autre plus proche
		for i, n := range p.possible_next {
			if n == m_next || valves[cur_pos].dist[n] <= valves[cur_pos].dist[m_next] {
				time_spent := valves[cur_pos].dist[n] + 1
				cur_path := make([]string, 0, len(p.paths[cur].path)+1)
				cur_path = append(cur_path, p.paths[cur].path...)
				r = append(r,
					MultiPath{
						append(utils.RemoveCopy(p.paths, cur),
							Path{append(cur_path, n), p.paths[cur].time - time_spent}),
						utils.RemoveCopy(p.possible_next, i),
						p.release_pressure + (p.paths[cur].time-time_spent)*valves[n].flow_rate})
			}
		}

		// Essai un autre si le courant est bloqué.
		if len(r) > 0 {
			break
		}
	}

	return
}

func main() {
	input := "input"
	if len(os.Args) > 1 {
		input = os.Args[1]
	}

	s := utils.ReadFileLines(input)
	valves := parse_input(s)

	var nonnull_v []string
	for _, v := range valves {
		if v.flow_rate > 0 {
			nonnull_v = append(nonnull_v, v.name)
		}
	}

	max := MultiPath{[]Path{{[]string{"AA"}, 30}}, nonnull_v, 0}
	q := utils.Queue[MultiPath]{}
	q.Push(max)
	for !q.Empty() {
		cur := q.Pop()
		nps := next_paths2(cur, valves)
		if len(nps) == 0 && max.release_pressure < cur.release_pressure {
			max = cur
		}
		q.Push(nps...)
	}
	fmt.Println("Part 1: ", max.paths, max.release_pressure)

	max = MultiPath{[]Path{{[]string{"AA"}, 26}, {[]string{"AA"}, 26}}, nonnull_v, 0}
	q.Push(max)
	cnt, last := 0, 0
	for !q.Empty() {
		cnt++

		cur := q.Pop()
		// nps := next_paths(cur, valves)
		nps := next_paths2(cur, valves)
		if len(nps) == 0 && max.release_pressure < cur.release_pressure {
			max = cur
		}
		q.Push(nps...)

		if cnt%10_000 == 0 {
			fmt.Printf("%d -> %d (delta %d)\n", cnt, q.Size(), q.Size()-last)
			last = q.Size()
		}
	}
	fmt.Println("count ", cnt)
	fmt.Println("Part 2: ", max.paths, max.release_pressure)
}
