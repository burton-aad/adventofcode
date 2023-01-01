package main

import (
	"aoc/utils"
	"fmt"
	"math"
	"regexp"
	"strconv"
)

type Mineral int

const (
	ore Mineral = iota
	clay
	obsidian
	geode

	mineral_count
)

type Blueprint struct {
	id          int
	robot_costs [][]int
}

func parse_line(l string) (b Blueprint) {
	b.robot_costs = make([][]int, mineral_count)
	for i := range b.robot_costs {
		b.robot_costs[i] = make([]int, mineral_count-1)
	}

	re := regexp.MustCompile(`Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.`)
	a := re.FindStringSubmatch(l)
	b.id, _ = strconv.Atoi(a[1])
	b.robot_costs[ore][ore], _ = strconv.Atoi(a[2])
	b.robot_costs[clay][ore], _ = strconv.Atoi(a[3])
	b.robot_costs[obsidian][ore], _ = strconv.Atoi(a[4])
	b.robot_costs[obsidian][clay], _ = strconv.Atoi(a[5])
	b.robot_costs[geode][ore], _ = strconv.Atoi(a[6])
	b.robot_costs[geode][obsidian], _ = strconv.Atoi(a[7])
	return
}

type Game struct {
	time       int
	collecting []int
	resources  []int
}

func (g Game) inc_time(time int) Game {
	return Game{g.time + time,
		[]int{g.collecting[ore], g.collecting[clay], g.collecting[obsidian], g.collecting[geode]},
		[]int{g.resources[ore] + g.collecting[ore]*time,
			g.resources[clay] + g.collecting[clay]*time,
			g.resources[obsidian] + g.collecting[obsidian]*time,
			g.resources[geode] + g.collecting[geode]*time}}
}

func bfs(b Blueprint, limit_time int) (r Game) {
	r.resources = make([]int, mineral_count)
	q := utils.Queue[Game]{}
	q.Push(Game{0, []int{1, 0, 0, 0}, []int{0, 0, 0, 0}})
	visited := utils.NewSet[string]()

	max_g := make([]int, limit_time)
	for c := 1; c < limit_time; c++ {
		max_g[c] = max_g[c-1] + (c - 1)
	}

	make_robot := func(g Game, cost []int) int {
		time := []int{0, 0, 0}
		time[ore] = int(math.Ceil(float64(cost[ore]-g.resources[ore]) / float64(g.collecting[ore])))
		if cost[clay] > 0 {
			if g.collecting[clay] == 0 {
				return math.MaxInt32
			} else {
				time[clay] = int(math.Ceil(float64(cost[clay]-g.resources[clay]) / float64(g.collecting[clay])))
			}
		}
		if cost[obsidian] > 0 {
			if g.collecting[obsidian] == 0 {
				return math.MaxInt32
			} else {
				time[obsidian] = int(math.Ceil(float64(cost[obsidian]-g.resources[obsidian]) / float64(g.collecting[obsidian])))
			}
		}
		return utils.AMax(time)
	}

	h := func(g Game) string {
		return fmt.Sprintf("%d-%d-%d-%d|%d-%d-%d-%d", g.collecting[ore], g.collecting[clay], g.collecting[obsidian], g.collecting[geode], g.resources[ore], g.resources[clay], g.resources[obsidian], g.resources[geode])
	}

	for !q.Empty() {
		g := q.PopBack()

		time_left := limit_time - g.time
		if g.time > 0 && g.resources[geode]+g.collecting[geode]*time_left+max_g[time_left] <= r.resources[geode] {
			continue
		}

		next := false
		for _, m := range []Mineral{ore, clay, obsidian, geode} {
			t_build := make_robot(g, b.robot_costs[m])
			if g.time+t_build+1 < limit_time {
				next = true
				ng := g.inc_time(t_build + 1)
				ng.collecting[m] += 1
				ng.resources[ore] -= b.robot_costs[m][ore]
				ng.resources[clay] -= b.robot_costs[m][clay]
				ng.resources[obsidian] -= b.robot_costs[m][obsidian]
				if !visited.Contains(h(ng)) {
					q.Push(ng)
					visited.Add(h(ng))
				}
			}
		}
		if !next {
			ng := g.inc_time(time_left)
			if ng.resources[geode] > r.resources[geode] {
				r = ng
			}
		}
	}

	return
}

const TIME_LIMIT = 24
const TIME_LIMIT_2 = 32

func main() {
	s := utils.ReadFileLines("input")
	blueprints := utils.Map(s, parse_line)

	m := 0
	for _, b := range blueprints {
		// fmt.Println(b)
		r := bfs(b, TIME_LIMIT)
		// fmt.Println(r)
		m += b.id * r.resources[geode]
	}
	fmt.Println("Part 1: ", m)

	m2 := 1
	for i := 0; i < 3; i++ {
		// fmt.Println(blueprints[i])
		r := bfs(blueprints[i], TIME_LIMIT_2)
		// fmt.Println(r)
		m2 *= r.resources[geode]
	}
	fmt.Println("Part 2: ", m2)
}
