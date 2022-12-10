package main

import (
	"aoc/utils"
	"fmt"
	"strconv"
	"strings"
)

type Register int
type CPU struct {
	cycle int // cycle counter
	X     Register

	cycle_hooks []func(CPU)
}

func NewCpu() CPU {
	c := CPU{}
	c.X = 1
	return c
}

func (c *CPU) call_cycle_hook() {
	c.cycle += 1
	for _, f := range c.cycle_hooks {
		f(*c)
	}
}

func (c *CPU) run_instruction(instruction string, args ...string) {
	switch instruction {
	case "noop":
		c.call_cycle_hook()
	case "addx":
		c.call_cycle_hook()
		c.call_cycle_hook()
		i, _ := strconv.Atoi(args[0])
		c.X += Register(i)
	default:
		panic("Unhandle instruction " + instruction)
	}
}

type CRT struct {
	screen [][]byte
	x      int
	y      int
}

func NewCrt(width int, height int) CRT {
	c := CRT{make([][]byte, height), 0, 0}
	for i := range c.screen {
		c.screen[i] = make([]byte, width)
		for j := range c.screen[i] {
			c.screen[i][j] = '.'
		}
	}
	return c
}

func (c *CRT) print_screen() {
	for _, s := range c.screen {
		fmt.Println(string(s))
	}
}

func (c *CRT) up_cycle(cpu CPU) {
	X := int(cpu.X)
	if c.x == X-1 || c.x == X || c.x == X+1 {
		c.screen[c.y][c.x] = '#'
	}
	c.x++
	if c.x == len(c.screen[c.y]) {
		c.x, c.y = 0, c.y+1
	}
}

const (
	WIDTH  = 40
	HEIGHT = 6
)

func main() {
	s := utils.ReadFileLines("input")
	cpu := NewCpu()
	crt := NewCrt(WIDTH, HEIGHT)
	signal_strength, next_cycle := 0, 20

	c_hook := func(c CPU) {
		if c.cycle == next_cycle {
			signal_strength += c.cycle * int(c.X)
			next_cycle += 40
		}
	}
	cpu.cycle_hooks = append(cpu.cycle_hooks, c_hook)
	cpu.cycle_hooks = append(cpu.cycle_hooks, crt.up_cycle)

	for i := range s {
		line := strings.Fields(s[i])
		cpu.run_instruction(line[0], line[1:]...)
	}

	fmt.Println("Part 1:", signal_strength)
	fmt.Println("Part 2:")
	crt.print_screen()
}
