package main

import (
	"aoc/utils"
	"fmt"
	"strconv"
	"strings"
)

type File struct {
	name string
	size int
}

type Dir struct {
	name    string
	subdirs map[string]Dir
	files   map[string]File
}

func (d *Dir) mkdir(name string) {
	if _, alredyExist := d.subdirs[name]; alredyExist {
		return // No error -> mkdir -p
	}
	sd := Dir{name, map[string]Dir{"..": *d}, make(map[string]File)}
	d.subdirs[name] = sd
}

func (d *Dir) touch(name string, size int) {
	d.files[name] = File{name, size}
}

func (d *Dir) rprint(indent string) {
	fmt.Printf("%s- %s (dir)\n", indent, d.name)
	indent += "  "
	for n, sd := range d.subdirs {
		if n != ".." {
			sd.rprint(indent)
		}
	}
	for _, f := range d.files {
		fmt.Printf("%s- %s (file, size=%d)\n", indent, f.name, f.size)
	}
}

func (d *Dir) print_dir() {
	d.rprint("")
}

type command int

const (
	cd command = iota
	ls
)

func make_root(s []string) Dir {
	root := Dir{"/", make(map[string]Dir), make(map[string]File)}
	curDir := root
	curCommand := cd
	for _, l := range s {
		if l == "$ cd /" {
			continue
		}

		line := strings.Fields(l)
		if line[0] == "$" {
			// Command
			switch line[1] {
			case "cd":
				curDir.mkdir(line[2])
				curDir = curDir.subdirs[line[2]]
				curCommand = cd
			case "ls":
				curCommand = ls
			}
		} else {
			switch curCommand {
			case ls:
				if line[0] == "dir" {
					curDir.mkdir(line[1])
				} else {
					s, _ := strconv.Atoi(line[0])
					curDir.touch(line[1], s)
				}
			}
		}
	}

	return root
}

func dirs_size(root Dir) map[string]int {
	r := map[string]int{root.name: 0}
	sep := "/"
	if root.name == "/" {
		sep = ""
	}
	size := 0
	for n, sd := range root.subdirs {
		if n == ".." {
			continue
		}
		for n, s := range dirs_size(sd) {
			r[root.name+sep+n] = s
		}
		size += r[root.name+sep+sd.name]
	}
	for _, f := range root.files {
		size += f.size
	}
	r[root.name] = size
	return r
}

const TOTAL_DISK_SPACE = 70_000_000
const SPACE_NEEDED = 30_000_000

func main() {
	s := utils.ReadFileLines("input")
	root := make_root(s)
	// root.print_dir()
	d_size := dirs_size(root)

	p1 := 0
	free_space := TOTAL_DISK_SPACE - d_size["/"]
	p2 := TOTAL_DISK_SPACE
	for _, s := range d_size {
		if s < 100000 {
			p1 += s
		}
		if free_space+s > SPACE_NEEDED && s < p2 {
			p2 = s
		}
	}
	fmt.Println("Part 1: ", p1)
	fmt.Println("Part 2: ", p2)
}
