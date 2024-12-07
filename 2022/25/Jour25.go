package main

import (
	"aoc/utils"
	"fmt"
)

func unsnafu(s string) (i int) {
	for _, c := range s {
		i *= 5
		switch c {
		case '2', '1', '0':
			i += int(c - '0')
		case '-':
			i -= 1
		case '=':
			i -= 2
		}
	}
	return
}

func snafu(i int) string {
	var s []rune
	carry := 0
	for p := 1; p <= i; p *= 5 {
		r := (i%(p*5))/p + carry
		switch r {
		case 0, 1, 2:
			carry = 0
			s = utils.Insert(s, 0, rune('0'+r))
		case 3:
			carry = 1
			s = utils.Insert(s, 0, '=')
		case 4:
			carry = 1
			s = utils.Insert(s, 0, '-')
		case 5: // Possible with carry
			carry = 1
			s = utils.Insert(s, 0, '0')
		}
	}

	if carry > 0 {
		s = utils.Insert(s, 0, rune('0'+carry))
	}
	return string(s)
}

func main() {
	s := utils.ReadFileLines("input")
	// for _, l := range s {
	// 	fmt.Println(l, "->", unsnafu(l))
	// }

	sum := utils.Sum(utils.Map(s, unsnafu))
	fmt.Println("sum", sum, "->", snafu(sum))
}
