package utils

import (
	"bufio"
	"os"
)

func ReadFileLines(path string) (s []string) {
	readFile, err := os.Open(path)
	if err != nil {
		panic(err)
	}
	defer readFile.Close()

	fileScanner := bufio.NewScanner(readFile)
	fileScanner.Split(bufio.ScanLines)

	for fileScanner.Scan() {
		s = append(s, fileScanner.Text())
	}

	return
}

func Map[T any, M any](a []T, f func(T) M) []M {
	n := make([]M, 0, len(a))
	for _, e := range a {
		n = append(n, f(e))
	}
	return n
}

func Sum(arr []int) (sum int) {
	for _, value := range arr {
		sum += value
	}
	return
}

func All(arr []bool) bool {
	for _, value := range arr {
		if !value {
			return false
		}
	}
	return true
}
