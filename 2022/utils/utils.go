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

func MakeSlice[T any](size int, value T) []T {
	r := make([]T, size)
	for i := range r {
		r[i] = value
	}
	return r
}

func MinMax[T Number](m T, marr ...T) (min T, max T) {
	min, max = m, m
	for i := range marr {
		if marr[i] > max {
			max = marr[i]
		} else if marr[i] < min {
			min = marr[i]
		}
	}
	return
}

func IndexOf[T comparable](arr []T, val T) int {
	for i, v := range arr {
		if v == val {
			return i
		}
	}
	return -1
}

func Remove[T any](arr []T, i int) []T {
	return append(arr[:i], arr[i+1:]...)
}

func Insert[T any](arr []T, i int, value T) []T {
	if i == len(arr) {
		return append(arr, value)
	}
	arr = append(arr[:i+1], arr[i:]...)
	arr[i] = value
	return arr
}

////////////////////////////////////////////////////////
// Functional utils
////////////////////////////////////////////////////////

func Map[T any, M any](a []T, f func(T) M) []M {
	n := make([]M, 0, len(a))
	for _, e := range a {
		n = append(n, f(e))
	}
	return n
}

type Number interface {
	Signed | uint8
}

type Signed interface {
	int | int8
}

func Sum[T Number](arr []T) (sum T) {
	for _, value := range arr {
		sum += value
	}
	return
}

func Prod[T Number](arr []T) (p T) {
	p = 1
	for _, value := range arr {
		p *= value
	}
	return
}

func AMax[T Number](arr []T) T {
	m := arr[0]
	for i := 0; i < len(arr); i++ {
		if arr[i] > m {
			m = arr[i]
		}
	}
	return m
}

func AMin[T Number](arr []T) T {
	m := arr[0]
	for i := 0; i < len(arr); i++ {
		if arr[i] < m {
			m = arr[i]
		}
	}
	return m
}

func AMinMax[T Number](arr []T) (T, T) {
	return MinMax(arr[0], arr...)
}

func Abs[T Number](v T) T {
	if v >= 0 {
		return v
	} else {
		return -v
	}
}

func Sign[T Signed](v T) T {
	if v >= 0 {
		return T(1)
	} else {
		return T(-1)
	}
}

func B2i(b bool) int {
	if b {
		return 1
	} else {
		return 0
	}
}

func All(arr []bool) bool {
	for _, value := range arr {
		if !value {
			return false
		}
	}
	return true
}

////////////////////////////////////////////////////////
// Set
////////////////////////////////////////////////////////

var exists = struct{}{}

type Set[T comparable] struct {
	m map[T]struct{}
}

func (s *Set[T]) Add(vals ...T) {
	for _, val := range vals {
		s.m[val] = exists
	}
}

func (s *Set[T]) Remove(val T) {
	delete(s.m, val)
}

func (s *Set[T]) Contains(val T) bool {
	_, c := s.m[val]
	return c
}

func (s *Set[T]) Len() int {
	return len(s.m)
}

func NewSet[T comparable]() *Set[T] {
	s := &Set[T]{}
	s.m = make(map[T]struct{})
	return s
}

////////////////////////////////////////////////////////
// Queue
////////////////////////////////////////////////////////

type Queue[T any] struct {
	s []T
}

func (queue *Queue[T]) Push(val T) {
	// push back
	queue.s = append(queue.s, val)
}

func (q *Queue[T]) Front() T {
	return q.s[0]
}

func (q *Queue[T]) Back() T {
	return q.s[len(q.s)-1]
}

func (q *Queue[T]) Pop() (r T) {
	// pop front
	r = q.Front()
	q.s = q.s[1:]
	return
}

func (q *Queue[T]) PopBack() (r T) {
	r = q.Back()
	q.s = q.s[:len(q.s)-1]
	return
}

func (q *Queue[T]) Size() int {
	return len(q.s)
}

func (q *Queue[T]) Empty() bool {
	return len(q.s) == 0
}
