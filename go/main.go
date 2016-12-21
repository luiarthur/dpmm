package main

import (
	"fmt"
	"time"
)

func gibbs(init *State, B int, burn int, every int) []*State {
	out := make([]*State, B)
	out[0] = init

	for i := 0; i < B+burn; i++ {
		if i <= burn {
			out[0] = update(out[0])
		} else {
			out[i-burn] = update(out[i-burn-1])
		}
	}

	return out
}

// Edit Below:
type State struct {
	x int
}

func update(s *State) *State {
	return &State{s.x + 1}
}

func main() {
	init := State{1}

	t1 := time.Now()
	gibbs(&init, 1000000, 1000, 0)
	t2 := time.Since(t1)

	fmt.Println(t2)
}
