package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"bufio"
	"strconv"
	"strings"
	"time"
)

/*------------------------CONST------------------------------*/

const S int = 9
const US uint = uint(S)
const SS int = S * S
const R int = 3
const ALL Value = (1 << US) - 1

type Value uint

/* This is the main struct of the program.
 * It stores the board, a list with all possible values for each
 * row / col / sqr and a list of the remaning indices in the board.
 * It stores each value as a power of 2, the posibles values are just
 * |s of all the possibilities
 */
type Sudoku struct {
	board  [SS]Value
	colsAv [S]Value
	rowsAv [S]Value
	sqrsAv [S]Value
}

/*-------------------------GLOBAL----------------------------*/

var EMPTY Sudoku = Sudoku{}

//To count the nodes and changes made
var forcedChanges int64
var nodes int64

var remeaning [SS]int
var counter int

/*---------------------INITIALIZATION------------------------*/
/* Test sudoku:
 024000000
 000007100
 090000000

 000000084
 000075000
 600030000

 000400029
 000200300
 100000000
*/

func Parse(s string) (sud Sudoku) {
	//It is assumed that the str is of length 81
	var e Value
	temp_counter := 0
	for i := 0; i < S; i++ {
		for j := 0; j < S; j++ {
			e = Value(s[i*S+j] - '0')
			if e == 0 || s[i*S+j] == '.' {
				sud.board[index(i, j)] = 0
				remeaning[temp_counter] = index(i, j)
				temp_counter++
			} else {
				sud.board[index(i, j)] = 1 << uint(e-1)
			}
		}
	}
	counter = temp_counter

	return
}

/*---------------------HELPER------------------------*/

func log2(v Value) int {
	// Returns 1 + log2(n) or 0
	if v == 0 {
		return 0
	} else {
		var log int
		for v != 1 {
			log++
			v >>= 1
		}
		return 1 + log
	}
}

//It is also possible use OnesCount from math/bits, but this is only available in version >= 1.9
func popCount(v Value) Value {
	//The numbers are in octal
	y := (v >> 1) & 033333333333
	z := v - y - ((y >> 1) & 033333333333)
	return ((z + (z >> 3)) & 030707070707) % 63
}

func index(i, j int) int {
	return S * i + j
}
func coord(i int) (int, int) {
	return i / S, i % S
}
func getSqrIndex(i, j int) int {
	return R * (i / R) + (j / R)
}
func isPow2(v Value) bool {
	return v & (v - 1) == 0 && v != 0
}

/*---------------------CHECKING------------------------*/

func (self Sudoku) PrintSudoku(pretty bool) string {
	/* String representation of the Sudoku. TODO: Implement it as a to_string() */
	var builder strings.Builder
	if !pretty {
		for i := 0; i < SS; i++ {
			builder.WriteString(strconv.Itoa(log2(self.board[i])))
		}
		return builder.String()
	}
	builder.WriteString("--------------------\n")
	for i := 0; i < S; i++ {
		for j := 0; j < S; j++ {

			if self.board[index(i, j)] == 0 {
				builder.WriteString("- ")
			} else {
				builder.WriteString(strconv.Itoa(log2(self.board[index(i, j)])) + " ")
			}

			if j % R == R - 1 {
				builder.WriteString(" ")
			}
		}

		builder.WriteString("\n")

		if i % R == R - 1 && i != S - 1 {
			builder.WriteString("\n")
		}
	}

	return builder.String() + "--------------------" + strconv.Itoa(self.filledTiles()) + "/" + strconv.Itoa(SS)
}

func (self Sudoku) filledTiles() (t int) {
	/* Returns the number of filled tiles */
	for i := 0; i < SS; i++ {
		if self.board[i] != 0 {
			t++
		}
	}

	return
}

func (self Sudoku) finished() bool {
	/* Returns true if all the tiles are set, false otherwise */
	for i := 0; i < SS; i++ {
		if self.board[i] == 0 {
			return false
		}
	}

	return true
}

func (self Sudoku) verifySudoku() bool {
	/* Returns if a sudoku is valid */
	var sum Value
	if !self.finished() {
		return false
	}
	//Check rows
	for i := 0; i < S; i++ {
		sum = 0
		for j := 0; j < S; j++ {
			if sum&self.board[index(i, j)] != 0 {
				return false
			}
			sum += self.board[index(i, j)]
		}
		if sum != ALL {
			return false
		}
	}
	//Check cols
	for i := 0; i < S; i++ {
		sum = 0
		for j := 0; j < S; j++ {
			if sum&self.board[index(j, i)] != 0 {
				return false
			}
			sum += self.board[index(j, i)]
		}
		if sum != ALL {
			return false
		}
	}
	//Check sqrs
	for x := 0; x < R; x++ {
		for y := 0; y < R; y++ {
			sum = 0
			for i := 0; i < R; i++ {
				for j := 0; j < R; j++ {
					if sum&self.board[index(R*x+i, R*y+j)] != 0 {
						return false
					}
					sum += self.board[index(R*x+i, R*y+j)]
				}
			}
			if sum != ALL {
				return false
			}
		}
	}

	return true
}

/*--------------------------SOLVER-----------------------------*/

func (self Sudoku) possible(index int) Value {
	/* Returns the possible values for a given index */
	i, j := coord(index)
	return self.rowsAv[i] & self.colsAv[j] & self.sqrsAv[getSqrIndex(i, j)]
}

func setPossible(s *Sudoku) {
	/* Initializes rows, cols, sqrs with the possibilities */
	var tempRows, tempCols, tempSqrs [S]Value
	var val Value

	for i := 0; i < S; i++ {
		for j := 0; j < S; j++ {
			val = s.board[index(i, j)]

			tempRows[i] |= val
			tempCols[j] |= val
			tempSqrs[getSqrIndex(i, j)] |= val
		}
	}

	for i := 0; i < S; i++ {
		s.rowsAv[i] = ALL ^ tempRows[i]
		s.colsAv[i] = ALL ^ tempCols[i]
		s.sqrsAv[i] = ALL ^ tempSqrs[i]
	}
}

func updateOne(index int, s *Sudoku) {
	/* Removes board[index] from the possible values in its row / col / sqr */
	i, j := coord(index)

	s.rowsAv[i] &= ^s.board[index]
	s.colsAv[j] &= ^s.board[index]
	s.sqrsAv[getSqrIndex(i, j)] &= ^s.board[index]
}

func setAllForced(s *Sudoku) bool {
	/* nodes set_forced until there are no more forced tiles.
	 * Returns false if the board is unsolvable
	 */
	var i, j, m int
	var val int
	var available, mask Value

	for lastUpdate := true; lastUpdate; {

		lastUpdate = false

		for k := 0; k < counter; k++ {
			val = remeaning[k]
			if s.board[val] != 0 {
				continue
			}

			i, j = coord(val)
			m = getSqrIndex(i, j)
			available = s.rowsAv[i] & s.colsAv[j] & s.sqrsAv[m]


			if available == 0 {
				return false
			}

			if !isPow2(available) {
				for j2 := 0; j2 < S && available != 0; j2++ {
					if j2 == j || s.board[index(i, j2)] != 0 {
						continue
					}

					available &= ^(s.colsAv[j2] & s.sqrsAv[getSqrIndex(i, j2)])
				}
				if !isPow2(available) {
					for i2 := 0; i2 < S && available != 0; i2++ {
						if i2 == i || s.board[index(i2, j)] != 0 {
							continue
						}

						available &= ^(s.rowsAv[i2] & s.sqrsAv[getSqrIndex(i2, j)])
					}
				}
			}

			if isPow2(available) {
				s.board[val] = available
				mask = ^available

				s.rowsAv[i] &= mask
				s.colsAv[j] &= mask
				s.sqrsAv[m] &= mask

				lastUpdate = true

				forcedChanges++
			}
		}
	}

	return true
}

func (self Sudoku) solve() (bool, Sudoku) {
	/*
	 * Main algorithm, tries to set all the forced values
	 * When that is no longer possible, it branches and checks if
	 * the resulting sudokus are solvable, it tries to reach as deep as
	 * possible in each branch. It is recursive. Returns false, _ if the
	 * sudoku is invalid
	 */
	nodes++

	isPos := setAllForced(&self)

	if !isPos {
		return false, self
	} else if self.finished() {
		return true, self
	}

	var index int

	//Find the most optimal tile (only 2 possible) or a suboptimal (3 possible)
	//Wort case: Nothing is found and we will use the one from the first loop
	for _, i := range remeaning {
		if self.board[i] != 0 {
			continue
		}
		tempPC := popCount(self.possible(i))

		if tempPC == 2 {
			index = i
			break
		} else if tempPC == 3 {
			index = i
		}
	}

	allPos := self.possible(index)
	for allPos != 0 {
		val := allPos & (^allPos + 1)
		allPos &= allPos - 1

		newS := Sudoku(self)
		newS.board[index] = val
		updateOne(index, &newS)

		pos, newS := newS.solve()
		if pos {
			return true, newS
		}
	}

	return false, EMPTY
}

func (self Sudoku) solveMain(parallel bool) (Sudoku, bool /*valid*/, int64 /*forcedChanges*/, int64 /*nodes*/) {

	setPossible(&self)
	isPos := setAllForced(&self)

	forcedChanges = 0
	nodes = 0

	if !isPos {
		return EMPTY, false, 0, 0
	} else if self.finished() {
		return self, true, int64(len(remeaning)), 0
	}

	//Find the first empty tile in the board
	var index int

	//Find the most optimal tile (only 2 possible) or a suboptimal (3 possible)
	//Wort case: Nothing is found and we will use the one from the first loop
	for _, i := range remeaning {
		if self.board[i] != 0 {
			continue
		}
		tempPC := popCount(self.possible(i))

		if tempPC == 2 {
			index = i
			break
		} else if tempPC == 3 {
			index = i
		}
	}

	allPos := self.possible(index)
	pc := popCount(allPos)

	ch := make(chan Sudoku, pc)
	for allPos != 0 {
		val := allPos & (^allPos + 1)
		allPos &= allPos - 1

		newS := Sudoku(self) //Duplicates self
		newS.board[index] = val
		updateOne(index, &newS)

		if parallel {
			ch <- helper(newS)
		} else {
			pos, newS := newS.solve()
			if pos {
				close(ch)
				return newS, true, forcedChanges, nodes
			}
		}
	}
	close(ch)

	if parallel {
		for nxt := range ch {
			if nxt != EMPTY {
				ok := nxt.filledTiles() == SS && nxt.verifySudoku()
				return nxt, ok, forcedChanges, nodes
			}
		}
	}

	return EMPTY, false, forcedChanges, nodes
}

func helper(s Sudoku) Sudoku {
	_, s = s.solve()
	return s
}


func solveFile(path string) (int64, int64){

	buf, err := os.Open(path)
    if err != nil {
        log.Fatal(err)
    }

    defer func() {
        if err = buf.Close(); err != nil {
            log.Fatal(err)
        }
    }()

    fC := int64(0)
    n := int64(0)
    snl := bufio.NewScanner(buf)
    for snl.Scan() {
    	sud := Parse(snl.Text())
        _, _, fC_, n_ := sud.solveMain(false)
    	fC += fC_
    	n += n_
    }
    err = snl.Err()
    if err != nil {
        log.Fatal(err)
    }

    return fC, n
}

func main() {
	bench := flag.Bool("bench", false, "The default 17 clue sudoku for simple benchmarking / testing")
	pretty := flag.Bool("pretty", false, "Draw the result as a pretty sudoku, use if a human is going to read the output")
	info := flag.Bool("info", false, "Information about the time taken and the nodes")
	parallel := flag.Bool("p", false, "Try to use multithreading to speed up the algorithm")

	flag.Parse()

	start := time.Now()

	var forcedChanges, nodes int64

	if len(os.Args) <= 1 {
		fmt.Println("Type -h for help")
	} else if *bench {
		s := Parse("024000000000007100090000000000000084000075000600030000000400029000200300100000000")
		s, _, fC_, n_ := s.solveMain(*parallel)
		fmt.Println(s.PrintSudoku(*pretty))

		forcedChanges += fC_
		nodes += n_
	} else {
		args := os.Args[1:]
		for _, path := range args {
			_, err := os.Stat(path)

			//We skip arguments that don't correspond to files
			if os.IsNotExist(err) {
				continue
			}

			fC_, n_ := solveFile(path)
			forcedChanges += fC_
			nodes += n_
		}
	}

	if *info {
		fmt.Println("\nTotal changes:", forcedChanges)
		fmt.Println("Total nodes:", nodes)
		fmt.Println("Time:", time.Now().Sub(start))
	}
}
