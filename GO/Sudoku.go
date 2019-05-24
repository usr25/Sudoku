package main

import ("fmt";"strconv";"time")


/*------------------------CONST------------------------------*/

const S int = 9
const SS int = S*S
const R int = 3
const ALL Value = (1 << uint(S)) - 1

type Value uint


/* This is the main struct of the program.
 * It stores the board, a list with all possible values for each
 * row / col / sqr and a list of the remaning indices in the board.
 * It stores each value as a power of 2, the posibles values are just
 * |s of all the possibilities
 */
type Sudoku struct{
	board [SS]Value
	colsAv [S]Value
	rowsAv [S]Value
	sqrsAv [S]Value
}

/*-------------------------GLOBAL----------------------------*/

//To count the nodes and changes made
var forcedChanges int
var calls int

var remeaning [SS]int
var counter int

/*---------------------INITIALIZATION------------------------*/


func GenSud() (s Sudoku){
	/* Generates the sudoku, it is hard coded */
	temp_counter := 0
	
	sud := [S][S]Value{
		{0, 2, 4,  0, 0, 0,  0, 0, 0},
		{0, 0, 0,  0, 0, 7,  1, 0, 0},
		{0, 9, 0,  0, 0, 0,  0, 0, 0},
		{0, 0, 0,  0, 0, 0,  0, 8, 4},
		{0, 0, 0,  0, 7, 5,  0, 0, 0},
		{6, 0, 0,  0, 3, 0,  0, 0, 0},
		{0, 0, 0,  4, 0, 0,  0, 2, 9},
		{0, 0, 0,  2, 0, 0,  3, 0, 0},
		{1, 0, 0,  0, 0, 0,  0, 0, 0}} //17

	for i, l := range sud{
		for j, e := range l{
			if e == 0{
				s.board[index(i, j)] = 0
				remeaning[temp_counter] = index(i, j)
				temp_counter++
			}else{
				s.board[index(i, j)] = 1 << uint(e - 1)
			}
		}
	}
	counter = temp_counter

	return s
}

/*---------------------HELPER------------------------*/

func log2(v Value) int{
	/* Returns 1 + log2(n) or 0*/
	if v == 0{
		return 0
	}else{
		var log int
		for v != 1{
			log++
			v >>=1
		}
		return 1 + log
	}
}



func index(i, j int) int{
	return S*i + j
}
func coord(i int) (int, int){
	return i / S, i % S
}
func getSqrIndex(i, j int) int{
	return R*(i/R) + (j/R)
}
func isPow2(v Value) bool{
	return v & (v-1) == 0 && v != 0
}


/*---------------------CHECKING------------------------*/


func (self Sudoku) PrintSudoku() string{
	/* String representation of the Sudoku. TODO: Implement it as a to_string() */
	str := "--------------------\n"
	for i := 0; i < S; i++ {
		sSmall := ""
		for j := 0; j < S; j++ {

			if self.board[index(i, j)] == 0{
				sSmall += "- "
			}else{
				sSmall += strconv.Itoa(log2(self.board[index(i, j)])) + " "
			}

			if j % R == R - 1{
				sSmall += " "
			}
		}

		str += sSmall
		str += "\n"
		
		if i % R == R - 1 && i != S - 1{
			str += "\n"
		}
	}

	return str + "--------------------" + strconv.Itoa(self.filledTiles()) + "/" + strconv.Itoa(SS)
}

func (self Sudoku) filledTiles() int{
	/* Returns the number of filled tiles */
	t := 0
	for i := 0; i < SS; i++ {
		if self.board[i] != 0{
			t++
		}
	}
	return t
}

func (self Sudoku) finished() bool{
	/* Returns true if all the tiles are set, false otherwise */
	for i := 0; i < SS; i++ {
		if self.board[i] == 0{
			return false
		}
	}

	return true
}

func (self Sudoku) verifySudoku() bool{
	/* Returns if a sudoku is valid */
	var sum Value
	if ! self.finished(){
		return false
	}
	//Check rows
	for i := 0; i < S; i++ {
		sum = 0
		for j := 0; j < S; j++ {
			if sum & self.board[index(i, j)] != 0{
				return false
			}
			sum += self.board[index(i, j)]
		}
		if sum != ALL{
			return false
		}
	}
	//Check cols
	for i := 0; i < S; i++ {
		sum = 0
		for j := 0; j < S; j++ {
			if sum & self.board[index(j, i)] != 0{
				return false
			}
			sum += self.board[index(j, i)]
		}
		if sum != ALL{
			return false
		}
	}
	//Check sqrs
	for x := 0; x < R; x++ {
		for y := 0; y < R; y++ {
			sum = 0
			for i := 0; i < R; i++ {
				for j := 0; j < R; j++ {
					if sum & self.board[index(R*x + i, R*y + j)] != 0{
						return false
					}
					sum += self.board[index(R*x + i, R*y + j)]		
				}
			}
			if sum != ALL{
				return false
			}
		}
	}

	return true
}

/*--------------------------SOLVER-----------------------------*/

func (self Sudoku) canBeFinished() bool{
	/* Returns if a sudoku can be finished. true DOES NOT imply it can be finished.
	 * a sudoku cant be finished if one of the remaning tiles to fill has got no
	 * possibilities
	 */
	for i := 0; i < counter; i++{
		if self.board[remeaning[i]] == 0 && self.possible(remeaning[i]) == 0{
			return false
		}
	}

	return true
}


func (self Sudoku) possible(index int) Value{
	/* Returns the possible values for a given index */
	i, j := coord(index)
	return self.rowsAv[i] & self.colsAv[j] & self.sqrsAv[getSqrIndex(i, j)]
}

func setPossible(s *Sudoku){
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

func updateOne(index int, s *Sudoku){
	/* Removes board[index] from the possible values in its row / col / sqr */
	i, j := coord(index)

	s.rowsAv[i] &= ALL ^ s.board[index]
	s.colsAv[j] &= ALL ^ s.board[index]
	s.sqrsAv[getSqrIndex(i, j)] &= ALL ^ s.board[index]
}


func setAllForced(s *Sudoku) bool{
	/* Calls set_forced until there are no more forced tiles.
	 * Returns false if the board is unsolvable
	 */
	var i, j int
	var val int
	var available, mask Value

	for lastUpdate := true; lastUpdate;{

		lastUpdate = false

		for k := 0; k < counter; k++{
			val = remeaning[k]
			if s.board[val] != 0{
				continue
			}
			i, j = coord(val)
			available = s.rowsAv[i] & s.colsAv[j] & s.sqrsAv[getSqrIndex(i, j)]

			if available == 0{
				return false
			}

			if isPow2(available){
				s.board[val] = available
				mask = ALL ^ available
				
				s.rowsAv[i] &= mask
				s.colsAv[j] &= mask
				s.sqrsAv[getSqrIndex(i, j)] &= mask

				lastUpdate = true
				
				forcedChanges++ //DEBUG

			}
		}
	}
	
	return true
}


func (self Sudoku) solve() (bool, Sudoku){
	/*
	* Main algorithm, tries to set all the forced values
	* When that is no longer possible, it branches and checks if
	* the resulting sudokus are solvable, it tries to reach as deep as
	* possible in each branch. It is recursive. Returns false, _ if the
	* sudoku is invalid
	*/
	calls++

	isPos := setAllForced(&self)

	if ! isPos{
		return false, self
	}
	if self.finished(){
		return true, self
	}
	
	//Faster in some cases
	var index int
	for i := counter - 1; self.board[index] != 0 && i >= 0; i-- {
		index = remeaning[i]	
	}
	
	//Alternative, some further testing is required to determine which one to use
	/*
	for i := 0; s.board[index] != 0 && i < counter; i++ {
		index = remeaning[i]	
	}
	*/
	var allPos Value = self.possible(index)
	var val Value
	for i := 0; i < S; i++ {
		val = (1 << uint(i))
		if val & allPos == 0{
			continue
		}

		newS := Sudoku(self) //Duplicates s
		newS.board[index] = val
		updateOne(index, &newS)
		
		if newS.canBeFinished(){ //This reduces the number of branches by about 1/4
			pos, newS := newS.solve()
			if pos{
				return true, newS
			}
		}
	}

	return false, Sudoku{}
}

func SolveMaster(s Sudoku) (Sudoku, /*ok*/ bool, /*forcedChanges*/ int, /*calls*/ int){
	rs := Sudoku(s)

	setPossible(&rs)

	if ! rs.canBeFinished(){
		return s, false, 0, 0
	}
	_, rs = rs.solve()

	ok := rs.filledTiles() == SS && rs.verifySudoku()

	return rs, ok, forcedChanges, calls
}

func main(){

	s := GenSud()
	fmt.Println(s.PrintSudoku())
	
	start := time.Now()
	
	s, ok, forcedChanges, calls := SolveMaster(s)

	end := time.Now()

	fmt.Println(s.PrintSudoku())

	fmt.Println("\nTotal changes:", forcedChanges)
	fmt.Println("Total nodes:", calls)
	fmt.Println("Is valid:", ok)
	fmt.Println("Time:", end.Sub(start))
	
}