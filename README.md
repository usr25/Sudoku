# Sudoku
Sudoku-solving algorithm implemented in different languages to observe the difference among them and, specially, the performance.

O(_n_ ^_m_) algorithm, where _n_ is the number of empty tiles and _m_ the possible values (In this case, [1, 9]). Sudokus are an NP-Complete problem.

### Use
_Requires go, rustc, gcc, python2/3, Cython and ghc. Other compilers may yield different results_


Place the files in a directory "dir"

$ `go install "dir/GO/Sudoku.go"  #Binary should be placed in GOBIN`

$ `rustc -O "dir/Rust/Sudoku.rs"  #Run the resulting binary file in $pwd`

$ `gcc -O3 -std=c11 -faggressive-loop-optimizations "dir/C/Sudoku.c" #Run the resulting binary in $pwd`

$ `python3 "dir/Python/sudoku.py" #Can also be run with python2`

$ `python2 "dir/Python/compiler.py"  #Requires Cython, check the [official website](https://cython.org/)`

$ `ghc -O2 -optc-O3 "dir/Haskell/SudokuMain.hs #-O3 improves performance x5`
 

### Results
In my computer old laptop (2 cores @ 1.33 GHz), solving the 17-clue proper sudoku which is hard coded, the proportions should stay the same regardless of the device

  * **GO**     -> 306ms, 306%
  * **Rust**   -> 90ms, 90%
  * **Rust P** -> 95ms, 95%
  * **C**      -> 100ms, 100%
  * **Python2** -> 9.8s, 9800%
  * **Python3** -> 14.2s, 14200%
  * **Cython** -> 1.2s, 1200%
  * **Haskell** -> 7.5s, 7500%

### Algorithm
  Note that in order to have a unique solution (proper sudoku), a sudoku has to have at least 17 clues (Having 17 clues does not imply it is a proper sudoku).

  The possible values for each square are the intersection of the possible values in its row / col / sqr

  1. Generate 3 arrays with the possible values for each row / col / square. (An empty row should have all possible values and a full one, zero)

  2. Set all values which are forced (only have 1 possibility)

  3. If there are no remeaning empty tiles: END

  4. Else: Select a tile and, for each value, try to solve the sudoku by making a duplicates and assigning one of the possible values, goto (2.)


### Notes
  * Solving sudokus benefits greatly from destructive writing into arrays, which is the opposite of what Haskell is used for (it can be done using the ST monad, generating messy and suboptimal code). The Haskell implementation involves a lot of unnecessary copying / writing.

  * Python2(.7) is significantly faster than Python3(.5/.7). This is due to Py3 using long integers, paired with the abundant use of integers in the sudoku

  * Concurrency isn't always an improvement: in Go it yields better results, in Python the same, and worse in Rust.