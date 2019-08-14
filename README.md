# Sudoku
Sudoku solving algorithm implemented in different languages to observe the differences among them and, specially, the performance.

O(_n_ ^_m_) algorithm, where _n_ is the number of empty tiles and _m_ the possible values (In this case, [1, 9] O(n^9)). Sudokus are an NP-Complete problem.

### Use
_Requires go, rustc, gcc, python2/3, Cython and ghc. Other compilers may yield different results_

There are compiled files in /Linux

cd into Sudoku/

$ `go install Go/Sudoku.go  #Binary should be placed in GOBIN, if it is not set type $export GOBIN=dirname or refer to the` [go installation guide](https://golang.org/doc/install)

$ `rustc -O dir/Rust/Sudoku.rs`

$ `gcc -O3 -std=c11 -faggressive-loop-optimizations C/Sudoku.c`

$ `python3 Python/sudoku.py #Can also be run with python2`

$ `python3 Python/compiler.py  #Requires Cython, check the` [official website](https://cython.org/)

$ `cd Haskell && ghc -O2 -optc-O3 SudokuMain.hs #-O2 improves performance x5`

$ `cd Haskell && ghc -O2 -optc-O3 -threaded SudokuParallel.hs && ./SudokuParallel +RTS -N<Number of cores> -RTS`

Go and Rust implementations use command line arguments, use those to solve different sudokus
  * -bench: Solve the hard-coded sudoku, for benchmarking purposes
  * -info: Provide stats about the performance, such as time, nodes and changes
  * -pretty: Print the solved sudoku in a human readable way, otherwise it is printed as a string of numbers, it is user to parse in another program

**Use example**:

Use Rust / Go implementation, since they are the most active and have cmd line args, Rust without parallelization is the fastest and most stable. Run the binaries or execute:

$ `rustc -O "dir/Rust/Sudoku.rs" && ./Sudoku -info -pretty 00200..00013 014300..58900` #Preferred

$ `go install "dir/Go/Sudoku.go" && $GOBIN/Sudoku -test -info -pretty` #Benchmarking

$ `go install "dir/Go/Sudoku.go" && $GOBIN/Sudoku -info 00200..00013 014300..58900` #Solve multiple, pass each sudoku as a string of numbers with 0s for blank tiles, each sudoku has to be 81 characters long


If there is no `-test` flag or sudokus the program will terminate. Illegal sudokus will be printed as 0000..0000

### Results
This are measured on my old laptop (2 cores @ 1.33 GHz), solving the 17-clue proper sudoku which is hard coded. The proportions should stay the same regardless of the device
(P stands for parallel)

  * **GO**     -> 120ms, 134%
  * **Rust**   -> 80ms, 89%
  * **Rust P** -> 85ms, 95%
  * **C**      -> 89ms, 100%
  * **Python2** -> 8.9s, 10100%
  * **Python3** -> 11.0s, 12395%
  * **Cython** -> 1.2s, 1348%
  * **Haskell** -> 7.2s, 8089%
  * **Haskell P** -> 6.2s, 6966%

### Algorithm
  The possible values for each square are the intersection of the possible values in its row / col / sqr

  1. Generate 3 arrays with the possible values for each row / col / square. (An empty row should have all possible values and a full one, zero)

  2. Set all values which are forced (only have 1 possibility)

  3. If there are no remeaning empty tiles: END

  4. Else: Select a tile and, for each value, try to solve the sudoku by making a duplicates and assigning one of the possible values, goto (2.)

  This algorithm is equivalent to traversing a tree in a DFS fashion, the input sudoku is the root and the leafs are solved sudokus or impossible sudokus, each branch is a choice, when there is only 1 branch it is forced, when there are more it is needed to traverse first one and then the other.

### Notes
  * In order to have a unique solution (proper sudoku), a sudoku has to have at least 17 clues (Having 17 clues does not imply it is a proper sudoku).

  * Solving sudokus benefits greatly from destructive writing into arrays, which is the opposite of what Haskell is used for (it can be done using the ST monad, generating messy and suboptimal code). The Haskell implementation involves a lot of unnecessary copying / writing.

  * Python2(.7) is significantly faster than Python3(.5/.7). This is due to Py3 using long integers, and the abundant use of integers in the sudoku

  * Concurrency isn't always an improvement: in Go / Haskell it yields better results, in Python the same, and worse in Rust.

  * Some languages, such as C, benefit from using 64bit-integers while others, such as Rust, don't and even yield worse performance. This may be due to the native size of the CPU and being able to fit more in the cache making up for eachother

### Thanks
  Thanks to [BartMassey](https://github.com/BartMassey) for the popcount algorithms and [tempate](https://github.com/tempate) for the idea