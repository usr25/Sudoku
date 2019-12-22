# Fast Sudoku Solver
Sudoku solving algorithm implemented in different languages to observe the differences among them and, specially, the performance.

O(_n_^_m_) algorithm, where _n_ is the number of empty tiles and _m_ the possible values (In this case, [1, 9] O(n^9)). Sudokus are an NP-Complete problem.

### Use
_Requires go, rustc, gcc, python2/3, Cython and ghc. Other compilers may yield different results._

There are compiled files in /Linux. `go run` has an overhead due to the compilation, use `go build` to generate the binary

cd into Sudoku/

$ `go run Go/Sudoku.go -h #If there is trouble with GOBIN, refer to the` [go installation guide](https://golang.org/doc/install)

$ `rustc -O dir/Rust/Sudoku.rs && ./Sudoku -h`

$ `gcc -O3 -std=c11 -faggressive-loop-optimizations C/Sudoku.c`

$ `python3 Python/sudoku.py #Can also be run with python2`

$ `python3 Python/compiler.py  #Requires Cython, check the` [official website](https://cython.org/)
0
$ `cd Haskell && ghc -O2 -optc-O3 SudokuMain.hs && ./SudokuMain`

$ `cd Haskell && ghc -O2 -optc-O3 -threaded SudokuMain.hs && ./SudokuMain p +RTS -N<Number of cores> -RTS` Use p as an argument for parallelism

Go and Rust implementations use command line arguments, use those to solve different sudokus
  * -bench: Solve the hard-coded sudoku, for benchmarking purposes
  * -info: Provide stats about the performance, such as time, nodes and changes
  * -p: In Rust, use parallelism
  * -pretty: Print the solved sudoku in a human readable way, otherwise it is printed as a string of numbers, it is user to parse in another program

**Use example**:

Use the Rust / Go implementation, since they are the most active and have cmd line args. Run the binaries or compile, then execute:

$ `./Sudoku -test -info -pretty` #Benchmarking

$ `./Sudoku -info 00200..00013 014300..58900` #Solve multiple, pass each sudoku as a string of numbers with 0s for blank tiles, each sudoku has to be 81 characters long, the sudokus are separated by spaces

If there is no `-test` flag or sudokus the program will terminate. Illegal sudokus will be printed as 0000..0000 or not printed at all

### Results
This are measured on my old laptop (2 cores @ 1.33 GHz), solving the 17-clue proper sudoku which is hard coded. The proportions should stay the same regardless of the device
Rust implements bitboards, making it even faster, results using vectors are in [brackets]. To use bb a language needs to have support for 128-bit ints.
P stands for parallel, lower is better

  * **GO**     -> 120ms, 141%
  * **Rust**   -> 54ms, 63%  [80ms last recorded score without 128bit bitboards]
  * **Rust P** -> 49ms, 57%  [69ms]
  * **C**      -> 85ms, 100%
  * **Python2** -> 8.9s, 10470%
  * **Python3** -> 11.0s, 12940%
  * **Cython** -> 1.2s, 1410%
  * **Haskell** -> 2.2s, 2588%
  * **Haskell P** -> 2.0s, 2352%

### Algorithm
  The possible values for each square are the intersection of the possible values in its row / col / sqr

  1. Generate 3 arrays with the possible values for each row / col / square. (An empty row should have all possible values and a full one, zero)

  2. Set all values which are forced (only have 1 possibility)

  3. If there are no remeaning empty tiles: END

  4. Else: Select a tile and, for each value, try to solve the sudoku by making a duplicate and assigning one of the possible values, goto (2.)

  This algorithm is equivalent to traversing a tree in a DFS fashion, the input sudoku is the root and the leafs are solved sudokus or impossible sudokus, each branch is a choice, when there is only 1 branch it is forced, when there are more it is needed to traverse first one and then the other

  The speed is improved using a struct to hold the remeaning empty tiles, such as a Vector or an Array, however, the best efficiency is achieved when representing the board as a bitboard, with 1s located where there are empty tiles and 0s elsewhere. Since a change in one tile only affects the neighbouring ones, there is no need to check the rest. This is achieved through the use of precalculated masks

### Notes
  * Due to Rust being so fast, the parallel version is very unstable, so the results have a higher standart derivation: reaching lows as 40ms but highs as 77ms

  * In order to have a unique solution (proper sudoku), a sudoku has to have at least 17 clues (Having 17 clues does not imply it is a proper sudoku). i.e. There are no proper sudokus with clues <= 16

  * Solving sudokus benefits greatly from destructive writing into arrays, which is the opposite of what Haskell is used for (it can be done using the ST monad, generating messy and suboptimal code). The Haskell implementation involves a lot of unnecessary copying / writing

  * Python2(.7) is significantly faster than Python3(.5/.7). This is due to Py3 using long integers, and the abundant use of integers in the sudoku

  * Concurrency yields different results based on the language

  * Some languages, such as C, benefit from using 64bit-integers while others, such as Rust, don't and even yield worse performance. This may be due to the native size of the CPU and being able to fit more in the cache making up for eachother

### Thanks
  Thanks to [BartMassey](https://github.com/BartMassey) for the popcount algorithms and [tempate](https://github.com/tempate) for the idea. I got the test sudokus from [sunjay](https://github.com/sunjay/sudoku)