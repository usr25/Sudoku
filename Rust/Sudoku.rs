//To compile the fastest version call $ rustc -O -C lto /.../Sudoku.rs

type Value = u32;

const R: Value = 3;
const S: Value = 9;
const ALL: Value = (1 << S) - 1;
const R_U: usize = R as usize;
const S_U: usize = S as usize;
const SS: usize = S_U * S_U;

static mut NODES: u64 = 0;
static mut CHANGES: u64 = 0;
static mut POSSIBLE: [u128; 81] = [0; 81];
static mut IS_TH: bool = false;

/*--------------------------HELPERS-----------------------------*/

#[inline(always)]
fn pop_count(n: u32) -> u32 {
    let y = (n >> 1) & 0o33333333333;
    let z = n - y - ((y >> 1) & 0o33333333333);

    ((z + (z >> 3)) & 0o30707070707) % 63
}

fn log2(mut v: Value) -> u8 {
    if v == 0 {
        0
    } else {
        let mut log = 0u8;
        while v != 1 {
            log += 1;
            v >>= 1;
        }
        1 + log
    }
}
#[inline(always)]
fn is_pow_2(v: Value) -> bool {
    v & (v - 1) == 0
}
#[inline(always)]
fn index(i: usize, j: usize) -> usize {
    S_U * i + j
}
#[inline(always)]
fn coord(i: usize) -> (usize, usize) {
    (i / S_U, i % S_U)
}
#[inline(always)]
fn get_sqr_index(i: usize, j: usize) -> usize {
    R_U * (i / R_U) + (j / R_U)
}

/*--------------------------SUDOKU-----------------------------*/

#[derive(Clone)]
struct Sudoku {
    board: [Value; SS],
    cols: [Value; S_U],
    rows: [Value; S_U],
    sqrs: [Value; S_U],
    remeaning: u128,
}

impl Sudoku {
    //test: 024000000000007100090000000000000084000075000600030000000400029000200300100000000
    fn parse(s: &str) -> Sudoku {
        let mut from_s: [Value; SS] = [0; SS];

        let mut i: usize = 0;
        let mut remeaning: u128 = 0;

        for ch in s.chars() {
            if ch == '.' {
                remeaning |= 1u128 << i;
            } else {
                let val = ch.to_digit(10).unwrap();
                if val == 0 {
                    remeaning |= 1u128 << i;
                } else {
                    from_s[i] = 1 << (val - 1);
                }
            }

            i += 1;
        }

        Sudoku {
            board: from_s,
            cols: [ALL; S_U],
            rows: [ALL; S_U],
            sqrs: [ALL; S_U],
            remeaning,
        }
    }

    fn solve_from_str(s: &str, pretty: bool, parallel: bool, print: bool) {
        let mut s = Sudoku::parse(s);
        unsafe {IS_TH = s.is_top_heavy();}

        let (_, res) = if parallel {
            Sudoku::launch_parallel(s)
        } else {
            s.set_possible();
            Sudoku::solve(s, SS+1)
        };
        if print {
            println!("{}", res.print_sudoku(pretty));
        }
    }

    fn print_sudoku(&self, pretty: bool) -> String {
        let mut string = String::with_capacity(SS * 2);
        if !pretty {
            for i in 0..SS {
                string.push((log2(self.board[i]) as u8 + '0' as u8) as char);
            }
            return string;
        }
        string.push_str("--------------------\n");

        for i in 0..S_U {
            for j in 0..S_U {
                if self.board[index(i, j)] == 0 {
                    string.push_str("- ");
                } else {
                    let borrowed = log2(self.board[index(i, j)]).to_string();
                    string.push_str(&borrowed);
                    string.push(' ');
                }

                if j % R_U == R_U - 1 {
                    string.push(' ');
                }
            }
            string.push('\n');
            if i % R_U == R_U - 1 && i != S_U - 1 {
                string.push('\n');
            }
        }
        string.push_str("--------------------");

        string
    }

    fn filled_squares(&self) -> usize {
        let mut t: usize = 0;
        for i in 0..SS {
            if self.board[i] != 0 {
                t += 1;
            }
        }
        t
    }

    fn is_valid(&self) -> bool {
        if self.filled_squares() != SS {
            return false;
        }

        let mut sum;

        //Rows
        for i in 0..S_U {
            sum = 0;
            for j in 0..S_U {
                if sum & self.board[index(i, j)] != 0 {
                    return false;
                }
                sum += self.board[index(i, j)];
            }

            if sum != ALL {
                return false;
            }
        }

        //Cols
        for i in 0..S_U {
            sum = 0;
            for j in 0..S_U {
                if sum & self.board[index(j, i)] != 0 {
                    return false;
                }
                sum += self.board[index(j, i)];
            }

            if sum != ALL {
                return false;
            }
        }

        //Sqrs
        for x in 0..R_U {
            for y in 0..R_U {
                sum = 0;
                for i in 0..R_U {
                    for j in 0..R_U {
                        if sum & self.board[index(R_U * x + i, R_U * y + j)] != 0 {
                            return false;
                        }
                        sum += self.board[index(R_U * x + i, R_U * y + j)];
                    }
                }
                if sum != ALL {
                    return false;
                }
            }
        }

        true
    }

    fn is_top_heavy(&self) -> bool {
        let mask = 0x1ffffffffff;
        (self.remeaning & mask).count_ones() < (self.remeaning & (mask << 40)).count_ones()
    }

    #[inline(always)]
    fn possible(&self, index: usize) -> Value {
        let i = index / S_U;
        let j = index % S_U;

        self.rows[i] & self.cols[j] & self.sqrs[get_sqr_index(i, j)]
    }

    fn set_possible(&mut self) {
        let mut val;

        for i in 0..S_U {
            for j in 0..S_U {
                val = self.board[index(i, j)];
                self.rows[i] ^= val;
                self.cols[j] ^= val;
                self.sqrs[get_sqr_index(i, j)] ^= val;
            }
        }
    }

    fn update(&mut self, index: usize) {
        let (i, j) = coord(index);
        let mask = !self.board[index];

        self.rows[i] &= mask;
        self.cols[j] &= mask;
        self.sqrs[get_sqr_index(i, j)] &= mask;
    }

    fn set_forced_prev(&mut self, prev: usize) -> bool {
        let mut temp = self.remeaning & unsafe {POSSIBLE[prev]};
        while temp != 0 {
            let index = temp.trailing_zeros() as usize;
            temp &= temp - 1;
            let (i, j) = coord(index);
            let available = self.rows[i] & self.cols[j] & self.sqrs[get_sqr_index(i, j)];

            if available == 0 {
                return false;
            }
            if is_pow_2(available) {
                self.board[index] = available;

                let mask = !available;

                self.rows[i] &= mask;
                self.cols[j] &= mask;
                self.sqrs[get_sqr_index(i, j)] &= mask;

                self.remeaning ^= 1u128 << index;
                temp |= unsafe {POSSIBLE[index] & self.remeaning};
                unsafe {CHANGES += 1;}
            }
        }

        true
    }

    fn set_all_forced(&mut self) -> bool {
        let mut temp = self.remeaning;
        while temp != 0 {
            let index = temp.trailing_zeros() as usize;
            temp &= temp - 1;
            let (i, j) = coord(index);
            let available = self.rows[i] & self.cols[j] & self.sqrs[get_sqr_index(i, j)];

            if available == 0 {
                return false;
            }
            if is_pow_2(available) {
                self.board[index] = available;

                let mask = !available;

                self.rows[i] &= mask;
                self.cols[j] &= mask;
                self.sqrs[get_sqr_index(i, j)] &= mask;

                self.remeaning ^= 1u128 << index;
                temp |= unsafe {POSSIBLE[index] & self.remeaning};
                unsafe {CHANGES += 1;}
            }
        }

        true
    }

    fn launch_parallel(mut s: Sudoku) -> (bool, Sudoku) {
        use std::thread;

        s.set_possible();
        let is_possible = s.set_all_forced();

        if !is_possible {
            return (false, s);
        } else if s.remeaning == 0 {
            return (true, s);
        } else {
            let index =
            if unsafe {IS_TH} {
                s.remeaning.trailing_zeros() as usize
            } else {
                (127 - s.remeaning.leading_zeros()) as usize
            };
            s.remeaning ^= 1u128 << index;
            let mut possible = s.possible(index);
            let mut children = Vec::with_capacity(pop_count(possible) as usize);

            while possible != 0 {
                let val = possible & (!possible + 1);
                possible &= possible - 1;
                let mut new_sud = s.clone();

                new_sud.board[index] = val;
                new_sud.update(index);

                children.push(thread::spawn(move || Sudoku::solve(new_sud, index)));
            }

            //Collect the results
            for child in children {
                let (b, sud) = child.join().unwrap();
                if b {
                    return (true, sud);
                }
            }
        }

        (false, s)
    }

    fn solve(mut s: Sudoku, prev: usize) -> (bool, Sudoku) {
        unsafe {NODES += 1;}

        if s.remeaning == 0 {
            return (true, s);
        }

        let is_possible = if prev >= SS {
            s.set_all_forced()
        } else {
            s.set_forced_prev(prev)
        };

        if !is_possible {
            return (false, s);
        } else if s.remeaning == 0 {
            return (true, s);
        } else {
            let index =
            if unsafe {IS_TH} {
                s.remeaning.trailing_zeros() as usize
            } else {
                (127 - s.remeaning.leading_zeros()) as usize
            };
            s.remeaning ^= 1u128 << index;
            let mut possible = s.possible(index);

            while possible != 0 {
                let val = possible & (!possible + 1);
                possible &= possible - 1;
                let mut new_sud = s.clone();

                new_sud.board[index] = val;
                new_sud.update(index);

                let (pos, res) = Sudoku::solve(new_sud, index);
                if pos {
                    return (true, res);
                }
            }
        }
        (false, s)
    }
}

fn read_from_file(path: &str, parallel: bool) -> std::io::Result<()> {
    use std::io::BufRead;

    //The sudokus have to be separated with a newline '\n'
    let file = std::fs::File::open(path)?;
    let reader = std::io::BufReader::new(file);
    for line in reader.lines() {
        Sudoku::solve_from_str(&line?, false, parallel, false);
    }

    Ok(())
}

unsafe fn populate_possible() {
    for i in 0..SS {
        let (row, col) = coord(i);
        let row = row as usize;
        let col = col as usize;
        let sqr = R_U * S_U * (row / R_U) + R_U * (col / R_U) as usize;
        for j in 0..S_U {
            POSSIBLE[i] |= 1u128 << (S_U * row + j);
            POSSIBLE[i] |= 1u128 << (col + S_U * j);
        }
        for j in 0..R_U {
            for k in 0..R_U {
                POSSIBLE[i] |= 1u128 << (sqr + S_U * j + k);
            }
        }
    }
}

/*--------------------------MAIN-----------------------------*/

const HELP_TXT: &str = 
"This is a Sudoku solver made by J
Usage: ./Sudoku [ARGS] [SUDOKUS]...

  -h, --help    Print help
  -p            Solve using parallelism [Unimplemented]
  -info         Provide information about the difficulty and time
  -pretty       Draw the sudoku in a human readable way
  -bench        Solve the benchmark sudoku, call with -info

  --/path       The path to a file separated by sudokus

Each sudoku has to be a string of 81 numbers, 0s or dots '.' to represent blank tiles 00021..31000";

fn main() {
    use std::time::Instant;
    unsafe {populate_possible()};
    let now = Instant::now();

    let args = std::env::args();
    let mut info = false;
    let mut pretty = false;
    let mut parallel = false;

    if args.len() == 1 {
        println!("Type -h for help");
    }

    for arg in args {
        match arg.as_ref() {
            "-h" | "--help" => println!("{}", HELP_TXT),
            "-info" => info = true,
            "-p" => parallel = true,
            "-pretty" => pretty = true,
            "-bench" => Sudoku::solve_from_str(
                "024000000000007100090000000000000084000075000600030000000400029000200300100000000",
                pretty,
                parallel,
                true,
            ),

            a => {
                if a.chars().next().unwrap() == '/' {
                    let _ = read_from_file(&a, parallel);
                } else if a.len() == SS {
                    Sudoku::solve_from_str(a, pretty, parallel, true);
                } else if !a.contains("Sudoku") {
                    println!(
                        "\'{}\' is an unknown command, type -h to see all commands",
                        a
                    );
                };
            }
        }
    }

    if info {
        unsafe {
            println!("Total changes: {:?}", CHANGES);
            println!("Total nodes:   {:?}", NODES);
        }

        println!("Time: {}ms", now.elapsed().as_millis());
    }
}
