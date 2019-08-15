//To compile the fastest version call $ rustc -O -C lto /.../Sudoku.rs

#![allow(dead_code)]

type Value = u32;
type Pair = (bool, Sudoku);

const R: Value = 3;
const S: Value = 9;
const ALL: Value = (1 << S) - 1;
const R_U: usize = R as usize;
const S_U: usize = S as usize;
const SS: usize = S_U * S_U;

static mut NODES: u64 = 0;
static mut CHANGES: u64 = 0;

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
    remeaning: Vec<usize>,
}

impl Sudoku {
    //test: 024000000000007100090000000000000084000075000600030000000400029000200300100000000
    fn parse(s: &str) -> Sudoku {
        let mut from_s: [Value; SS] = [0; SS];
        let mut v: Vec<usize> = Vec::with_capacity(SS);

        let mut i: usize = 0;
        for ch in s.chars() {
            let val = ch.to_digit(10).unwrap();
            if val == 0 {
                v.push(i)
            } else {
                from_s[i] = 1 << (val - 1);
            }
            i += 1
        }

        v.shrink_to_fit();

        Sudoku {
            board: from_s,
            cols: [ALL; S_U],
            rows: [ALL; S_U],
            sqrs: [ALL; S_U],
            remeaning: v,
        }
    }

    fn solve_from_str(s: &str, pretty: bool) {
        let s = Sudoku::parse(s);

        let res = Sudoku::start(s);
        println!("{}", res.print_sudoku(pretty));
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

    fn set_all_forced(&mut self) -> bool {
        let mut last_updated = true;

        while last_updated {
            last_updated = false;
            let mut next_remeaning: Vec<usize> = Vec::with_capacity(self.remeaning.len());

            for index in self.remeaning.iter() {
                let (i, j) = coord(*index);
                let available = self.rows[i] & self.cols[j] & self.sqrs[get_sqr_index(i, j)];

                if available == 0 {
                    return false;
                }
                if is_pow_2(available) {
                    self.board[*index] = available;

                    let mask = !available;

                    self.rows[i] &= mask;
                    self.cols[j] &= mask;
                    self.sqrs[get_sqr_index(i, j)] &= mask;

                    last_updated = true;

                    unsafe {
                        CHANGES += 1;
                    }
                } else {
                    next_remeaning.push(*index);
                }
            }
            if last_updated {
                self.remeaning = next_remeaning;
            }
        }

        true
    }

    fn solve(mut s: Sudoku) -> Pair {
        unsafe {
            NODES += 1;
        }

        let is_pos = s.set_all_forced();

        if !is_pos {
            return (false, s);
        } else if s.remeaning.len() == 0 {
            return (true, s);
        } else {
            let index = s.remeaning.pop().expect("NO ELEMS");
            let mut possible = s.possible(index);

            while possible != 0 {
                let val = possible & (!possible + 1);
                possible &= possible - 1;
                let mut new_sud = s.clone();

                new_sud.board[index] = val;
                new_sud.update(index);

                let (pos, res) = Sudoku::solve(new_sud);
                if pos {
                    return (true, res);
                }
            }
        }

        (false, s)
    }

    fn start(mut s: Sudoku) -> Sudoku {
        use std::sync::mpsc;
        use std::thread;

        s.set_possible();
        let is_pos = s.set_all_forced();

        if !is_pos {
            return s;
        } else if s.remeaning.len() == 0 {
            return s;
        } else {

            let index = s.remeaning.pop().expect("NO ELEMS");
            let mut possible = s.possible(index);
            let mut children = Vec::with_capacity(pop_count(possible) as usize);

            while possible != 0 {
                let val = possible & (!possible + 1);
                possible &= possible - 1;
                let mut new_sud = s.clone();

                new_sud.board[index] = val;
                new_sud.update(index);

                children.push(thread::spawn(move || Sudoku::solve(new_sud)));
            }

            //Collect the results
            for child in children {
                let (b, sud) = child.join().unwrap();
                if b {
                    return sud;
                }
            }
        }

        Sudoku {
            board: [0; SS],
            cols: [0; S_U],
            rows: [0; S_U],
            sqrs: [0; S_U],
            remeaning: Vec::new(),
        }
    }
}

/*--------------------------MAIN-----------------------------*/

const HELP_TXT: &str = 
"This is a Sudoku solver made by J
Usage: ./Sudoku [ARGS] [SUDOKUS]...

  -h, --help    Print help
  -info         Provide information about the difficulty and time
  -pretty       Draw the sudoku in a human readable way
  -bench        Solve the benchmark sudoku, call with -info

Each sudoku has to be a string of 81 numbers, 0s for blank tiles 00021..31000";

fn main() {
    use std::time::Instant;
    let now = Instant::now();

    let args = std::env::args();
    let mut info = false;
    let mut pretty = false;

    if args.len() <= 1 {
        println!("Type -h for help");
    }

    for arg in args {
        match arg.as_ref() {
            "-h" | "--help" => println!("{}", HELP_TXT),
            "-info" => info = true,
            "-pretty" => pretty = true,
            "-bench" => Sudoku::solve_from_str(
                "024000000000007100090000000000000084000075000600030000000400029000200300100000000",
                pretty,
            ),
            a => {
                if a.len() == SS {
                    Sudoku::solve_from_str(a, pretty);
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

        println!("Time: {}ms", now.elapsed().subsec_millis());
    }
}
