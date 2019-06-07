//To compile the fastest version call $ rustc -O -C lto /.../Sudoku.rs

type Value = u32;

const R: Value = 3;
const S: Value = 9;
const ALL: Value = (1 << S) - 1;
const R_U: usize = R as usize;
const S_U: usize = S as usize;
const SS: usize = S_U*S_U;

static mut NODES: u64 =  0;
static mut CHANGES: u64 = 0;


/*--------------------------HELPERS-----------------------------*/

fn log2(mut v: Value) -> u8{
	if v == 0{
		0
	}else{
		let mut log = 0u8;
		while v != 1{
			log += 1;
			v >>=1;
		}
		1 + log
	}
}
#[inline(always)]
fn is_pow_2(v: Value) -> bool{
	(v != 0) && (v & (v - 1) == 0)
}
#[inline(always)]
fn index(i: usize, j: usize) -> usize{
	S_U * i + j
}
#[inline(always)]
fn coord(i: usize) -> (usize, usize){
	(i/S_U, i % S_U)
}
#[inline(always)]
fn get_sqr_index(i: usize, j: usize) -> usize{
	R_U*(i/R_U) + (j/R_U)
}


/*--------------------------SUDOKU-----------------------------*/

#[derive(Clone)]
struct Sudoku{
	board: [Value; SS],
	cols: [Value; S_U],
	rows: [Value; S_U],
	sqrs: [Value; S_U],
	remeaning: Vec<usize>,
}

impl Sudoku {
	fn print_sudoku(&self) -> String{
		let mut string = String::with_capacity(SS * 2);
		string.push_str("--------------------\n");

		for i in 0..S_U {
			for j in 0..S_U{
				if self.board[index(i, j)] == 0{
					string.push_str("- ");
				}else{
					let borrowed = log2(self.board[index(i, j)]).to_string();
					string.push_str(&borrowed);
					string.push(' ');
				}

				if j % R_U == R_U - 1{
					string.push(' ');
				}
			}
			string.push('\n');
			if i % R_U == R_U - 1 && i != S_U - 1{
				string.push('\n');
			}
		}
		string.push_str("--------------------");

		string
	}

	fn filled_squares(&self) -> usize{
		let mut t: usize = 0;
		for i in 0..SS {
			if self.board[i] != 0{
				t += 1;
			}
		}
		t
	}

	fn is_valid(&self) -> bool{
		if self.filled_squares() != SS{
			return false;
		}

		let mut sum;

		//Rows
		for i in 0..S_U {
			sum = 0;
			for j in 0..S_U {
				if sum & self.board[index(i, j)] != 0{
					return false;
				}
				sum += self.board[index(i, j)];
			}

			if sum != ALL{
				return false;
			}
		}

		//Cols
		for i in 0..S_U {
			sum = 0;
			for j in 0..S_U {
				if sum & self.board[index(j, i)] != 0{
					return false;
				}
				sum += self.board[index(j, i)];
			}

			if sum != ALL{
				return false;
			}
		}

		//Sqrs
		for x in 0..R_U {
			for y in 0..R_U {
				sum = 0;
				for i in 0..R_U {
					for j in 0..R_U{
						if sum & self.board[index(R_U*x + i, R_U*y + j)] != 0{
							return false;
						}
						sum += self.board[index(R_U*x + i, R_U*y + j)];
					}
				}
				if sum != ALL{
					return false;
				}
			}
		}

		true
	}

	fn possible(&self, index: usize) -> Value{
		let i = index / S_U;
		let j = index % S_U;

		self.rows[i] & self.cols[j] & self.sqrs[get_sqr_index(i, j)]
	}


	fn set_possible(&mut self){
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

	fn update(&mut self, index: usize){
		let (i, j) = coord(index);
		let mask = ALL ^ self.board[index];

		self.rows[i] &= mask;
		self.cols[j] &= mask;
		self.sqrs[get_sqr_index(i, j)] &= mask;
	}

	fn set_all_forced(&mut self) -> bool{
		let mut last_updated = true;
		
		while last_updated{
			last_updated = false;
			let mut next_remeaning: Vec<usize> = Vec::with_capacity(self.remeaning.len());

			for index in self.remeaning.iter() {
				let (i, j) = coord(*index);
				let available = self.rows[i] & self.cols[j] & self.sqrs[get_sqr_index(i, j)];

				if available == 0{
					return false;
				}
				if is_pow_2(available){
					self.board[*index] = available;

					let mask = ALL ^ available;

					self.rows[i] &= mask;
					self.cols[j] &= mask;
					self.sqrs[get_sqr_index(i, j)] &= mask;

					last_updated = true;

					unsafe{CHANGES += 1;}
				}else{
					next_remeaning.push(*index);
				}

			}
			if last_updated{
				self.remeaning = next_remeaning;
			}
		}

		true
	}

	fn solve(mut s: Sudoku) -> (bool, Sudoku){
		unsafe {NODES += 1;}

		if s.remeaning.len() == 0{
			return (true, s);
		}

		let is_pos = s.set_all_forced();

		if ! is_pos{
			return (false, s);
		}else if s.remeaning.len() == 0{
			return (true, s);
		}else{

			let index = s.remeaning.pop().expect("NO ELEMS");
			let possible = s.possible(index);
			let mut val: Value = 1;
			
			for _ in 0..S{
				if val & possible == 0{
					val <<= 1;
					continue
				}
				let mut new_sud = s.clone();

				new_sud.board[index] = val;
				new_sud.update(index);

				let (pos, res) = Sudoku::solve(new_sud);
				if pos{
					return (true, res);
				}

				val <<= 1;
			}
		}

		(false, s)
	}
}

/*-------------------------GENERATOR----------------------------*/

fn gen_sudoku() -> Sudoku{
	
	let s: [Value; SS] = 
		[0, 2, 4,  0, 0, 0,  0, 0, 0, 
		0, 0, 0,  0, 0, 7,  1, 0, 0,
		0, 9, 0,  0, 0, 0,  0, 0, 0,
		0, 0, 0,  0, 0, 0,  0, 8, 4,
		0, 0, 0,  0, 7, 5,  0, 0, 0,
		6, 0, 0,  0, 3, 0,  0, 0, 0,
		0, 0, 0,  4, 0, 0,  0, 2, 9,
		0, 0, 0,  2, 0, 0,  3, 0, 0,
		1, 0, 0,  0, 0, 0,  0, 0, 0]; //17
	/*
	let s: [Value; SS] =
		[0, 0, 3,  0, 2, 9,  6, 0, 8,
		6, 0, 0,  8, 0, 0,  0, 9, 0,
		0, 9, 5,  4, 0, 6,  0, 0, 7,
		1, 6, 0,  9, 3, 0,  8, 0, 4,
		4, 0, 7,  0, 8, 0,  9, 0, 6,
		9, 0, 8,  0, 4, 2,  0, 0, 0,
		3, 0, 0,  2, 0, 4,  0, 6, 0,
		0, 7, 0,  0, 0, 1,  0, 0, 5,
		5, 0, 9,  7, 6, 0,  3, 1, 2];
	*/
	let mut from_s: [Value;SS] = [0; SS];
	let mut v: Vec<usize> = Vec::with_capacity(SS);

	for i in 0..SS {
		if s[i] == 0{
			v.push(i);
		}else{
			from_s[i] = 1 << (s[i] - 1);
		}
	}

	v.shrink_to_fit();

	Sudoku{
		board: from_s, 
		cols: [ALL; S_U], 
		rows: [ALL; S_U], 
		sqrs: [ALL; S_U], 
		remeaning: v}
}


/*--------------------------MAIN-----------------------------*/

fn main() {
	let mut s = gen_sudoku();
	s.set_possible();

	println!("{}", s.print_sudoku());
	println!("{}, {}", s.is_valid(), s.filled_squares());
	
	let (_, res) = Sudoku::solve(s);

	println!("{}", res.print_sudoku());
	println!("{}, {}", res.is_valid(), res.filled_squares());
	
	unsafe{
		println!("Total changes: {:?}", CHANGES);
		println!("Total nodes:   {:?}", NODES);
	}
}