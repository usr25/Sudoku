from multiprocessing import Pool

R = 3
S = 9
SS = S*S
ALL = (1 << S) - 1
      

class Sudoku(object):
    """This is the main class of the program.
    It stores the board, a list with all possible values for each
    row / col / sqr and a list of the remaning indices in the board.
    It stores each value as a power of 2, the posibles values are just
    |s of all the possibilities"""
    def __init__(self, _board, _rows, _cols, _sqrs, remeaning=None):
        self.board = _board
        self.rows = _rows
        self.cols = _cols
        self.sqrs = _sqrs

        if remeaning:
            self.remeaning = remeaning
        else:
            self.remeaning = [i for i in range(SS) if _board[i] == 0]

    def __repr__(self):
        """
        String representation of the sudoku
        """
        s = "--------------------\n"
        for i in range(S):
            s_small = ""
            for j in range(S):
                if self.board[index(i, j)] == 0:
                    s_small += "- "
                else:
                    s_small += str(log2(self.board[index(i, j)])) + " "

                if j % R == R - 1:
                    s_small += " "
            s += s_small + "\n"
            if i % R == R - 1 and i != S - 1:
                s += "\n"

        return s

    def duplicate(self):
        return Sudoku(list(self.board), list(self.rows), list(self.cols), list(self.sqrs), list(self.remeaning))

    def possible(self, index):
        """
        Returns the possible values for a tile
        """
        i, j = coord(index)
        return self.rows[i] & self.cols[j] & self.sqrs[get_sqr_index(i, j)]

    def update_one(self, index):
        """
        Removes the value in board[index] from the possible in its row / col / sqr
        """
        i, j = coord(index)
        self.rows[i] &= ALL ^ self.board[index]
        self.cols[j] &= ALL ^ self.board[index]
        self.sqrs[get_sqr_index(i, j)] &= ALL ^ self.board[index]
 

    def set_all_forced(self):
        """
        Sets all the tiles which have 1 possible value in one iteration throught the board
        Until there are no changes made
        """
        last_update = True
        while last_update:
            last_update = False
            next_rem = []

            for index in self.remeaning:
                i, j = coord(index)
                available = self.rows[i] & self.cols[j] & self.sqrs[get_sqr_index(i, j)]
                
                if not available:
                    return False

                if is_pow_2(available):
                    self.board[index] = available
                    self.rows[i] &= ALL ^ available
                    self.cols[j] &= ALL ^ available
                    self.sqrs[get_sqr_index(i, j)] &= ALL ^ available

                    last_update = True
                else:
                    next_rem.append(index)

            if last_update:
                self.remeaning = next_rem

        return True

    def is_finished(self):
        """
        Returns True if all the tiles are set, False otherwise
        """
        return not all(self.board)

def generate_sudoku():
    """
    Generates the sudoku, so far it is hard coded
    """
    board = list(map(get_pow, 
        [0, 2, 4,  0, 0, 0,  0, 0, 0, 
        0, 0, 0,  0, 0, 7,  1, 0, 0,
        0, 9, 0,  0, 0, 0,  0, 0, 0,
        0, 0, 0,  0, 0, 0,  0, 8, 4,
        0, 0, 0,  0, 7, 5,  0, 0, 0,
        6, 0, 0,  0, 3, 0,  0, 0, 0,
        0, 0, 0,  4, 0, 0,  0, 2, 9,
        0, 0, 0,  2, 0, 0,  3, 0, 0,
        1, 0, 0,  0, 0, 0,  0, 0, 0])) #17

    rows = [ALL] * S
    cols = [ALL] * S
    sqrs = [ALL] * S


    for i in range(S):
        for j in range(S):
            val = board[index(i, j)]
            rows[i] ^= val
            cols[j] ^= val
            sqrs[get_sqr_index(i, j)] ^= val

    return Sudoku(board, rows, cols, sqrs)

def log2(n):
    """
    Returns 1 + log2(n) or 0
    """    
    log = 0
    while n:
        log += 1
        n >>= 1

    return log

def get_pow(n):
    return 1 << (n - 1) if n else 0

def index(i, j):
    return S * i + j

def coord(i):
    return i // S, i % S

def get_sqr_index(i, j):
    return R*(i // R) + (j // R)

def is_pow_2(v):
    return (v & (v - 1)) == 0

def ls_of_possible(v):
    """
    Parses the possible values into a list of powers of 2
    """
    return [1<<i for i in range(S) if v & (1 << i)]

def solve(sud):
    """
    Main algorithm, tries to set all the forced values
    When that is no longer possible, it branches and checks if
    the resulting sudokus are solvable, it tries to reach as deep as
    possible in each branch 
    """
    is_pos = sud.set_all_forced()

    if not is_pos:
        return False, None
    elif sud.remeaning == []:
        return True, sud

    index = sud.remeaning.pop()

    for value in ls_of_possible(sud.possible(index)):
        new_sud = sud.duplicate()
        new_sud.board[index] = value
        new_sud.update_one(index)

        pos, new_sud = solve(new_sud)
        if pos:
            return True, new_sud

    return False, None

def start(sud):
    """
    Special version of solve() which takes advantage of multithreading
    It is only called once at the beggining of the program
    """
    is_pos = sud.set_all_forced()

    if not is_pos:
        return False, None
    elif sud.remeaning == []:
        return True, sud

    index = sud.remeaning[-1]

    for rem_index in sud.remeaning:
        tpc = bin(sud.possible(rem_index)).count('1')
        if tpc == 2:
            index = rem_index
            break
        elif tpc == 3:
            index = rem_index

    sud.remeaning.remove(index)

    pool = Pool(bin(sud.possible(index)).count('1'))
    ls = [(sud, v, index) for v in ls_of_possible(sud.possible(index))] 
    result = pool.map(get_nxt, ls)

    clean = [res for (b,res) in result if b]
    return clean[0] #There will always be a solution unless it is an impossible sudoku

def get_nxt(three):
    new_sud, value, index = three
    new_sud.board[index] = value
    new_sud.update_one(index)

    return solve(new_sud)

def main():
    sudoku = generate_sudoku()
    print(sudoku)

    res = start(sudoku)

    print(res)


if __name__ == '__main__':
    main()