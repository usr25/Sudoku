cdef int R = 3
cdef int S = 9
cdef int SS = S*S
cdef int ALL = (1 << S) - 1


cdef class Sudoku(object):

    cdef public list board, rows, cols, sqrs, remeaning

    def __init__(self, list _board, list  _rows, list _cols, list _sqrs, list remeaning=None):
        self.board = _board
        self.rows = _rows
        self.cols = _cols
        self.sqrs = _sqrs

        if remeaning:
            self.remeaning = remeaning
        else:
            self.remeaning = [i for i in range(SS) if not self.board[i]]

    def duplicate(self):
        return Sudoku(list(self.board), list(self.rows), list(self.cols), list(self.sqrs), list(self.remeaning))

    def __repr__(self):
        cdef str s = "--------------------\n"
        cdef str s_small
        cdef int i, j

        for i in range(S):
            s_small = ""
            for j in range(S):
                if self.board[index(i, j)] == 0:
                    s_small += "- "
                else:
                    s_small += str(base10(self.board[index(i, j)])) + ' '

                if j % R == R - 1:
                    s_small += ' '
            s += s_small + '\n'
            if i % R == R - 1 and i != S - 1:
                s += '\n'

        return s

    def possible(self, const int index):
        cdef int i, j
        i, j = coord(index)
        return self.rows[i] & self.cols[j] & self.sqrs[get_sqr_index(i, j)]

    def update_one(self, const int index):
        cdef int i, j
        i, j = coord(index)
        self.rows[i] &= ALL ^ self.board[index]
        self.cols[j] &= ALL ^ self.board[index]
        self.sqrs[get_sqr_index(i, j)] &= ALL ^ self.board[index]   

    def set_all_forced(self):
        cdef int last_update = 1
        cdef list next_rem
        cdef int index, i, j, available, mask

        while last_update:
            last_update = 0
            next_rem = []
            for index in self.remeaning:
                i, j = coord(index)
                available = self.rows[i] & self.cols[j] & self.sqrs[get_sqr_index(i, j)]
                
                if not available:
                    return False

                if is_pow_2(available):
                    mask = ALL ^ available
                    
                    self.board[index] = available
                    self.rows[i] &= mask
                    self.cols[j] &= mask
                    self.sqrs[get_sqr_index(i, j)] &= mask

                    last_update = 1
                else:
                    next_rem.append(index)

            if last_update:
                self.remeaning = next_rem
        return True

    def is_finished(self):
        return not all(self.board)

cdef Sudoku generate_sudoku():
    
    cdef list board = list(map(get_pow, 
        [0, 2, 4,  0, 0, 0,  0, 0, 0, 
        0, 0, 0,  0, 0, 7,  1, 0, 0,
        0, 9, 0,  0, 0, 0,  0, 0, 0,
        0, 0, 0,  0, 0, 0,  0, 8, 4,
        0, 0, 0,  0, 7, 5,  0, 0, 0,
        6, 0, 0,  0, 3, 0,  0, 0, 0,
        0, 0, 0,  4, 0, 0,  0, 2, 9,
        0, 0, 0,  2, 0, 0,  3, 0, 0,
        1, 0, 0,  0, 0, 0,  0, 0, 0])) #17
    """
    board = list(map(get_pow, 
        [0, 0, 3,  0, 2, 9,  6, 0, 8,
        6, 0, 0,  8, 0, 0,  0, 9, 0,
        0, 9, 5,  4, 0, 6,  0, 0, 7,
        1, 6, 0,  9, 3, 0,  8, 0, 4,
        4, 0, 7,  0, 8, 0,  9, 0, 6,
        9, 0, 8,  0, 4, 2,  0, 0, 0,
        3, 0, 0,  2, 0, 4,  0, 6, 0,
        0, 7, 0,  0, 0, 1,  0, 0, 5,
        5, 0, 9,  7, 6, 0,  3, 1, 2]))
    """

    cdef list rows = [ALL] * S
    cdef list cols = [ALL] * S
    cdef list sqrs = [ALL] * S

    cdef int i, j, val
    for i in range(S):
        for j in range(S):
            val = board[index(i, j)]
            rows[i] ^= val
            cols[j] ^= val
            sqrs[get_sqr_index(i, j)] ^= val

    return Sudoku(board, rows, cols, sqrs)

cdef int base10(int n):    
    cdef int log = 0
    while n:
        log += 1
        n >>= 1

    return log

cdef inline int get_pow(const int n):
    return 1 << (n - 1) if n else 0

cdef inline int index(const int i, const int j):
    return S * i + j

cdef inline tuple coord(const int i):
    return i // S, i % S

cdef inline int get_sqr_index(const int i, const int j):
    return R*(i // R) + (j // R)

cdef inline int is_pow_2(const int v):
    return (v & (v - 1)) == 0

cdef inline list ls_of_possible(const int v):
    return [1<<i for i in range(S) if v & (1 << i)]

cdef tuple solve(sud):    
    cdef int is_pos = sud.set_all_forced()

    if not is_pos:
        return False, None
    elif sud.remeaning == []:
        return True, sud

    cdef int index = sud.remeaning.pop()
    cdef int value
    cdef Sudoku new_sud


    for value in ls_of_possible(sud.possible(index)):
        new_sud = sud.duplicate()
        new_sud.board[index] = value
        new_sud.update_one(index)

        pos, new_sud = solve(new_sud)
        if pos:
            return True, new_sud

    return False, None

def main():
    cdef Sudoku sudoku
    sudoku = generate_sudoku()
    print(sudoku)
    _, sudoku = solve(sudoku)
    print(sudoku)


if __name__ == '__main__':
    main()