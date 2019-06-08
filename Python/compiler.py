import pyximport; pyximport.install(pyimport= True, build_dir = "/home/usr/Desktop")

import Sudoku
print("Finished compiling\n\t---===---")

from time import perf_counter

beg = perf_counter()

Sudoku.main()

print(perf_counter() - beg)