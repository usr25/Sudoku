#include <stdio.h>
#include <stdbool.h>
#include <time.h>

#define R 3
#define S 9
#define ALL (1<<S) - 1
#define log_2(ll) __builtin_ctz(ll)


/*TO RUN:
 * Call with cmd
 * $ gcc -O3 -std=c11 -faggressive-loop-optimizations <file> -o <output file>.out
 * $ <output file>.out
 */

/*--------------------------INLINES--------------------------*/

static inline int POS(int x) { return 1 << (x-1); }
static inline int POW2(char pow) { return 1 << pow; }
static inline int GET_SQR(int i, int j) { return R * (int)(i/R) + (int)(j/R); }
static inline int GET_X(unsigned short c) { return (c) & 0xf; }
static inline int GET_Y(unsigned short c) { return ((c) & 0xf0) >> 4; }
static inline int GET_CRD(int i, int j) { return (i << 4) + j; }

/*------------------------PROTOTYPES------------------------*/

typedef struct {
    unsigned int values[S][S];
    
    unsigned int rowsPos[S];
    unsigned int colsPos[S];
    unsigned int sqrsPos[S];
} Board;

Board generateSudoku();
void printBoard(Board board);

void solve(Board *board);

bool dfs(Board *board, int index);
char setAllForced(Board *board, int min);

void calculatePossible(Board *board);
void updateTileAdded(Board *board, const char y, const char x);

int getNextPossibleValue(Board board, const int val, const char x, const char y);

int log_2(int index);

bool checkBoard(Board board);
bool completeBoard(Board board);
bool finishedBoard(Board board);

/*--------------------------GLOBAL--------------------------*/

unsigned int blanks[S*S];
int blanks_size = 0;

/*--------------------------BOARD---------------------------*/
Board generateSudoku(){
    unsigned int values[S][S] = {
        {0,2,4, 0,0,0, 0,0,0},
        {0,0,0, 0,0,7, 1,0,0},
        {0,9,0, 0,0,0, 0,0,0},

        {0,0,0, 0,0,0, 0,8,4},
        {0,0,0, 0,7,5, 0,0,0},
        {6,0,0, 0,3,0, 0,0,0},
        
        {0,0,0, 4,0,0, 0,2,9},
        {0,0,0, 2,0,0, 3,0,0},
        {1,0,0, 0,0,0, 0,0,0}
    };
    
    Board board = (Board) {
        .rowsPos = {0},
        .colsPos = {0},
        .sqrsPos = {0}
    };
    
    for (int i = 0; i < S; i++) {
        for (int j = 0; j < S; j++) {
            if (! values[i][j]){
                blanks[blanks_size++] = GET_CRD(i, j);
            }else{
                board.values[i][j] = POS(values[i][j]);
            }
        }
    }

    return board;
}

void printBoard(Board board) {
    printf("Board values: \n");
    printf("---------------------------\n");

    for (int i = 0; i < S; i++) {
        for (int j = 0; j < S; j++) {
            if (! board.values[i][j]) {
                printf(" - ");
            } else {
                printf(" %d ", log_2(board.values[i][j]) + 1);
            }
        }
        printf("\n");
    }
    
    printf("---------------------------\n");
}

/*--------------------------CHECK---------------------------*/

bool checkBoard(Board board) {
    int pos, row, col, sqr;
    
    // Checks to see if all rows match
    for (int i = 0; i < S; i++) {
        row = 0;
        for (int j = 0; j < S; j++) {
            if (board.values[i][j]) {
                pos = board.values[i][j];
                if (pos & row) return false;
                row |= pos;
            }
        }
    }
    
    // Checks to see if all columns match
    for (int i = 0; i < S; i++) {
        col = 0;
        for (int j = 0; j < S; j++) {
            if (board.values[j][i]) {
                pos = board.values[j][i];
                if (pos & col) return false;
                col |= pos;
            }
        }
    }
    
    // Checks to see if all squares match
    for (int i = 0; i < S; i += R) {
        for (int j = 0; j < S / R; j += R) {
            sqr = 0;
            for (int k = i; k < i + R; k++) {
                for (int l = j; l < j + R; l++) {
                    if (board.values[k][l]) {
                        pos = board.values[k][l];
                        if (pos & sqr) return false;
                        sqr |= pos;
                    }
                }
            }
        }
    }
    
    return true;
}

bool completeBoard(Board board) {
    for (int i = 0; i < S; i++) {
        for (int j = 0; j < S; j++) {
            if (board.values[i][j] < 1 || board.values[i][j] >= ALL) return false;
        }
    }
    
    return true;
}

bool finishedBoard(Board board) {
    return completeBoard(board) && checkBoard(board);
}

/*--------------------------SOLVER--------------------------*/

bool dfs(Board *board, int index) {
    Board new_board;
    int state, new_index;
    
    // If all values are set, the recursion is finished
    if (index >= blanks_size) return true;
    
    int x = GET_X(blanks[index]);
    int y = GET_Y(blanks[index]);
    
    // Goes to the next tile if the current one is already set
    if (board->values[y][x])
        return dfs(board, index+1);
    
    unsigned int possible = board->rowsPos[y] & board->colsPos[x] & board->sqrsPos[GET_SQR(y, x)];

    while (possible)
    {
        unsigned int v = possible & (-possible);
        possible &= possible - 1;
        // Generates a new copy of the board to improve performance
        new_board = *board;
        new_board.values[y][x] = v;
        new_index = index + 1;
        
        // Removes the picked value from the possible ones
        updateTileAdded(&new_board, y, x);
        
        // Sets all the possible forced tiles
        do {
            state = setAllForced(&new_board, new_index);
        } while (state == 1);
        
        
        if (state != 2 && dfs(&new_board, new_index)) {
            *board = new_board;
            return true;
        }
    }

    // There was no possible value to be chosen
    return false;
}

/* Calculates all the possible values for each row, column, and square
 */
void calculatePossible(Board *board) {
    unsigned short pos;
    int m;
    
    // Goes tile by tile and saves all the found values
    for (int i = 0; i < S; i++) {
        for (int j = 0; j < S; j++) {
            if (board->values[i][j]) {
                m = GET_SQR(i,j);
                pos = board->values[i][j];
                
                board->rowsPos[i] |= pos;
                board->colsPos[j] |= pos;
                board->sqrsPos[m] |= pos; 
            }
        }
    }
    
    for (int k = 0; k < S; k++) {
        board->rowsPos[k] ^= ALL;
        board->colsPos[k] ^= ALL;
        board->sqrsPos[k] ^= ALL;
    }
}


/* Updates the possible values for the row, column, and square of the 
 * tile that was changed.
 */
void updateTileAdded(Board *board, const char y, const char x) {
    int val = ~board->values[y][x];
    
    board->rowsPos[y] &= val;
    board->colsPos[x] &= val;
    board->sqrsPos[GET_SQR(y,x)] &= val;
}

/* Sets all the tiles that only have one possible value.
 * 
 * Returns:
 *  1 if a modification was made
 *  2 if there is an empty tile with no possible value
 *  any other value if no modifications were made
 */
char setAllForced(Board *board, int min) {
    int x, y;
    int possible;
    char state = 0;

    for (int i = min; state != 2 && i < blanks_size; i++) {
        x = GET_X(blanks[i]);
        y = GET_Y(blanks[i]);
        
        if (! board->values[y][x]) {
        	possible = board->rowsPos[y] & board->colsPos[x] & board->sqrsPos[GET_SQR(y, x)];
            state = 2 - __builtin_popcount(possible);

            if (state == 1) {
                board->values[y][x] = possible;
                updateTileAdded(board, y, x);
            }
        }
    }
    
    return state;
}

void solve(Board *board) {
    calculatePossible(board);
    
    dfs(board, 0);
}


int main(int argc, char const *argv[])
{
    Board b = generateSudoku();
    printBoard(b);
 
    time_t start = clock();
    
    solve(&b);

    time_t end = clock();


    printBoard(b);
    printf("Is valid: "); printf((finishedBoard(b)) ? "true\n" : "false\n");
    printf("Time: %ldms\n", (end - start)/1000);

    return 0;
}