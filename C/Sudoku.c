#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <getopt.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#define R 3
#define S 9
#define log_2(ll) __builtin_ctz(ll)

const unsigned int ALL = (1<<S)-1;

/*TO RUN:
 * Call with cmd
 * $ gcc -O3 -std=c11 -faggressive-loop-optimizations <file> -o <output file>.out
 * $ <output file>.out
 */

/*--------------------------INLINES--------------------------*/

static inline int POS(int x) { return 1 << (x-1); }
static inline int POW2(int pow) { return 1 << pow; }
static inline int GET_SQR(int i, int j) { return R * (int)(i/R) + (int)(j/R); }
static inline int GET_X(unsigned short c) { return (c) & 0xf; }
static inline int GET_Y(unsigned short c) { return ((c) & 0xf0) >> 4; }
static inline int GET_CRD(int i, int j) { return (i << 4) + j; }
static inline int IS_POW_2(int p) { return p && ((p & (p-1)) == 0); }

/*------------------------PROTOTYPES------------------------*/

typedef struct {
    unsigned int values[S][S];

    unsigned int rowsPos[S];
    unsigned int colsPos[S];
    unsigned int sqrsPos[S];

    unsigned int blanksSize;
} Board;

Board generateSudoku();
void printBoard(Board board);

void solve(Board *board);

bool dfs(Board *board, int index);
int setAllForced(Board *board, int min);

void calculatePossible(Board *board);
void updateTileAdded(Board *board, const int y, const int x);

int getNextPossibleValue(Board board, const int val, const int x, const int y);

int log_2(int index);

bool checkBoard(const Board board);
bool completeBoard(const Board board);
bool finishedBoard(const Board board);

/*--------------------------GLOBAL--------------------------*/

const char defaultSudoku[S*S+1] = "024000000000007100090000000000000084000075000600030000000400029000200300100000000";

unsigned int blanks[S*S];
int beautifyFlag = 0;
int logFlag = 0;

/*--------------------------BOARD---------------------------*/

void printBoard(Board board) {
    if (beautifyFlag)
    {
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
    else
    {
        char* c = (char*)malloc((S*S+1)*sizeof(char));
        c[S*S] = '\0';
        int ci = 0;
        if (c == NULL)
            exit(2);
        for (int i = 0; i < S; i++) {
            for (int j = 0; j < S; j++) {
                if (! board.values[i][j]) {
                    c[ci] = '0';
                } else {
                    c[ci] = '1' + log_2(board.values[i][j]);
                }
                ci++;
            }
        }
        assert(c[S*S] == '\0');
        printf("%s\n", c);

        free(c);
    }
}

/*--------------------------CHECK---------------------------*/

bool checkBoard(const Board board) {
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

bool completeBoard(const Board board) {
    for (int i = 0; i < S; i++) {
        for (int j = 0; j < S; j++) {
            if (board.values[i][j] < 1 || board.values[i][j] >= ALL) return false;
        }
    }

    return true;
}

bool finishedBoard(const Board board) {
    return completeBoard(board) && checkBoard(board);
}

/*--------------------------SOLVER--------------------------*/

bool dfs(Board *board, int index) {
    Board new_board;
    int state, new_index;

    // If all values are set, the recursion is finished
    if (index < 0) return true;

    int x = GET_X(blanks[index]);
    int y = GET_Y(blanks[index]);

    // Goes to the next tile if the current one is already set
    if (board->values[y][x])
        return dfs(board, index - 1);

    unsigned int possible = board->rowsPos[y] & board->colsPos[x] & board->sqrsPos[GET_SQR(y, x)];

    while (possible)
    {
        unsigned int v = possible & -possible;
        possible &= possible - 1;
        // Generates a new copy of the board to improve performance
        new_board = *board;
        new_board.values[y][x] = v;
        new_index = index - 1;

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
inline void updateTileAdded(Board *board, const int y, const int x) {
    const int mask = ~board->values[y][x];

    board->rowsPos[y] &= mask;
    board->colsPos[x] &= mask;
    board->sqrsPos[GET_SQR(y,x)] &= mask;
}

/* Sets all the tiles that only have one possible value.
 * 
 * Returns:
 *  1 if a modification was made
 *  2 if there is an empty tile with no possible value
 *  any other value if no modifications were made
 */
int setAllForced(Board *board, int max) {
    int x, y;
    int possible;
    int state = 0;

    for (int i = max; state != 2 && i >= 0; i--) {
        x = GET_X(blanks[i]);
        y = GET_Y(blanks[i]);

        if (!board->values[y][x]) {
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
    setAllForced(board, 0);
    dfs(board, 80);
}


Board parseSudoku(const char* sudStr)
{
    Board s = (Board){
        .rowsPos = {0},
        .colsPos = {0},
        .sqrsPos = {0},
        .blanksSize = 0
    };

    int i = 0, val, x,y;
    char curr;

    while (i < S*S && ((curr = sudStr[i]) != '\0'))
    {
        if (curr == ' ') continue;
        if (curr == '-') curr = '0';

        val = (int)(curr - '0');
        if (val < 0 || val > 9)
        {
            printf("[-] Unknown char: %c\n", curr);
            exit(1);
        }

        x = i % S;
        y = i / S;

        if (! val){
            blanks[s.blanksSize++] = GET_CRD(y, x);
            s.values[y][x] = 0;
        }else{
            s.values[y][x] = POS(val);
        }

        i++;
    }

    if (i < S*S)
    {
        fprintf(stderr, "\'%s\' is less than %d chars long\n", sudStr, S*S);
        exit(1);
    }

    return s;
}

void solveFromStr(const char* sudoku)
{
    Board b = parseSudoku(sudoku);

    solve(&b);

    printBoard(b);
    if (logFlag)
    {
        printf("Is valid: "); printf((finishedBoard(b)) ? "true\n" : "false\n");
    }
}


void readMultipleSudokus(FILE* txt)
{
    char buffer[S*S+1];
    char separator[1];

    buffer[S*S] = '\0';

    //We read 1 sudoku and 1 separator at a time
    while (fread(buffer, sizeof(char), S*S, txt) == S*S)
    {
        assert(buffer[S*S] == '\0');
        solveFromStr(buffer);
        //Read the separator
        if (fread(separator, sizeof(char), 1, txt) != 1)
        {
            fprintf(stderr, "Ensure that the sudokus take up %d chars and are separated by 1 char that isnt a number or -\n", S*S);
            exit(4);
        }
        //Ooops, the separator is a number, so the sudokus may be in the wrong format
        //Just in case, exit with an error
        if (separator[0] == '-' || (separator[0] >= '0' && separator[0] <= '9'))
        {
            fprintf(stderr, "Ensure that the sudokus take up %d chars and are separated by 1 char that isnt a number or -\n", S*S);
            exit(4);   
        }
    }
}

void readFromFile(const char* path)
{
    FILE* f = fopen(path, "r");

    if (f == NULL)
    {
        fprintf(stderr, "Couldnt open file %s\n", path);
        exit(5);
    }

    readMultipleSudokus(f);

    fclose(f);
}

const char* helpMsg = "Sudoku solver, by default it solves sudokus from stdin"
                        "\n-h: Help msg"
                        "\n-b: Beautify, print human-readable sudokus"
                        "\n-t: Test default sudoku"
                        "\n-l: Enables log msgs, only fundamental information logged"
                        "\n-f <file>: Solve all the sudokus in the file";

int parseArgs(const int argc, char *const argv[])
{
    int readFromStdin = 1;

    opterr = 0;
    //Flags
    int testDefaultFlag = 0, helpFlag = 0;
    int option;
    char* file = NULL;

    while((option = getopt(argc, argv, "lhbtf:")) != -1)
    {
        switch(option)
        {
            //Help
            case 'h':
            helpFlag = 1;
            break;
            //Test default sudoku
            case 't':
            testDefaultFlag = 1;
            readFromStdin = 0;
            break;
            //Beautify
            case 'b':
            beautifyFlag = 1;
            break;
            //Log
            case 'l':
            logFlag = 1;
            break;

            //File to read
            case 'f':
            file = (char*)malloc(strlen(optarg)*sizeof(char));
            if (file == NULL) exit(3);
            strcpy(file, optarg);
            readFromStdin = 0;
            break;

            case '?':
                if(optopt == 'f')
                    fprintf(stderr, "Option -%c requires a file path\n", optopt);
                else
                    fprintf(stderr, "Unknown option char -%c\n", optopt);
            break;

            default:
            break;
        }
    }

    //Unused arguments will be considered sudokus
    for (int i = optind; i < argc; ++i)
    {
        readFromStdin = 0;
        solveFromStr(argv[i]);
    }

    if (helpFlag)
        printf("%s\n", helpMsg);

    if (testDefaultFlag)
    {
        time_t start = clock();
        solveFromStr(defaultSudoku);
        time_t end = clock();

        printf("Time: %ldms\n", (end - start)/1000);
        exit(0);
    }

    if (file)
    {
        readFromFile(file);
        free(file);
    }

    return readFromStdin;
}

int main(const int argc, char *const argv[])
{
    time_t start = clock();

    const int readFromStdin = parseArgs(argc, argv);

    if (readFromStdin)
        readMultipleSudokus(stdin);

    time_t end = clock();

    if (logFlag)
        printf("Time: %ldms\n", (end - start)/1000);

    return 0;
}