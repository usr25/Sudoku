/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "board.h"
#include "checker.h"
#include "solver.h"

#define SQRS WIDTH * HEIGHT / (SQR * SQR)


// Calculates the possible values inside of certain squares
void calculatePossible(Board *board) {
    unsigned int rows[HEIGHT] = {0}, cols[WIDTH] = {0}, sqrs[SQRS] = {0};
    int pos, m, k;
    
    // Goes tile by tile and ORs against the values in the respective arrays
    for (int i = 0; i < HEIGHT; i++) {
        for (int j = 0; j < WIDTH; j++) {
            if (board->values[i][j] != 0) {
                m = GET_SQR(i,j);
                pos = POS(board->values[i][j]);
                rows[i] |= pos;
                cols[j] |= pos;
                sqrs[m] |= pos;
                board->possible[i][j] = 0;
            }
        }
    }
    
    // Inverts the values
    for (int i = 0; i < HEIGHT; i++) {
        for (int j = 0; j < WIDTH; j++) {
            if (board->values[i][j] == 0) {
                k = GET_SQR(i,j);
                board->possible[i][j] = SIZE ^ (rows[i] | cols[j] | sqrs[k]);
            } else {
                board->possible[i][j] = 0;
            }
        }
    }
}


// Call this instead of calculatePossible when the value of a tile is changed
// This function removes 'val' from possible in the row, col and sqr
// PRE: board->values[x][y] != 0
void updateTileAdded(Board *board, const int y, const int x) {
    unsigned int val = SIZE ^ POS(board->values[y][x]);
    int x0 = SQR*(int)(x/SQR), y0 = SQR*(int)(y/SQR);
    board->possible[y][x] = 0;
    
    for (int j = 0; j < WIDTH; j++){
        if (board->values[y][j] == 0)
            board->possible[y][j] &= val;
    }

    for (int i = 0; i < HEIGHT; i++){
        if (board->values[i][x] == 0) 
            board->possible[i][x] &= val;
    }
    
    for (int i = y0; i < y0 + SQR; i++){
        for (int j = x0; j < x0 + SQR; j++){
            if (board->values[i][j] == 0) 
                board->possible[i][j] &= val;
        }
    }
}

long depthFS(Board *board) {
    unsigned short c[HEIGHT*WIDTH];
    char index = 0, val, x, y;
    long count = 0;
    
    calculatePossible(board);
    
    FEEDFORWARD:
    count++;
    
    for (int i = 0; i < HEIGHT; i++) {
        for (int j = 0; j < WIDTH; j++) {
            if (board->values[i][j] == 0) {
                if (board->possible[i][j] == 0) {
                    goto BACKPROPAGATION;
                } else {
                    addNextPossibleValue(board, &index, &c[0], 0, i, j);
                }
            }
        }
    }
    
    if (!completeBoard(*board)) {
        BACKPROPAGATION:
        
        do {
            index--;
            if (index < 0) {
                printf("[-] A solution wasn't found.\n");
                exit(1);
            }
            
            val = GET_VAL(c[index]);
            x = GET_X(c[index]);
            y = GET_Y(c[index]);

            board->values[y][x] = 0;
            calculatePossible(board);
        } while ((1 << val) > board->possible[y][x]);
        
        addNextPossibleValue(board, &index, &c[0], val, y, x);
        goto FEEDFORWARD;
    } else if (!checkBoard(*board)) {
        printf("[-] The solution found has errors.");
        exit(1);
    }
    
    return count;
}

void addNextPossibleValue(Board *board, char *index, unsigned short *c, const char start, const char i, const char j) {
    if (__builtin_popcount(board->possible[i][j]) == 1) {
        changeValue(board, index, c, i, j, getForced(board->possible[i][j]));
    } else {
        for (int k = start; k < RANGE; k++) {
            if (VAL_IN_BYTE(board->possible[i][j], k)) {
                changeValue(board, index, c, i, j, k+1);
                return;
            }
        }
    }
}

void changeValue(Board *board, char *index, unsigned short *c, const int i, const int j, const int k) {
    c[*index] = (unsigned short) SET_VAL(i,j,k);
    board->values[i][j] = k;
    updateTileAdded(board,i,j);
    (*index)++;
}

int getForced(unsigned short index) {
    int val = 1;
    while (index >>= 1) ++val;
    return val;
}