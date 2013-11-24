/**
 * Kirill Melentyev (c) 2013 
 * Поле в форме тора
 */
#include <stdio.h>
#include <string.h>
#include <conio.h>
#include <windows.h>

#define ROWS 25
#define COLUMNS 80
#define STEPS 8
#define EMPTY ' '

int di[] = {  0, -1, -1, -1,  0,  1,  1,  1};
int dj[] = {  1,  1,  0, -1, -1, -1,  0,  1};

char *map;
char *buffer;

void consoleGotoRC(int row, int column, HANDLE hConsole) {
    COORD pos;
    pos.X = column;
    pos.Y = row;
    SetConsoleCursorPosition(hConsole, pos);
}

void consolePutChar(int row, int column, char ch, HANDLE hConsole) {
    consoleGotoRC(row, column, hConsole);
    putc(ch, stdout);
}

void init() { 
    int i = 0, j = 0;
    char strbuf[100];
    FILE *input = fopen("input.txt", "r");
    map = (char*)malloc(sizeof(char) * ROWS * COLUMNS);
    buffer = (char*)malloc(sizeof(char) * ROWS * COLUMNS);
    memset(map, EMPTY, sizeof(char) * ROWS * COLUMNS);
    memset(buffer, EMPTY, sizeof(char) * ROWS * COLUMNS);
    while(fgets(strbuf, 100, input) ) {
        for (j = 0; j < strlen(strbuf); j++) {
            if(strbuf[j] == '#') {
                buffer[i * COLUMNS + j] = '#';
            }
        }
        i++;
    }
}

void clear_output(HANDLE hConsole) {
    int i, j;
    for (i = 0; i < ROWS; i++) {
        for(j = 0; j < COLUMNS; j++) {
            if (map[i * COLUMNS + j] != ' ') {
                consolePutChar(i, j, EMPTY, hConsole);
            }
        }
    }
    memset(map, EMPTY, sizeof(char) * ROWS * COLUMNS);
}

void print_map(HANDLE hConsole) {
    int i, j;
    for (i = 0; i < ROWS; i++) {
        for(j = 0; j < COLUMNS; j++) {
            if (map[i * COLUMNS + j] != ' ') {
                consolePutChar(i, j, map[i * COLUMNS + j], hConsole);
            }
        }
    }
}

int next_state(int i, int j) {
    int sum = 0, k, res;
    for (k = 0; k < STEPS; k++) {
        sum += (map[( (i + ROWS + di[k] ) % ROWS ) * COLUMNS +  (j + COLUMNS + dj[k] ) % COLUMNS ] != EMPTY ? 1 : 0);
    }
    if (map[i * COLUMNS + j] != EMPTY) {
        res = (sum > 3 || sum < 2) ? 0 : 1;    
    }
    else {
        res = (sum == 3) ? 1 : 0;
    }
    if (res != 0) {
        res++;  res--;
    }
    return res;
}

void next_frame() {
    int i, j;
    for (i = 0; i < ROWS; i++) {
        for(j = 0; j < COLUMNS; j++) {
            buffer[i * COLUMNS + j] = (next_state(i, j) == 0 ? EMPTY : '#');
        }
    }
}

void swap_buffer(char **lhs, char **rhs) {
    char *temp = *lhs;
    *lhs = *rhs;
    *rhs = temp;
}

void run() {
    HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
    while (1) {
        clear_output(hConsole);
        swap_buffer(&map, &buffer);
        print_map(hConsole);
        next_frame();
        Sleep(100);
    }
}

int main() {
    init();
    run();
    return 0;
}
