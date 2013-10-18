/**
 * Kirill Melentyev (c) 2013 
 * Pascal's triangle                  
 *
 * Usage: 
 * pretty_table.exe rows columns [cellsize]
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>           


int main(int argc, char **argv) {
    int n = 0, m = 0, i, j, prev, above, *a;
    int cellsize = 8;
    char *strDelim, formatModifier[] = "%_d|";
    
    if(argc < 3) {
         printf("Usage: pretty_table.exe rows columns [cellsize]\n");
         return 0;
    }
    
    sscanf(argv[1],  "%d", &n);
    sscanf(argv[2],  "%d", &m);  
    
    // Это что бы вывод был регулировался значением cellsize (при cellsize > 9 в любом случае уже будет плохо)
    if(argc > 3) { 
        sscanf(argv[3],  "%d", &cellsize); 
    }
    formatModifier[1] = cellsize + '0';
    strDelim = (char*)malloc(sizeof(char) * cellsize + 2);
    for(i = 0; i < cellsize; i++) {
        strDelim[i] = '-';
    }
    strDelim[cellsize] = '+';
    strDelim[cellsize + 1] = 0;


    //printf("Enter rows & columns (separated by space)\n");
    //scanf("%d%d", &n, &m);
    
    a = (int*)malloc(sizeof(int) * m);

    printf("+");
    for(j = 0; j < m; j++) {
        printf(strDelim);
        a[j] = 0;
    }
    printf("\n");
    
    for(i = 0; i < n; i++) {
        prev = (i == 0 ? 1 : 0);
        printf("|");
        for(j = 0; j < m; j++) {
            above = a[j];
            a[j] = above + prev;
            prev = a[j];
            printf(formatModifier, a[j]);
        }
        printf("\n+");
        for(j = 0; j < m; j++) {
            printf(strDelim);
        }
        printf("\n");
    }
    return 0;
}
