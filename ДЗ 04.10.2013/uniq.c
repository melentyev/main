/**
 * Kirill Melentyev (c) 2013 
 * unix uniq
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <time.h>  
#include <string.h>

#define DEFAULT_CHUNK_SIZE 64 

char* append_char(char *s, char c, int len) {
    if (len % DEFAULT_CHUNK_SIZE == 0) {
        s = (char*)realloc(s, len + DEFAULT_CHUNK_SIZE);
    }
    s[len++] = c;
    return s;
}

void run(FILE *stream) {
    int c, cnt = 0, i;
    char **s = (char**)malloc(sizeof(char*));
    int *lens = (int*)malloc(sizeof(int));

    s[cnt] = NULL; 
    lens[cnt] = 0;
    
    do { 
        c = getc(stream);
        if (c == '\n' || c == EOF) {
            s[cnt] = append_char(s[cnt], 0, lens[cnt]);
            cnt++;
            s = (char**)realloc(s, sizeof(char*) * (cnt + 1) );
            lens = (int*)realloc(lens, sizeof(int) * (cnt + 1) );
            s[cnt] = NULL; 
            lens[cnt] = 0;
        }
        else {
            s[cnt] = append_char(s[cnt], c, lens[cnt]);
            lens[cnt]++; 
        }
    } while(c != EOF);
    puts(s[0]);
    for (i = 1; i < cnt; i++) {
        if (strcmp(s[i - 1], s[i]) != 0) {
            puts(s[i]);
        }
        free(s[i - 1]);
    } 
    free(s[cnt - 1]);
    free(s);
    free(lens);
}

int main(int argc, char **argv) {    
    FILE *stream = stdin;
    
    if (argc > 1) {
        stream = fopen(argv[1], "r");
        if (stream == NULL) {
            fprintf(stderr, "File not found.\n");
            return 0;
        }
    }
    
    run(stream);
    
    return 0;
}
    