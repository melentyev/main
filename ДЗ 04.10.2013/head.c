/**
 * Kirill Melentyev (c) 2013 
 * unix head
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <time.h>         

#define DEFAULT_STR_CNT 10 

void run(FILE *stream, int strcnt) {
    int cnt = 0, c; 
    do {
        c = getc(stream);
        if(c != EOF) {
            putc(c, stdout);
        }
        if(c == '\n') {
            cnt++;
        }
    } while(c != EOF && cnt < strcnt);
}

int main(int argc, char **argv) {
    int strcnt = DEFAULT_STR_CNT, i;
    FILE *stream = stdin;
    if(argc > 1) {
        for(i = 1; i < argc; i++) {
            if (strcmp(argv[i], "-n") == 0) {
                if (i + 1 >= argc || sscanf(argv[i + 1], "%d", &strcnt) <= 0) {
                    fprintf(stderr, "Error: count not defined.\n");
                    return 0; 
                }                                   
                i++;
            }
            else {
                if(stream != stdin) {
                    fprintf(stderr, "Error: more than one file provided.\n");
                    return 0;
                }
                stream = fopen(argv[i], "r");
                if (stream == NULL) {
                    fprintf(stderr, "File not found.\n");
                    return 0;
                }
            }
        }
    }
    run(stream, strcnt);
    return 0;
}
    