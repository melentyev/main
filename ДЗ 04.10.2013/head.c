/**
 * Kirill Melentyev (c) 2013 
 * unix head
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <time.h>         

#define DEFAULT_STR_CNT 10 

int main(int argc, char **argv) {
    int strcnt = DEFAULT_STR_CNT, cnt = 0, c, i;
    FILE *stream = stdin;
    if(argc > 1) {
        for(i = 1; i < argc; i++) {
            if (strcmp(argv[i], "-n") == 0) {
                sscanf(argv[i + 1], "%d", &strcnt); 
                i++;
            }
            else {
                stream = fopen(argv[i], "r");
                if (stream == NULL) {
                    fprintf(stderr, "File not found.\n");
                    return 0;
                }
            }
        }
    }
    do {
        c = getc(stream);
        if(c != EOF) {
            putc(c, stdout);
        }
        if(c == '\n') {
            cnt++;
        }
    } while(c != EOF && cnt < strcnt);
    return 0;
}
    