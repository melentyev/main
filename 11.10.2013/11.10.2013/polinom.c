/**
 * Kirill Melentyev (c) 2013 
 *
 * Polinom
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>    
#include <string.h>

#define TRUE 1
#define FALSE 0
#define DEFAULT_CHUNK_SIZE 2

typedef struct List {
    int x;
    struct List* Next;
} * plist;

int* append_int(int *a, int v, int len) {
    if (len % DEFAULT_CHUNK_SIZE == 0) {
        a = (int*)realloc(a, (len + DEFAULT_CHUNK_SIZE) * sizeof(int));
    }
    a[len] = v;
    return a;
}

int *get_vals_stream(FILE *stream, int *return_len) {
    int c, val, slen  = 0, cnt = 0, done = FALSE;
    int *vals = NULL;
    char *s = (char*)malloc(sizeof(char) * 32);    
    
    do {
        c = getc(stream);
        if(c == ' ' || c == '\n' || c == 0) {
            s[slen] = 0;
            if(slen > 0) {
                sscanf(s, "%d", &val);
            }
            vals = append_int(vals, val, cnt++);
            s[(slen = 0)] = 0;
            if(c == '\n' || c == 0) {
                done = TRUE;
            }
        }
        else s[slen++] = c; 
    } while(!done);
    free(s);
    *return_len = cnt; 
    return vals;
}

int *calc(int *a, int a_len, int *b, int b_len, int sign, int *ans_len) {
    int i, *c;
    // ƒл€ ответа не выдел€ем пам€ть а просто перепизаписываем данные в бќльший по длине многочлен
    *ans_len = (a_len >= b_len ? a_len : b_len);
    c = (a_len >= b_len ? a : b);  
    for(i = 0; i < (*ans_len); i++) {
        c[i] = (i < a_len ? a[i] : 0) + sign * (i < b_len ? b[i] : 0);
    }
    return c;
}

void output(int *a, int len) {
    int something_printed = FALSE, i;
    for(i = len - 1; i >= 0; i--) {
        if(a[i] != 0) {
            if(something_printed && a[i] > 0) {
                printf("+");
            } 
            printf("%d", a[i]);
            if(i > 0) {
                printf("*x");
            }
            if(i > 1) {
                printf("^%d", i);
            }
            something_printed = TRUE;
        }
    }
    if(!something_printed) {
        printf("0");
    }
    printf("\n");
}

void run() {
    int *a, *b, *c, a_len, b_len, ans_len;
    char operation;
    a = get_vals_stream(stdin, &a_len);
    b = get_vals_stream(stdin, &b_len);
    scanf("%c", &operation);
    c = calc(a, a_len, b, b_len, operation == '+' ? 1 : -1, &ans_len);
    output(c, ans_len);
    free(a);
    free(b);
}

int main(int argc, char **argv) {
    run();
    return 0;
}
