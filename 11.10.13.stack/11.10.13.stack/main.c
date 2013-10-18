/**
 * Kirill Melentyev (c) 2013 
 * Стековый калькулятор
 */
  
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <time.h>  
#include "stack.h"

#define TRUE 1
#define FALSE 0 

void run() {
    stack c = create();
    push(&c, 1);
    push(&c, 2);
    push(&c, 3);
    while(!empty(&c) ) {
        printf("%d\n", pop(&c) );
    }
    getc(stdin);
}

int main() {
    run();
    return 0;
}
    