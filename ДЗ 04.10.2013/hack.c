/**
 * Kirill Melentyev (c) 2013 
 * Вывод Yes при переполнении буфера функцией gets                  
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <time.h>         

#define INITIAL (-128) 

void other_func(char a) {
    if(a != INITIAL) {
        printf("Yes\n");
    }
    else {
        printf("No\n");
    }
}

int main(int argc, char **argv) {
    char s[5];
    char a = INITIAL;    
    gets(s);          
    other_func(a);
    return 0;
}
    