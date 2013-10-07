/**
 * Kirill Melentyev (c) 2013 
 * Вывод Yes при переполнении буфера функцией gets     
 * Компилировал под MinGW gcc 4.8.1. 
 * В функции main компилятор последовательно кладет 5 байт массива s и один байт переменной a
 * если читаем >= 5-ти символов, то значение прежнее a (8 единиц) затирается.
 * А если ввод сильно большой, то все падает по рантайму совсем.
 * Я наверное не очень хорошо понял задание все-таки...
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
    