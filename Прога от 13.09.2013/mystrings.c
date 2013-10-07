/**
 * Kirill Melentyev (c) 2013 
 * Строковые функции
 */

#include <stdio.h> 
#include <stdlib.h>
#define true 1
#define false 0
                 
typedef char* pchar;
  
int _strlen(pchar c) {
    int len = 0;
    while (*c) {
        c++, len++;
    }
    return len;
}

void _strcpy(pchar dst, pchar src) {
    while (*src) {
        *(dst++) = *(src++);
    }
    *dst = 0;
}

void _strcat(pchar dst, pchar src) {
    while (*dst) { 
        dst++;
    }
    _strcpy(dst, src);
}

int _strcmp(pchar s1, pchar s2) {
    while ( (*s1) && (*s2) && (*s1) == (*s2) ) {
        s1++;
        s2++;
    }
    return (*s1) - (*s2);
}

void tests() {
    char s1[] = "Hello, ";
    char s2[] = "World!";
    char s3[] = "String3";
    char *buf = (char*)malloc(sizeof(char) * 100);
    
    _strcpy(buf, s1);
    _strcat(buf, s2);
        puts(buf);
    printf("Compare: 'Hello, ' and 'Hello, World!' %d\n", strcmp(s1, buf) );
}

int main() {
    tests();    
    return 0;
}
