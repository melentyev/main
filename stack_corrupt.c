#include <stdio.h>
#include <stdlib.h>

#pragma pack(1)

int global = 2;

int main();

void function2() {
    printf("HACK");
    exit(0);
}

void print_mem(void *start, int words) {
    unsigned int *addr = (unsigned int*)start;
    int i;
    for (i = 0; i < words; i++) {
        printf("0x%08X: %08X\n", addr, *((unsigned int*)addr));
        addr++;
    }
}

void function1(FILE *stream) {
    char s[4];
    printf("main: %08X\n", &main);
    printf("function2: %08X\n", function2);
    //*(unsigned int*)( (&s) + 4) = (unsigned int*)function2;
    print_mem((void*)&s, 20);
    //fgets(s, 100000000, stream);
}

void writeHackSequence(FILE *stream) {
    int cnt = 16;
    int i;
    unsigned char sequence[] = {
                                '#', '#', '#', '#', 
                                '#', '#', '#', '#',
                                '#', '#', '#', '#',
                                '#', '#', '#', '#',
                                '\x00', 
                                };
    for (i = 0; i < cnt; i++) {
        putc(sequence[i], stream);
    }                         
}                              

int main() {
    function1(stdin);
    printf("NOT HACK");
    return 0;
}
