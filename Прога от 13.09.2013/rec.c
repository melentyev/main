#include <stdio.h>

int sub() {
    int k = 3;
    printf("%d\n", (int)&k);
    return 0;
}

int main() {
    int n = 1;
    printf("%d\n", (int)&n);
    sub();
    return 0;
}