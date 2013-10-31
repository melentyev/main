#include <stdio.h>

int global = 2;

int g(int x) {
    int y = x + 1;
    return y;
}
int main() {
    g(5);
    return 0;
}
