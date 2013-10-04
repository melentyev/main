/*
 * Kirill Melentyev (c) 2013 
 * Fast a^n                  
 */

#include <stdio.h>

int bin_pow(int a, int n) {
	if (n > 0) {
		int b = bin_pow(a, n / 2);
		return b * b * (n % 2 == 0 ? 1 : a);
	}
	else {
		return 1;
	}
}
 

int main() {
	int a, n;
	printf("bin_pow(a, n)\na = ");
	scanf("%d", &a);
	printf("n = ");
	scanf("%d", &n);
	printf("bin_pow(%d, %d) = %d", a, n, bin_pow(a, n) );
	return 0;
}
