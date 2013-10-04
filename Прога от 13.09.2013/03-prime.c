/****************************
* Kirill Melentyev (c) 2013 *
* prime numbers             *
****************************/

#include <stdio.h>

#define true 1
#define false 0

int isPrime(int n) {           
	int i, stillPrime = true;
	for(i = 2; i * i <= n; i++) {
		if(n % i == 0) {
			stillPrime = false; 
			break; 
		}	
	}
	return (n != 1 && stillPrime);
}

int main() {
	int n;                   
	printf("n = ");
	scanf("%d", &n);
	printf(isPrime(n) ? "%d is prime" : "%d is not prime", n);
	return 0;
}
