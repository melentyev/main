/****************************
* Kirill Melentyev (c) 2013 *
* palindrom                 *
****************************/

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <memory.h>
#include <stdlib.h> 

#define true 1
#define false 0

int eq(char a, char b) {
	a = tolower(a);
	b = tolower(b);
	return (a == b);
}
                         
int isPalindrom(char *s) {
    int stillPalindrom = true;
	char *l = s;
	char *r = s + strlen(s) - 1;  
	while(l < r) {
		while ((*l) && (*(l++) ) == ' '); 
		while (r >= s  && (*(r--) ) == ' ');
		if(l < r && !eq( *(l++), *(r--) ) ) {
			stillPalindrom = false;
			break;
		}
	}  
	return stillPalindrom;
}


int main() {
	char *s = (char*)malloc(256 * sizeof(char) );

	printf("String:\n");
	gets(s);
	
	printf(isPalindrom(s) ? "palindrom\n" : "not palindrom\n");
	return 0;
}
