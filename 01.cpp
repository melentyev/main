#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <time.h> 
#include <iomanip>
using namespace std;


int main() {
    int size = 32 * 1024 * 1024 / sizeof(int);
	int *a = new int[size];
    int s1 = clock();
    int t = 0;
    for (int it = 0; it < 10; it++) {
        for(int cnt = 0, i = 0; cnt < 5 * size; cnt++, i = (i + 2000000) % size) {
            t += a[i];
            //t += 3;
        }
    }
    int s2 = clock();
    
    cout << t << endl << setprecision(3) << fixed << (double)(s2 - s1) / CLOCKS_PER_SEC << endl;
    system("pause");
   	return 0;
}


