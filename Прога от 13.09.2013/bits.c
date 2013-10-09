/**
 * Kirill Melentyev (c) 2013 
 * Битовые операции
 */
  
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <time.h>  

#define true 1
#define false 0 

typedef int byte;

void print_float(int x) {
    double mval = 0.0, mpow = 1.0;
    int i, mi = 22, msum = 0, current = x;
    char m[24], firstbit = '0';
    unsigned int uex = 0;
    signed char cex;
    char sign, shift[] = "                              ";

    m[23] = 0;
    for (i = 0; i < 32; i++) {
        if(i < 23) {        
            m[mi] = (current & 1);
            msum += m[mi];
            m[mi--] += '0';
        }
        else if(i < 31) {
            uex >>= 1;
            uex |= (current & 1) * (1 << 7);
        }
        else {
            sign = (current & 1) ? '-' : '+';
        }
        current >>= 1;
    }
    if(uex == 0) {
        if(msum == 0) {
            printf("%c0\n", sign);
        }
        else {
            printf("%s-126\n", shift);
            printf("%c%c.%s * 2\n", sign, firstbit, m);
        }
    }
    else {
        if(uex == 255) {
            if(msum == 0) {
                printf("%cINF\n", sign);  
            }
            else {
                printf("NaN\n");
            }
        }
        else {
            firstbit = '1';
            cex = (*( (char*)&uex) ) - 127;
            printf("%s%d\n", shift, cex);
            printf("%c%c.%s * 2\n", sign, firstbit, m);
        }
    }
}

void print_bits(int *x, int size) {    
    int i, j, current; 
    printf("%.5f\n", *((float*)x));
    current = *x;
    for (i = 0; i < 32; i++) {
        if(i == 23 || i == 31) printf(" ");
        printf("%d", current & 1);
            current >>= 1;
    }
    printf("\n");
    print_float(*x);
}


void tests() {
    float fval0 = 0.0f;
    float fval0001 = 0.00001f;
    float fval025 = 0.25f;
    float fval05 = 0.5f;
    float fval075 = 0.75f;
    float fval1 = 1.0f;
    float fval2 = 2.0f;
    float fval3 = 3.0f;
    float fval4 = 4.0f;
    float fval5 = 5.0f;
    float fval6 = 6.0f;
    float fval7 = 7.0f;
    float fval8 = 8.0f;
    float fval9 = 9.0f;
    float fval10 = 10.0f;
    float fval20 = 20.0f;

    float mfval0 = -0.0f;
    float mfval1 = -1.0f;
    float mfval2 = -2.0f;
    float mfval5 = -5.0f;
    float mfval20 = -20.0f;
       
    print_bits((byte*)&fval0, sizeof(float));
    print_bits((byte*)&fval0001, sizeof(float));
    print_bits((byte*)&fval025, sizeof(float));
    print_bits((byte*)&fval05, sizeof(float));
    print_bits((byte*)&fval075, sizeof(float));
    
    print_bits((byte*)&fval1, sizeof(float));
    print_bits((byte*)&fval2, sizeof(float));
    print_bits((byte*)&fval3, sizeof(float));
    print_bits((byte*)&fval4, sizeof(float));
    print_bits((byte*)&fval5, sizeof(float));
    print_bits((byte*)&fval6, sizeof(float));
    print_bits((byte*)&fval7, sizeof(float));
    print_bits((byte*)&fval8, sizeof(float));
    print_bits((byte*)&fval9, sizeof(float));
    print_bits((byte*)&fval10, sizeof(float));
    print_bits((byte*)&fval20, sizeof(float));
    
    printf("%.8f\n", fval1);

    printf("\n");

    print_bits((byte*)&mfval0, sizeof(float));
    print_bits((byte*)&mfval1, sizeof(float));
    print_bits((byte*)&mfval2, sizeof(float));
    print_bits((byte*)&mfval5, sizeof(float));
    print_bits((byte*)&mfval20, sizeof(float));
    
}


int main() {
    float inputVal;
    float f = 2.0f;
    
    f = 1e100;
    print_float(*((int*)&f));

    f = -1e100;
    print_float(*((int*)&f)); 
    
    f = 2.0f;
    f -= 2.0f;
    f = 0.0f / f;
    print_float(*((int*)&f));

    printf("Enter float number: \n");
    scanf("%f", &inputVal);

    print_float(*((int*)&inputVal) );
    system("pause");
    return 0;
}
    