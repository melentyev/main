/**
 * Kirill Melentyev (c) 2013 
 * Сортировки интовых массивов                  
 *
 * По выводу программы видно в файле sortout.txt видно, что быстрая 
 * сортировка работает примерно в два раза быстрее, чем пирамидальная,
 * а модификация быстрой сортировки (сортировка маленьких подмассивов
 * "пузырьком") работает еще немного быстрее.
 * Сортировку пузырьком на большом случайном массиве не запукаем совсем, 
 * потому что дождаться ее не реально, но на большом отсортированом массиве, 
 * и на большом почти отсортированном она отрабатывает достаточно быстро благодаря модификации 
 * поочередных проходов от начала в конец и от конца в начало.
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <time.h>
#include <math.h>
#include <string.h>         

#define min MIN_
#define max MAX_

#define true 1
#define false 0

#define BIG 5000000
#define SMALL 500
#define MEDIUM 50000

typedef int(*pfunc)(int*, int);

// rand() returns at least 15 bit integer, but we need more than 15 bit
int my_rand() {
    int res = (rand() << 15) | (rand() & ( (1 << 15) - 1) );
    return res;
}

int min(int a, int b) {
    return (a < b ? a : b);
}

int max(int a, int b) {
    return (a < b ? a : b);
}

void swap(int *a, int *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

void heap_restore(int *a, int n, int _v) {
    int v = _v;
    while(v * 2 <= n && a[v * 2] > a[v] || v * 2 + 1 <= n && a[v * 2 + 1] > a[v]) {
        if(v * 2 + 1 <= n && a[v * 2 + 1] > a[v] && a[v * 2 + 1] > a[v * 2]) {
            swap(a + v, a + (v * 2 + 1) );
            v = v * 2 + 1;
        }
        else {
            swap(a + v, a + v * 2 );
            v = v * 2;
        }
    }
}

int heap_sort(int *a, int n) {
    int i;
    int *last = a + n - 1;
    
    // теперь мы никогда не будем обращаться к элементу a[0], но будем к a[n] (удобно для кучи);
    a--;

    for(i = n; i >= 1; i--) {
        heap_restore(a, n, i);      
    }
    while(last > a) {
        swap(last, a + 1);
        last--;
        heap_restore(a, --n, 1);
    }
    return 0;     
}

// На самом деле шейкер, потому что в такой модификации хорошо работает на "почти отсортированных"
int bubble_sort(int *a, int n) {
    int i = 0, j, swaps = 0, dir = 0, init;
    while(n > 1) {
        if(i == 0) {
            swaps = 0;
            dir = 1;
            init = 0;
        }
        else {
            dir = -1;
            init = n - 1;
        }
        for(j = init; j < n && j >= 0 && j + dir >= 0 && j + dir < n; j += dir) {
            if(dir == 1 && a[j] > a[j + 1] || dir == -1 && a[j] < a[j - 1]) {
                swap(a + j, a + (j + dir) );
                swaps++;
            }
        }
        if(i == 1) {
            a++;
            n -= 2;
            if(swaps == 0) 
                break;
        }   
        i ^= 1;
    }
    return 0;
}

void rand_array(int *a, int n, int l,  int r) {
    int *end = a + n;
    while(a != end) {
        *(a++) = my_rand() % (r - l + 1)  + l;
    }
}

void sorted_array(int *a, int n, int l, int r) {
    rand_array(a, n, l, r);
    quick_sort_bubble(a, n);
}

void almost_sorted_array(int *a, int n, int l, int r) {
    int i;
    sorted_array(a, n, l, r);
    for(i = 0; i < 2 * (double)log(n); i++) {
        swap(a + (my_rand() % n), a + (my_rand() % n));
    }
    
}

void print_array(int *a, int n) {
    int i, *end = a + n;
    printf("array(cnt: %d, sorted: %c):", n, (is_sorted(a, n) ? 'Y' : 'N') );
    while(a != end) {
        printf(" %d", *(a++) ); 
    }
    printf("\n");
}

int is_sorted(int *a, int n) {
    int *last = a + n - 1;
    int stillSorted = true;
    a++;
    while(a < last) {
        if ( *a > *(a + 1) ) {               
            stillSorted = false;
            break;
        }
        a++;
    }
    return stillSorted;
}


int _quick_sort(int *a, int n, int small_part, pfunc fn) {    
    while(n > 1) {
        int *l, *r, x, n1, n2, *a1, *a2;
        if(n < small_part && fn != NULL) {
            fn(a, n);
            return 0;       
        }       
        l = a;
        r = a + n - 1;
        x = a[my_rand() % n];
        //printf("%d\n", x);
        do {
            while((*l) < x) l++;
            while((*r) > x) r--;
            if(l <= r) {
                swap(l, r);
                l++;
                r--;
            }   
        }
        while (l <= r);
        a1 = a;
        a2 = l;
        n1 = r - a + 1;
        n2 = a + n - l;
        
        //Рекурсивно спускаемся только в меньшую ветвь - так стек не превысит log(n)
        if(n1 > n2) {
            _quick_sort(a2, n2, small_part, fn);
            a = a1;
            n = n1;
        }
        else {
            _quick_sort(a1, n1, small_part, fn);
            a = a2;
            n = n2;
        }                       
    }
    return 0;
}

int quick_sort(int *a, int n) {
    _quick_sort(a, n, 0, NULL);
}
                       
int quick_sort_bubble(int *a, int n) {
    _quick_sort(a, n, 8, bubble_sort);
}


// Чтобы удобно было
int test_sort(int *a, int n, int(*fn)(int*, int), int show_res, int times) {
    int *buf, start, finish, sorted, step;
    buf = (int*)malloc(n * sizeof(int) );
    
    start = clock();

    for(step = 0; step < times; step++) {
        memcpy(buf, a, n * sizeof(int) );
        fn(buf, n);
        if (show_res) {
            print_array(buf, min(n, 100) ); 
        }   
    }
    
    finish = clock();
          
    sorted = is_sorted(buf, n);
    free(buf);
    if (!sorted) {
        return -1;
    }
    else {
        return finish - start;
    }
}

void _test_array_type(pfunc functions[], char **sort_names, int sort_num, int smalltimes, int *small, int *medium, int *big, char *suff) {
    int res1, res2, res3;

    res1 = test_sort(small, SMALL, functions[sort_num], false, smalltimes);
    res2 = test_sort(medium, MEDIUM, functions[sort_num], false, 1);
    
    printf("\n  small_%s: %.7lf\n", suff, (double)res1 / ((double)smalltimes) / 1000.0);
    printf("  medium_%s: %.7lf\n", suff, (double)res2 / 1000.0);

    if(sort_num == 0 && strcmp(suff, "rand") == 0) {
        printf("  big_%s: not runnig this sort algo on this data\n\n");
    }
    else {
        res3 = test_sort(big, BIG, functions[sort_num], false, 1);    
        printf("  big_%s: %.7lf\n\n", suff, (double)res3 / 1000.0);
    }
}

void run_testing(int sorts_cnt, pfunc functions[], char **sort_names, int smalltimes) {
    int n,
        res_rand1 = 0, res_rand2 = 0, res_rand3 = 0, 
        res_almost1 = 0, res_almost2 = 0, res_almost3 = 0, 
        res_sorted1 = 0, res_sorted2 = 0, res_sorted3 = 0, 
        sort_num,
        *small_rand = (int*)malloc(sizeof(int) * SMALL), 
        *medium_rand = (int*)malloc(sizeof(int) * MEDIUM), 
        *big_rand = (int*)malloc(sizeof(int) * BIG),
        
        *small_almost_rand = (int*)malloc(sizeof(int) * SMALL), 
        *medium_almost_rand = (int*)malloc(sizeof(int) * MEDIUM), 
        *big_almost_rand = (int*)malloc(sizeof(int) * BIG),
        
        *small_sorted = (int*)malloc(sizeof(int) * SMALL), 
        *medium_sorted = (int*)malloc(sizeof(int) * MEDIUM), 
        *big_sorted = (int*)malloc(sizeof(int) * BIG);
    
    rand_array(small_rand, SMALL, 0, SMALL * 3);
    rand_array(medium_rand, MEDIUM, 0, MEDIUM * 3);
    rand_array(big_rand, BIG, 0, BIG * 3);    
    
    almost_sorted_array(small_almost_rand, SMALL, 0, SMALL * 3);
    almost_sorted_array(medium_almost_rand, MEDIUM, 0, MEDIUM * 3);
    almost_sorted_array(big_almost_rand, BIG, 0, BIG * 3); 
    
    sorted_array(small_sorted, SMALL, 0, SMALL * 3);
    sorted_array(medium_sorted, MEDIUM, 0, MEDIUM * 3);
    sorted_array(big_sorted, BIG, 0, BIG * 3);
   
    for (sort_num = 0; sort_num < 4; sort_num++) {        
        printf("Sort: %s\n  Times:\n", sort_names[sort_num]);
        _test_array_type(functions, sort_names, sort_num, smalltimes, small_rand, medium_rand, big_rand, "rand");
        _test_array_type(functions, sort_names, sort_num, smalltimes, small_almost_rand, medium_almost_rand, big_almost_rand, "almost_rand");
        _test_array_type(functions, sort_names, sort_num, smalltimes, small_sorted, medium_sorted, big_sorted, "sorted");
    }
    free(small_rand);
    free(medium_rand);
    free(big_rand);

    free(small_almost_rand);
    free(medium_almost_rand);
    free(big_almost_rand);

    free(small_sorted);
    free(medium_sorted);
    free(big_sorted);
}

int main() {            
    srand(117);
    //int a[] = {4, 1, 2, 10, 3, 7, 8, 6, 9, 5};
    pfunc functions[] = {bubble_sort, heap_sort, quick_sort, quick_sort_bubble};
    char *sort_names[] = {"bubble_sort", "heap_sort", "quick_sort", "quick_sort_bubble"};
    run_testing(4, functions, sort_names, 1000);
    //test_sort(a, 10, bubble_sort, 1, 1);
    return 0;
}
    