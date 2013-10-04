/**
 * Kirill Melentyev (c) 2013 
 * lib                       
 *
 */

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <memory.h>
#include <stdlib.h> 









#define true 1
#define false 0
                 
typedef char byte;

void DEBUG(char *s) {
    fprintf(stderr, s);
}

void DEBUG_i(int n) {
	fprintf(stderr, "%d\n", n);
}

typedef struct vector {
	byte *_data;
	int size, _capacity;
	int _element_size;
	byte *iterator;
	void(*destruct)(byte*);
} * pvec;

void vec_init(pvec *v, int element_size, void(*_destruct)(byte*)) {
	(*v) = (pvec)malloc(sizeof(**v) );
	(*v)->_element_size = element_size; 
	(*v)->_capacity = 1;
	(*v)->size = 0;              
	(*v)->_data = (char*)malloc(element_size);
	(*v)->destruct  = _destruct;
}

byte* vec_get(pvec v, int x) {
    return v->_data + x * v->_element_size;
}

void vec_push(pvec v, void *element) {
    if(v->size == v->_capacity) {
    	void *new_data = malloc(v->_element_size * v->_capacity * 2);
    	memcpy(new_data, v->_data, v->_element_size * v->size);
    	free(v->_data);
    	v->_data = new_data;
    	v->_capacity *= 2;
    }
    //DEBUG("HERE\n");
    memcpy((v->_data + v->_element_size * v->size), element, v->_element_size);
	v->size++;
}

void vec_release(pvec v) {
	free(v->_data);
	free(v);
}

void vec_debugi(pvec v) {
	int i;
	printf("(%d/%d):", v->size, v->_capacity);
	for(i = 0; i < v->size; i++) {
		int* x = (int*)vec_get(v, i);
	    printf(" %d", *x ); 
	}
	printf("\n");
}

void vec_pop(pvec v) {
	if (v->destruct != NULL) {
		v->destruct(v->_data + v->size * v->_element_size);
	}
	v->size--;
	if (v->size <= v->_capacity / 2 && v->_capacity > 1) {
		byte *new_data = (byte*)malloc(v->_capacity / 2 * v->_element_size);
	    memcpy(new_data, v->_data, v->_element_size * v->size);
	    free(v->_data);
	    v->_data = new_data;
	} 
}

struct string {
	char *_data;
	int _l;
};
          
int main() {
	int n = 10, i;                   
	pvec v1;
	int* it = NULL;
	vec_init(&v1, sizeof(int), NULL );
	for(i = 0; i < 20; i++) {
		vec_push(v1, &i);                          
	    vec_debugi(v1);
	}                                        
	vec_debugi(v1);
	vec_release(v1);
	for(it = (int*)vec_begin(v); it != (int*)vec_end(); it++) {
		printf("%d ",*it); 
	}

	return 0;
}
