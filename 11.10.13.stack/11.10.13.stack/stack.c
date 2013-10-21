/**
 * Kirill Melentyev (c) 2013 
 * Стековый калькулятор
 */

#include <memory.h>
#include <stdlib.h>
#include "stack.h"

stack create() {
    stack c;
    c.top = NULL;
    return c;
}

void push(stack *c, int val) {
    stackElement *el = (stackElement*)malloc(sizeof(stackElement));
    el->val = val;
    el->_next = c->top;
    c->top = el;
}

int size(stack *c) {
    int res = 0;
    stackElement *ptr = c->top;
    while(ptr != NULL) {
        ptr = ptr->_next;
        res++;
    }
    return res;
}

int pop(stack *c) {
    stackElement *ptr = c->top;
    int val = c->top->val;
    c->top = c->top->_next;
    free(ptr);
    return val;
}

int empty(stack *c) {
    return size(c) == 0;
}

void dup(stack *c) {
    push(c, c->top->val);
}