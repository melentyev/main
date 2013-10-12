#ifndef _STACK_H_
#define _STACK_H_

typedef struct __stack {
    struct __stackElement *top;
} stack;

typedef struct __stackElement {
    struct __stackElement *_next;
    int val;
} stackElement;

stack create();
void push(stack *c, int val);
int size(stack c);
int pop(stack *c);
void dup(stack *c);
int empty(stack *c);

#endif