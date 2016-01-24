#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define STACK_SIZE 1024

// check for 64 bit machine
#ifdef X64
#define stackv uint64_t
#else
#define stackv uint32_t
#endif

// TODO: grow stack on demand

typedef enum opcode {
    NOP   = 0x0,
    PUSH  = 0x1,
    POP   = 0x2,
    LOAD  = 0x3,
    EOP   = 0x7f // end of program
} opcode;

typedef struct stack {
    stackv *root;
    size_t size;
    stackv *sp; // stack pointer
    stackv *fp; // frame pointer
} stack_t;

stack_t *init_stack() {
    stack_t *s = malloc(sizeof(stack_t));
    s->size = STACK_SIZE;
    s->root = malloc(s->size);
    s->fp = s->root + s->size;
    s->sp = s->fp;
    return s;
}

void push(stack_t *s, stackv v) {
    *(s->sp) = v;
    s->sp--;
}

stackv pop(stack_t *s) {
    s->sp++;
    return *(s->sp - 1);
}

void dump_stack(stack_t *s) {
    puts("--- stack dump ---");
    for(stackv *o = s->fp; o > s->sp; o--) {
        printf("0x%x\t0x%x\n", o, *o);
    }
    puts("--- finished dump ---");
}

void run(stack_t *s, char *prog) {
    char *c = prog;
    while(c[0] != EOP) {
        char *v;
        char *tmp;
        char *r;
        size_t i;
        switch(c[0]) {
            case NOP:
                break;
            case PUSH:
                tmp = strchr(c, '\x0');
                i = (size_t) (tmp - c);
                v = malloc(i);
                memcpy(v, c, i);
                r = strdup(v);
                push(s, (stackv) v);
                c+=i;
                break;
            case POP:
                pop(s);
                break;
            default:
                printf("UNKNOWN OPCODE: 0x%x", c[0]);
                exit(1);
        }
        c++;
    }
}

int main(void) {
    stack_t *S = init_stack();
    char *prog = "\x1" "somethinasdf\x0"
        "\x2"
        "\x1" "str\x0"
        "\x7f";

    run(S, prog);
    dump_stack(S);
    return 0;
}
