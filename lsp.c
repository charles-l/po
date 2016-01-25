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
    PUSHA = 0x1, // push atom
    PUSHN = 0x2, // push number
    PUSHC = 0x3, // push cons
    POP   = 0x4,
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
        printf("0x%x\t'%s'\n", o, *o);
    }
    puts("--- finished dump ---");
}

char *getatom(char **c) {
    char *v;
    char *tmp;
    char *r;
    size_t i;

    tmp = strchr(*c, '\x0');
    i = (size_t) (tmp - *c);
    v = malloc(i);
    memcpy(v, *c, i);
    r = strdup(v + 1);
    *c = tmp;
    return r;
}

void run(stack_t *s, char *prog) {
    char *c = prog;
    while(c[0] != EOP) {
        char *a;
        switch(c[0]) {
            case NOP:
                break;
            case PUSHA:
                a = getatom(&c);
                push(s, (stackv) a);
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
    char *prog =
        "\x1" "somethinaddf\x0"
        "\x4"
        "\x1" "str\x0"
        "\x7f";

    run(S, prog);
    dump_stack(S);
    return 0;
}
