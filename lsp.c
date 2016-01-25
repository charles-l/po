#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define STACK_SIZE 1024

// TODO: grow stack on demand (maybe?)

typedef enum opcode {
    NOP   = 0x0,
    PUSHA = 0x1, // push atom
    PUSHN = 0x2, // push number
    PUSHC = 0x3, // push cons
    POP   = 0x4,
    EOP   = 0x7f // end of program
} opcode;

typedef enum a_type {
    ATOM,
    NUM,
    CONS
} a_type;

typedef struct atom {
    a_type type;
    union {
        int num;
        char *atom;
        struct {
            struct atom *car, *cdr;
        };
    };
} atom_t;

typedef struct stack {
    atom_t *atoms;
    atom_t *h;
    size_t size;
} stack_t;

stack_t *init_stack() {
    stack_t *s = malloc(sizeof(stack_t));
    s->size = STACK_SIZE;
    s->atoms = malloc(sizeof(atom_t) * s->size);
    s->h = s->atoms;
    return s;
}

void destroy_stack(stack_t *s) {
    // TODO: delete malloced mem from atoms
    // TODO: delete malloced mem from cons
    free(s->atoms);
    free(s);
    s = NULL;
}

void push(stack_t *s, atom_t v) {
    memcpy(s->h, &v, sizeof(atom_t)); // not sure i need this
    s->h++;
}

atom_t pop(stack_t *s) {
    s->h--;
    return *(s->h);
}

void dump_stack(stack_t *s) {
    puts("--- stack dump ---");
    for(atom_t *o = s->atoms; o < s->h; o++) {
        switch(o->type) {
            case ATOM:
                printf("0x%x\t'%s'\n", o, o->atom);
                break;
            case NUM:
                printf("0x%x\t%i\n", o, o->num);
                break;
            case CONS:
                printf("0x%x\t(%p, %p)\n", o, o->car, o->cdr);
                break;
        }
    }
    puts("--- finished dump ---");
}

atom_t mkatom(char **c) {
    char *tmp = strchr(*c, '\x0');
    size_t i = (size_t) (tmp - *c);
    char *v = malloc(i);
    memcpy(v, *c, i);
    char *r = strdup(v + 1);
    *c = tmp;

    atom_t t = {.type = ATOM, .atom = r};
    return t;
}

atom_t mknum(char **c) {
    char *tmp = strchr(*c, '\x0');
    int r = (int) atoi(*c + 1);
    *c = tmp;

    atom_t t = {.type = NUM, .num = r};
    return t;
}

#define MEMDUP(dest, src, sz) dest = malloc(sz); memcpy(dest, src, sz);

atom_t mkcons(stack_t *s, char **prog) {
    atom_t a = pop(s); atom_t b = pop(s);
    atom_t *c, *d;
    MEMDUP(c, &a, sizeof(atom_t));
    MEMDUP(d, &b, sizeof(atom_t));
    atom_t t = {.type = CONS, .car = &a, .cdr = &b};
    return t;
}

void run(stack_t *s, char *prog) {
    char *c = prog;
    while(c[0] != EOP) {
        atom_t a;
        switch(c[0]) {
            case NOP:
                goto end;
            case PUSHA:
                a = mkatom(&c);
                break;
            case PUSHN:
                a = mknum(&c);
                break;
            case PUSHC:
                a = mkcons(s, &c);
                break;
            case POP:
                pop(s);
                goto end;
            default:
                fprintf(stderr, "UNKNOWN OPCODE: 0x%x", c[0]);
                exit(1);
        }
        push(s, a);
end:
        c++;
    }
}

int main(void) {
    stack_t *s = init_stack();
    char *prog =
        "\x1" "somethinaddf\x0"
        "\x1" "think\x0"
        "\x2" "3\x0"
        "\x2" "12\x0"
        "\x2" "31\x0"
        "\x2" "312\x0"
        "\x1" "str\x0"
        "\x3"
        "\x7f";

    run(s, prog);
    dump_stack(s);
    destroy_stack(s);
    return 0;
}
