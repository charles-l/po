#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#define STACK_SIZE 1024

// TODO: grow stack on demand (maybe?)
// TODO: JIT for blazing fast speedz

typedef enum opcode {
    NOP   = 0x0,
    PUSHA = 0x1, // push atom
    PUSHN = 0x2, // push number
    PUSHC = 0x3, // push cons
    PUSHL = 0x4, // push lambda
    POP   = 0x5,
    JUMP  = 0x6,
    TJUMP = 0x7, // true jump
    FJUMP = 0x8, // false jump
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

typedef struct jmp_tab {
    uint16_t addrs;
} jmp_tab_t;

typedef struct state {
    atom_t *atoms;
    atom_t *sp;
    atom_t *fp;
    size_t size;
} state_t;

state_t *init_state() {
    state_t *s = malloc(sizeof(state_t));
    s->size = STACK_SIZE;
    s->atoms = malloc(sizeof(atom_t) * s->size);
    s->sp = s->fp = s->atoms;
    return s;
}

void destroy_state(state_t *s) {
    // TODO: delete malloced mem from atoms
    // TODO: delete malloced mem from cons
    free(s->atoms);
    free(s);
    s = NULL;
}

void push(state_t *s, atom_t v) {
    memcpy(s->sp, &v, sizeof(atom_t)); // not sure i need this
    s->sp++;
}

atom_t pop(state_t *s) {
    s->sp--;
    return *(s->sp);
}

void dump_state(state_t *s) {
    puts("--- state dump ---");
    for(atom_t *o = s->atoms; o < s->sp; o++) {
        switch(o->type) {
            case ATOM:
                printf("0x%x\t'%s'\n", o, o->atom);
                break;
            case NUM:
                printf("0x%x\t%i\n", o, o->num);
                break;
            case CONS:
                printf("0x%x\t(%s, %p)\n", o, o->car->atom, o->cdr);
                break;
        }
    }
    puts("--- finished dump ---");
}

#define FULLTOK(res) \
    char *tmp = strchr(*c, '\x0'); \
    size_t i = (size_t) (tmp - *c); \
    char *v = malloc(i); \
    memcpy(v, *c, i); \
    res = strdup(v + 1); \
    *c = tmp; \

atom_t mkatom(char **c) {
    char *r;
    FULLTOK(r);

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

atom_t mkcons(state_t *s, char **p) {
    atom_t a = pop(s); atom_t b = pop(s);
    atom_t *c, *d;
    MEMDUP(c, &a, sizeof(atom_t));
    MEMDUP(d, &b, sizeof(atom_t));
    atom_t t = {.type = CONS, .car = c, .cdr = d};
    return t;
}

char *istrchr(char *s, int c, int e) {
    for(; s[0] != e; s++) {
        if(s[0] == c) {
            return s;
        }
    }
}

void jump(state_t *s, char **p) {
    char lab[2];
    char *tmp = istrchr(*p, ':', EOP);
    memcpy(&lab, tmp + 1, 2);
    while(strncmp(tmp, lab, 2) != 0) {
        tmp = strchr(tmp, ':');
        memcpy(&lab, tmp, 2);
    }
    *p = tmp + 2;
}

void run(state_t *s, char *prog) {
    char *c = prog;
    while(c[0] != EOP) {
        atom_t a;
        switch(c[0]) {
            case NOP:
                goto end;
            case ':':
                c += 2;
            case PUSHA:
                a = mkatom(&c);
                break;
            case PUSHN:
                a = mknum(&c);
                break;
            case PUSHC:
                a = mkcons(s, &c);
                break;
            case JUMP:
                jump(s, &c);
                goto end;
            case POP:
                pop(s);
                goto end;
            default:
                fprintf(stderr, "UNKNOWN OPCODE: 0x%x", c[0]);
                exit(1);
        }
        push(s, a); // TODO: move this to push functions
end:
        c++;
    }
}

int main(void) {
    state_t *s = init_state();
    char *str = "a str";
    char *str2 = "b efesadsf";
    char *prog =
        "\x1" "somethinaddf\x0"
        "\x1" "think\x0"
        "\x6" "AB"
        "\x2" "3\x0"
        "\x2" "12\x0"
        "\x2" "31\x0"
        ":AB"
        "\x2" "312\x0"
        "\x1" "str\x0"
        "\x3"
        "\x7f";

    run(s, prog);
    dump_state(s);
    destroy_state(s);
    return 0;
}
