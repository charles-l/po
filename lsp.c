#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#define STACK_SIZE 1024

// TODO: grow stack on demand (maybe?)
// TODO: JIT for blazing fast speedz
// TODO: replace labels with offset addresses

typedef enum opcode {
    NOP   = 0x0,
    PUSHA = 0x1, // push atom
    PUSHN = 0x2, // push number
    PUSHC = 0x3, // push cons
    PUSHL = 0x4, // push lambda
    PUSH0 = 0x5, // push null
    POP   = 0x6,
    JUMP  = 0x7,
    TJUMP = 0x8, // true jump
    FJUMP = 0x9, // false jump
    EOP   = 0x7f // end of program
} opcode;

typedef enum a_type {
    SYM,
    NUM,
    CONS
} a_type;

typedef struct atom {
    a_type type;
    union {
        int num;
        char *sym;
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
    // TODO: clean up allocated shtuff here
    s->sp--;
    return *(s->sp);
}

#define GAP "    "
void dump_state(state_t *s) {
    puts("--- state dump ---");
    for(atom_t *o = s->atoms; o < s->sp; o++) {
        switch(o->type) {
            case SYM:
                printf("0x%08x" GAP "'%s'\n", o, o->sym);
                break;
            case NUM:
                printf("0x%08x" GAP "%i\n", o, o->num);
                break;
            case CONS:
                if(o->car == NULL) {
                    printf("0x%08x" GAP "'()\n");
                } else {
                    printf("0x%08x" GAP "(%s, %p)\n", o, o->car->sym, o->cdr);
                }
                break;
        }
    }
    puts("--- finished dump ---");
}

#define GETTOK(res) \
    char *tmp = strchr(*c, '\x0'); \
    size_t i = (size_t) (tmp - *c); \
    char *v = malloc(i); \
    memcpy(v, *c, i); \
    res = strdup(v + 1); \
    *c = tmp; \

#define PUSHTOK(fields...) \
    atom_t t = {fields}; \
    push(s,t);

#define MEMDUP(dest, src, sz) dest = malloc(sz); memcpy(dest, src, sz);

void mksym (state_t *s, char **c) {
    char *r;
    GETTOK(r);
    PUSHTOK(.type = SYM, .sym = r);
}

void mknum(state_t *s, char **c) {
    char *tmp = strchr(*c, '\x0');
    int r = (int) atoi(*c + 1);
    *c = tmp;
    PUSHTOK(.type = NUM, .num = r);
}

void mkcons(state_t *s, char **c) {
    atom_t a = pop(s); atom_t b = pop(s);
    atom_t *x, *y;
    MEMDUP(x, &a, sizeof(atom_t));
    MEMDUP(y, &b, sizeof(atom_t));
    PUSHTOK(.type = CONS, .car = x, .cdr = y);
}

void mknil (state_t *s, char **c) {
    PUSHTOK(.type = CONS, .car = NULL, .cdr = NULL);
}

// ignore \0
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

int istrue(atom_t t) {
    if(t.type == NUM) {
        return t.num != 0;
    } else if (t.type == CONS) {
        return t.car && t.cdr;
    } else if (t.type == SYM) {
        return 1;
    }
    return 0;
}

void tjump(state_t *s, char **p) {
    if(istrue(pop(s))) {
        jump(s, p);
    }
}

void fjump(state_t *s, char **p) {
    if(!istrue(pop(s))) {
        jump(s, p);
    }
}

void run(state_t *s, char *prog) {
    char *c = prog;
    while(c[0] != EOP) {
        atom_t a;
        switch(c[0]) {
            case NOP:
                break;
            case ':':
                c += 2;
            case PUSHA:
                mksym(s, &c);
                break;
            case PUSHN:
                mknum(s, &c);
                break;
            case PUSHC:
                mkcons(s, &c);
                break;
            case PUSH0:
                mknil(s, &c);
                break;
            case JUMP:
                jump(s, &c);
                break;
            case TJUMP:
                tjump(s, &c);
                break;
            case FJUMP:
                fjump(s, &c);
                break;
            case POP:
                pop(s);
                break;
            default:
                fprintf(stderr, "UNKNOWN OPCODE: 0x%x", c[0]);
                exit(1);
        } c++;
    }
}

int main(void) {
    state_t *s = init_state();
    char *prog =
        "\x1" "somethinaddf\x0"
        "\x1" "think\x0"
        "\x7" "AB"
        "\x2" "3\x0"
        "\x2" "12\x0"
        "\x2" "31\x0"
        ":AB"
        "\x2" "312\x0"
        "\x1" "str\x0"
        "\x3"
        "\x5"
        "\x7f";

    run(s, prog);
    dump_state(s);
    destroy_state(s);
    return 0;
}
