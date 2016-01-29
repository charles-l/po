#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>

#define STACK_SIZE 1024
#define FIRST_ATOM (s->atoms + 1)

#define P(a) print_atom(a); printf("\n");
#define error(args...) do { fprintf(stderr, args); exit(1); } while (0)

// TODO: MAKE THE ENTIRE STACK AN ALIST THAT IT CAN REWRITE AT RUNTIME!!!
// TODO: JIT for blazing fast speedz
// TODO: replace labels with offset addresses
// TODO: error type

// I'm pretty sure this can all eventually be simplified to:
// car
// cdr
// cons
// null?
// eq?
// an environment list
// stack

typedef enum opcode { NOP   = 0x0,
    PUSHA = 0x1, // push atom
    PUSHN = 0x2, // push number
    PUSHC = 0x3, // push cons
    PUSH0 = 0x5, // push null
    POP   = 0x6,

    CAR   = 0xA,
    CDR   = 0xB,

    QUOTE = 0xC,
    ATOM  = 0xD,
    EQ    = 0xE,
    COND  = 0xF,

    ///

    FFI   = 0x40, // FFI call
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
        struct atom * (*func)(struct atom *a);
        int num;
        char *sym;
        struct {
            struct atom *car, *cdr;
        };
    };
} atom_t;

typedef struct state {
    atom_t **atoms;
    atom_t **sp;
    size_t size;
} state_t;

atom_t *nil;

state_t init_state() {
    state_t s;
    s.size = STACK_SIZE;
    s.atoms = malloc(sizeof(atom_t *) * s.size);
    s.sp = s.atoms;

    return s;
}

int isnil(atom_t *t) {
    if (t == NULL) return 1;
    return t->car == NULL && t->cdr == NULL;
}

void destroy_atom(atom_t *a, int fulldestroy) {
    if(isnil(a) || a == NULL) return;
    switch(a->type) {
        case NUM:
            break;
        case SYM:
            free(a->sym); // free malloced string
            break;
        case CONS:
            // fulldestroy will destroy all cons
            if(fulldestroy) {
                destroy_atom(a->car, 1);
                if(a->cdr != nil) {
                    destroy_atom(a->cdr, 1);
                }
            }
            break;
    }
    free(a);
    a = nil;
}

void destroy_state(state_t *s) {
    atom_t **i = FIRST_ATOM;
    while(i <= s->sp)
    {
        destroy_atom(*(i++), 1);
    }
    free(s->atoms);
}

void push(state_t *s, atom_t *v) {
    s->sp++;
    *(s->sp) = v;
}

atom_t *pop(state_t *s) {
    return *(s->sp--);
}

atom_t *adup(atom_t *a) {
    atom_t *r = malloc(sizeof(atom_t));
    memcpy(r, a, sizeof(atom_t));
    return r;
}

#define GAP " "

void print_atom(atom_t *a) {
    if(a == NULL) return;
    switch(a->type) {
        case SYM:
            printf("'%s'", a->sym);
            break;
        case NUM:
            printf("%i", a->num);
            break;
        case CONS:
            if(isnil(a)) {
                printf("nil");
            } else {
                printf("(", a);
                print_atom(a->car);
                printf(GAP);
                print_atom(a->cdr);
                printf(")");
            }
            break;
    }
}

void dump_state(state_t *s) {
    puts("--- state dump ---");
    for(atom_t **o = FIRST_ATOM; o <= s->sp; o++)
    {
        printf("%08p" GAP, *o);
        P(*o);
    }
    puts("--- finished dump ---");
}

// ignore \0
char *istrchr(char *s, int c, int e) {
    for(; s[0] != e; s++) {
        if(s[0] == c) {
            return s;
        }
    }
    return NULL;
}

char *gettok(char **c) {
    char *tmp = istrchr(*c, '\x0', '\x7f');
    size_t i = (size_t) (tmp - *c);
    char *r = malloc(i);
    memcpy(r, (*c) + 1, i);
    *c = tmp;
    return r;
}

// note: r MUST be allocated memory
atom_t *sym (char *r) {
    atom_t a = {.type = SYM, .sym = r};
    return adup(&a);
}

atom_t *num(int v) {
    atom_t a = {.type = NUM, .num = v};
    return adup(&a);
}

atom_t *cons(atom_t *x, atom_t *y) {
    atom_t a = {.type = CONS, .car = adup(x), .cdr = adup(y)};
    // TODO: figure this out
    //destroy_atom(x, 0);
    //destroy_atom(y, 0);
    return adup(&a);
}

atom_t *car(atom_t *a) {
    if(a->type == CONS)
        return a->car;
    return a;
}

atom_t *cdr(atom_t *a) {
    if(a->type == CONS)
        return a->cdr;
    return a;
}

atom_t *eq(atom_t *a, atom_t *b) {
    if(a->type != b->type) return nil;
    switch(a->type) {
        case NUM:
            if(a->num == b->num) return sym(strdup("t"));
        case SYM:
            if(strcmp(a->sym, b->sym) == 0) return sym(strdup("t"));
        case CONS:
            if(eq(a->car, b->car)) {
                return eq(a->cdr, b->cdr);
            } else {
                return nil;
            }
    }
    return nil;
}

atom_t *isatom(atom_t *a) {
    if(a->type == SYM || a->type == NUM) {
        return sym(strdup("t"));
    } else {
        return nil;
    }
}

#define CASE(en, b) \
    case en: \
b; \
break;

void eval(state_t *s, char **c) {
    char *t;
    atom_t *a;
    atom_t *x;
    atom_t *y;
    switch((*c)[0]) {
        // TODO: do not reference prog directly
        CASE(NOP, NULL);
        CASE(PUSHA, push(s, sym(gettok(c))));
        CASE(PUSHN,
                t = gettok(c);
                push(s, num(atoi(t)));
                free(t));
        CASE(PUSHC,
                x = pop(s);
                y = pop(s);
                push(s, cons(x, y)));
        CASE(PUSH0,
                push(s, nil));
        CASE(CAR,   push(s, adup(car(*(s->sp)))));
        CASE(CDR,   push(s, adup(cdr(*(s->sp)))));
        CASE(POP,   a = pop(s); destroy_atom (a, 1));
        CASE(QUOTE, push(s, cdr(*(s->sp))));
        CASE(ATOM,  push(s, isatom(*(s->sp))));
        //CASE(COND,  push(s, );
                default:
                error("UNKNOWN OPCODE: 0x%x\n", (*c)[0]);
    }
    (*c)++;
}

void run(state_t *s, char *prog) {
    char *c = prog;
    atom_t *env = nil;
    while(c[0] != EOP) {
        eval(s, &c);
    }
}

int main(void) {
    // set nil
    atom_t a = {.type = CONS, .car = NULL, .cdr = NULL};
    nil = &a;

    state_t s = init_state();
    char *prog =
        // (def a 312)
        "\x5"           // push nil
        "\x2" "312\x0"  // push num
        "\x3"           // push cons
        "\x1" "a\x0"    // push sym
        "\x3"           // push cons
        "\x1" "def\x0"  // push sym
        "\x3"           // push cons

        // a
        "\x1" "a\x0"

        // (if 'a 'a 'b)
        "\x5"
        "\x1" "b\x0"
        "\x3"
        "\x1" "a\x0"
        "\x3"
        "\x5"
        "\x3"
        "\x1" "if\x0"
        "\x3"
        "\x7f";

    run(&s, prog);
    dump_state(&s);
    destroy_state(&s);
    return 0;
}
