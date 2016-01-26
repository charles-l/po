#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#define STACK_SIZE 1024
#define error(args...) do { fprintf(stderr, args); exit(1); } while (0)

// TODO: <s>grow stack on demand (maybe?)</s> MAKE THE ENTIRE STACK AN ALIST THAT IT CAN REWRITE AT RUNTIME!!!
// TODO: JIT for blazing fast speedz
// TODO: replace labels with offset addresses
// TODO: error type

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
    CAR   = 0xA,
    CDR   = 0xB,
    CALL  = 0xC, // call the top of the stack

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
        int num;
        char *sym;
        struct {
            struct atom *car, *cdr;
        };
    };
} atom_t;

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
   // free(s->atoms);
    //free(s);
    s = NULL;
}

void push(state_t *s, atom_t *v) {
    memcpy(s->sp, v, sizeof(atom_t));
    s->sp++;
}

atom_t *pop(state_t *s) {
    // TODO: clean up allocated shtuff here
    s->sp--;
    return s->sp;
}

atom_t *adup(atom_t *a) {
    atom_t *r = malloc(sizeof(atom_t));
    memcpy(r, a, sizeof(atom_t));
    return r;
}

#define GAP " "

void print_atom(atom_t *a) {
    switch(a->type) {
        case SYM:
            printf("'%s'", a->sym);
            break;
        case NUM:
            printf("%i", a->num);
            break;
        case CONS:
            if(a->car == NULL) {
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
    for(atom_t *o = s->atoms; o < s->sp; o++)
    {
        printf("%08p" GAP, o);
        print_atom(o);
        printf("\n");
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
    push(s,&t);

#define MEMDUP(dest, src, sz) dest = malloc(sz); memcpy(dest, src, sz);

char *gettok(char **c) {
    char *tmp = strchr(*c, '\x0');
    size_t i = (size_t) (tmp - *c);
    char *v = malloc(i);
    memcpy(v, *c, i);
    char *r = strdup(v + 1);
    *c = tmp;
    return r;
}

atom_t *mksym (char *r) {
    atom_t a = {.type = SYM, .sym = r};
    return adup(&a);
}

atom_t *mknum(int v) {
    atom_t a = {.type = NUM, .num = v};
    return adup(&a);
}

atom_t *mkcons(atom_t *x, atom_t *y) {
    atom_t a = {.type = CONS, .car = adup(x), .cdr = adup(y)};
    //free(x);
    //free(y);
    return adup(&a);
}

atom_t *mknil() {
    atom_t a = {.type = CONS, .car = NULL, .cdr = NULL};
    return adup(&a);
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

int isnil(atom_t *t) {
    if (t == NULL) return 1;
    if (t->type == CONS) return t->car == NULL;
    return 0;
}

void tjump(state_t *s, char **p) {
    atom_t *a = pop(s);
    if(!isnil(a)) jump(s, p);
}

void fjump(state_t *s, char **p) {
    atom_t *a = pop(s);
    if(isnil(a)) jump(s, p);
}

void fficall(state_t *s, char **p) { /* TODO: implement */ }

#define TOPATOM (s->sp - 1)

atom_t *car(atom_t *a) {
    assert(a->type == CONS);
    return a->car;
}

atom_t *cdr(atom_t *a) {
    assert(a->type == CONS);
    return a->cdr;
}

atom_t *assoc(atom_t *key, atom_t *alist) {
    if(isnil(alist)) return alist;
    if(car(car(alist)) == key) return car(alist);
    return assoc(key, cdr(alist));
}

#define SYMEQ(a, s) (strcmp(a->sym, s) == 0)
#define setcar(X,Y)           (((X)->car) = (Y))
#define setcdr(X,Y)           (((X)->cdr) = (Y))

atom_t *insert(atom_t *alist, atom_t *e) {
    alist = mkcons(e, alist);
    return alist;
}

void list_dump(atom_t *alist) {
    puts("--- dumping list ---");
    for(; !isnil(alist->cdr); alist = alist->cdr)
        print_atom(alist->car);
    puts("--- end dump ---");
}

atom_t *eval(atom_t *e, atom_t *env) {
    atom_t *t;
    if(isnil(e)) return e;
    switch(e->type) {
        case NUM:
            return e;
        case SYM:
            t = assoc(e, env);
            if(isnil(t)) list_dump(env); error("unbound symbol '%s'\n", t->sym);
            return cdr(t);
        case CONS:
            // TODO: bootstrap anything in here
            if(SYMEQ(car(e), "def")) {
                insert(env, mkcons(car(cdr(e)), eval(car(cdr(cdr(e))), env)));
            }
    }
    return e;
}

#define CASE(en, b) \
    case en: \
b; \
break;

void run(state_t *s, char *prog) {
    char *c = prog;
    atom_t *env = mknil();
    insert(env, mkcons(mksym("tesT"), mknum(1)));
    list_dump(env);
    while(c[0] != EOP) {
        atom_t *a;
        atom_t *x;
        atom_t *y;
        char *t;
        switch(c[0]) {
            // TODO: do not reference prog directly
            CASE(NOP, NULL);
            CASE(':', c+=2); // skip label
            CASE(PUSHA, push(s, mksym(gettok(&c))));
            CASE(PUSHN, t = gettok(&c);
                    push(s, mknum(atoi(t)));
                    free(t));
            CASE(PUSHC,
                    x = pop(s);
                    y = pop(s);
                    push(s, mkcons(x, y)));
            CASE(PUSH0, push(s, mknil()));
            CASE(JUMP,  jump   (s, &c));
            CASE(TJUMP, tjump  (s, &c));
            CASE(FJUMP, fjump  (s, &c));
            CASE(FFI,   fficall(s, &c));
            CASE(CAR,   push(s, adup(car(TOPATOM))));
            CASE(CDR,   push(s, adup(cdr(TOPATOM))));
            CASE(CALL,  push(s, eval(TOPATOM, env)));
            CASE(POP,   free  (pop(s)));
            default:
            error("UNKNOWN OPCODE: 0x%x\n", c[0]);
        } c++;
    }
}

int main(void) {
    state_t *s = init_state();
    char *prog =
        // (def a 312)
        "\x5"                   // push nil
        "\x2" "312\x0"          // push num
        "\x3"                   // push cons
        "\x1" "a\x0"            // push sym
        "\x3"                   // push cons
        "\x1" "def\x0"          // push sym
        "\x3"                   // push cons
        "\xC"                   // call top

        // a
        "\x1" "a\x0"
//        "\xC"
        "\x7f";

    run(s, prog);
    dump_state(s);
    destroy_state(s);
    return 0;
}
