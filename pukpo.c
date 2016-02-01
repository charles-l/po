#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <err.h>

#define MAX_VAR_LEN 16

// adapted from: http://nakkaya.com/2010/08/24/a-micro-manual-for-lisp-implemented-in-c/

enum type {CONS, ATOM, LAMBDA, FFI};

typedef struct atom {
    enum type type;
    union {
        char *sym;
        struct atom *(*func)(struct atom *, struct atom *);
        struct {
            struct atom *car;
            struct atom *cdr;
        };
    };
} atom;

atom nil = {.type = ATOM, .car = NULL, .cdr = NULL};
atom tee = {.type = ATOM, .sym = "t"};

void print_atom(atom *a) {
    if(a == NULL) {printf("null"); return;}
    if(a == &nil) {printf("<nil>"); return;}
    switch(a->type) {
        case ATOM:
            printf("%s", a->sym);
            break;
        case CONS:
            printf("(");
            print_atom(a->car);
            printf(" ");
            print_atom(a->cdr);
            printf(")");
            break;
        case LAMBDA:
            printf("<l: %p>", a);
            break;
        case FFI:
            printf("<ffi: %p>", a->func);
            break;
    }
}

#define P(a) print_atom(a); printf("\n");

//// constructors

atom *adup(atom *a) {
    atom *r = malloc(sizeof(atom));
    memcpy(r, a, sizeof(atom));
    return r;
}

void adel(atom *a) {
    if(a == NULL) return;
    if(a->type == ATOM) {
        free(a->sym);
    } else if(a->type == CONS) {
        adel(a->car);
        adel(a->cdr);
    }
    free(a);
}

atom *natom(char *sym) { // assume sym is going to be sticking around
    atom r = (atom){.type = ATOM, .sym = sym};
    return adup(&r);
}
// as opposed to `cons`, this func is used internally
atom *ncons(atom *a, atom *b) {
    atom r = (atom){.type = CONS, .car = a, .cdr = b};
    return adup(&r);
}

atom *nffi(atom *(*func)(atom *, atom *)) {
    atom r = (atom){.type = FFI, .func = func};
    return adup(&r);
}

atom *nlambda(atom *args, atom *sexp) {
    atom r = (atom){.type = LAMBDA, .car = args, .cdr = sexp};
    return adup(&r);
}

//// builtins

atom *quote(atom *l, atom *env) {
    return l;
}

atom *is_atom(atom *a, atom *env) {
    if(a == NULL || a->car == NULL) return &nil;
    if(a->car->type == ATOM) { return &tee; }
    return &nil;
}

atom *eq(atom *a, atom *env) {
    if(a->car == NULL && a->cdr->car == NULL) return &tee;
    if(a->car != a->cdr->car) return &nil;
    switch(a->car->type) {
        case ATOM:
            if(strcmp(a->car->sym, a->cdr->car->sym) == 0) return &tee;
        case CONS:
            if(eq(a->car, a->cdr->car)) {
                return eq(a->cdr, a->cdr->cdr);
            }
    }
}

atom *cons(atom *l, atom *env) {
    return ncons(l->cdr, l->car);
}

atom *eval(atom *sexp, atom *env);
atom *cond(atom *l, atom *env) {
    if(l == NULL) return &nil;
    if(l->cdr) {
        if (eval(l->car, env)) return l->cdr;
    }
    while(l = l->cdr) {
        if(l->car == &tee) {
            return l->cdr;
        }
    }
    return &nil;
}

atom *car(atom *a, atom *env) {
    return a->car->car;
}

atom *cdr(atom *a, atom *env) {
    return a->car->cdr;
}

//// utility

atom *lookup(atom *a, char *nm) {
    if(a == NULL || a->car == NULL) return NULL;
    if(a->car->car->type == ATOM && strcmp(a->car->car->sym, nm) == 0)
        return a->car->cdr;
    if(a->cdr != NULL)
        return lookup(a->cdr, nm);
    return NULL;
}

atom *append(atom *l, atom *a) {
    atom *p = l;
    while (p->cdr != NULL) {
        p = p->cdr;
    }
    p->cdr = ncons(a, NULL);
    p = l;
    return ncons(l->car, p->cdr);
}

atom *eval_fn(atom *sexp, atom *env) {
    atom *s = sexp->car;
    atom *a = sexp->cdr;
    if(s->type == LAMBDA) {
        return nlambda(sexp, env);
    } else if(s->type == FFI){
        return(s->func)(a, env);
    } else {
        errx(1, "unbound symbol %s", sexp->car->car->sym);
    }
}

atom *eval(atom *sexp, atom *env) {
    if(sexp == NULL)
        return &nil;

    if(sexp->type == CONS) {
        if(sexp->car->type == ATOM && strcmp(sexp->car->sym, "lam") == 0) {
            return nlambda(sexp->car->cdr, sexp->cdr->cdr->car);
        } else {
            atom *a = ncons(eval(sexp->car, env), NULL);
            sexp = sexp->cdr;

            while (sexp != NULL && sexp->type == CONS){
                a = append(a, eval(sexp->car,env));
                sexp = sexp->cdr;
            }

            return eval_fn(a, env);
        }
    } else {
        atom *v = lookup(env, sexp->sym);
        if(v == NULL) {
            return sexp;
        } else {
            return v;
        }
    }
}

#define NEXTCHAR ((*p)++)
char *nexttok(char **p) {
    char *a;
    int i;
    switch(*p[0]) {
        case '\n':
        case '\t':
        case ' ':
            NEXTCHAR;
            return nexttok(p);
        case '(':
            NEXTCHAR; return strdup("(");
        case ')':
            NEXTCHAR; return strdup(")");
        default:
            a = malloc(MAX_VAR_LEN);
            i = 0;
            while(**p != '('
                    && **p != ')'
                    && **p != ' '
                    && **p != '\n'
                    && **p != '\t')
            {
                assert(i < MAX_VAR_LEN);
                a[i++] = **p;
                NEXTCHAR;
            }
            a[i] = '\0';
            return a;
    }
    return NULL;
}

atom *parse_rest(char **p) {
    atom *a, *b;
    char *t = nexttok(p);

    switch(t[0]) {
        case ')':
            free(t);
            return NULL;
        case '(':
            free(t);
            a = parse_rest(p);
            b = parse_rest(p);
            return ncons(a, b);
        default:
            a = natom(t);
            b = parse_rest(p);
            return ncons(a, b);
    }
}

atom *parse(char **p) {
    char *t = nexttok(p);
    if(t[0] == '(') {
        free(t);
        return parse_rest(p);
    }
    return natom(t);
}

int main(void) {
    atom *env = ncons(ncons(natom(strdup("quote")), nffi(&quote)), NULL);
    env = append(env, ncons(natom(strdup("atom?")), nffi(&is_atom)));
    env = append(env, ncons(natom(strdup("eq?")), nffi(&eq)));
    env = append(env, ncons(natom(strdup("car")), nffi(&car)));
    env = append(env, ncons(natom(strdup("cdr")), nffi(&cdr)));
    env = append(env, ncons(natom(strdup("cons")), nffi(&cons)));
    env = append(env, ncons(natom(strdup("cond")), nffi(&cond)));

    char *p = "(quote 1)";
    atom *r = parse(&p);
    atom *s = eval(r, env);

    P(s);

    adel(r);
    adel(s);
    adel(env);
}
