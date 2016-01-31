#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>

// taken from stb
void stb_replaceinplace(char *src, char *find, char *replace)
{
    size_t len_find = strlen(find);
    size_t len_replace = strlen(replace);
    int delta;

    char *s,*p,*q;

    delta = len_replace - len_find;
    assert(delta <= 0);
    if (delta > 0) return;

    p = strstr(src, find);
    if (p == NULL) return;

    s = q = p;
    while (*s) {
        memcpy(q, replace, len_replace);
        p += len_find;
        q += len_replace;
        s = strstr(p, find);
        if (s == NULL) s = p + strlen(p);
        memmove(q, p, s-p);
        q += s-p;
        p = s;
    }
    *q = 0;
}

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

//// internal

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
    return a->car;
}

atom *cdr(atom *a, atom *env) {
    return a->cdr;
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

char *nexttok(char **p) {
    return strsep(p, " ");
}

atom *parse(char **p) {
    atom *a, *b;
    char *t = nexttok(p);
    if(t == NULL) return NULL;
    switch(t[0]) {
        case ')':
            return NULL;
        case '(':
            a = parse(p);
            b = parse(p);
            return ncons(a, b);
        default:
            a = natom(t);
            b = parse(p);
            return ncons(a, b);
    }
}

int main(void) {
    atom *env = ncons(ncons(natom("quote"),nffi(&quote)), NULL);
    env = append(env, ncons(natom("atom?"), nffi(&is_atom)));
    env = append(env, ncons(natom("eq?"), nffi(&eq)));
    env = append(env, ncons(natom("car"), nffi(&car)));
    env = append(env, ncons(natom("cdr"), nffi(&cdr)));
    env = append(env, ncons(natom("cons"), nffi(&cons)));
    env = append(env, ncons(natom("cond"), nffi(&cond)));

    char *a = strdup("( atom? c )");
    atom *r = parse(&a);
    P(r);
    P(eval(r, env));
}
