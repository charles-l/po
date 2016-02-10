#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <err.h>

#define MAX_VAR_LEN 16
#define GUARD(cond, ...) if(!(cond)) {errx(EXIT_FAILURE, "" __VA_ARGS__);}

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

atom nil = {.type = ATOM, .car = &nil, .cdr = &nil};
atom tee = {.type = ATOM, .sym = "t"};

void print_atom(atom *a) {
    if(a == &nil) {fprintf(stderr, "<nil>"); return;}
    switch(a->type) {
        case ATOM:
            fprintf(stderr, "%s", a->sym);
            break;
        case CONS:
            fprintf(stderr, "(");
            print_atom(a->car);
            fprintf(stderr, " ");
            print_atom(a->cdr);
            fprintf(stderr, ")");
            break;
        case LAMBDA:
            fprintf(stderr, "<l: %p>", a);
            break;
        case FFI:
            fprintf(stderr, "<ffi: %p>", a->func);
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
    if(a == NULL || a == &tee || a == &nil) return;
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

atom *is_atom(atom *a, atom *env) {
    if(a == &nil || a->car == &nil) return &nil;
    if(a->car->type == ATOM) { return &tee; }
    return &nil;
}

int equal(atom *a, atom *b) {
    if(a == &nil && b == &nil) return 1;
    if(a->type != b->type) return 0;
    switch(a->type) {
        case ATOM:
            if(strcmp(a->sym, b->sym) == 0) return 1;
        case CONS:
            if(equal(a->car, b->car))
                return equal(a->cdr, b->cdr);
            else
                return 0;
    }
    // fallback: equal pointer?
    return a == b;
}

atom *eq(atom *a, atom *env) {
    if(equal(a->car, a->cdr->car))
        return &tee;
    return &nil;
}

atom *cons(atom *l, atom *env) {
    return ncons(l->car, l->cdr->car);
}

atom *car(atom *a, atom *env) {
    return a->car->car;
}

atom *cdr(atom *a, atom *env) {
    return a->car->cdr;
}

//// utility

atom *lookup(atom *a, char *nm) {
    if(a == &nil || a->car == &nil) return &nil;
    if(a->car->car->type == ATOM && strcmp(a->car->car->sym, nm) == 0)
        return a->car->cdr;
    if(a->cdr != &nil)
        return lookup(a->cdr, nm);
    return &nil;
}

void append(atom **l, atom *a) {
    atom *p = *l;
    while ((*l)->cdr != &nil) {
        *l = (*l)->cdr;
    }
    (*l)->cdr = ncons(a, &nil);
    *l = p;
}

atom *splice(atom *a, atom *b) {
    // this is crap
    if(a->car == &nil || b->car == &nil) return &nil; // (<nil>, <nil>) instead?
    atom *r = ncons(ncons(a->car, b->car), &nil);
    while(a->cdr != &nil && b->cdr != &nil) {
        a = a->cdr;
        b = b->cdr;
        append(&r, ncons(a->car, b->car));
    }
    return r;
}

atom *eval(atom *sexp, atom *env);
atom *call_lam(atom *args, atom *env) {
    // also crap
    atom *lam = args->car;
    args = args->cdr;
    atom *list = splice(lam->car, args);
    append(&env, list->car);
    return eval(lam->cdr, env);
}

atom *eval_fn(atom *sexp, atom *env) {
    atom *s = sexp->car;
    atom *a = sexp->cdr;
    if(s->type == LAMBDA) {
        return call_lam(sexp, env);
    } else if(s->type == FFI){
        return(s->func)(a, env);
    } else {
        // TODO: think of better error handling
        fprintf(stderr, "symbol unbound: ");
        P(sexp->car);
        return &nil; // error
    }
}

atom *eval(atom *sexp, atom *env) {
    if(sexp == &nil)
        return &nil;

    if(sexp->type == CONS) {
        if(sexp->car->type == ATOM && strcmp(sexp->car->sym, "lam") == 0) {
            return nlambda(sexp->cdr->car, sexp->cdr->cdr->car);
        } else if (sexp->car->type == ATOM && strcmp(sexp->car->sym, "quote") == 0) {
            return sexp->cdr->car;
        } else if (sexp->car->type == ATOM && strcmp(sexp->car->sym, "def") == 0) {
            // TODO: cleanup
            atom *a;
            if((a = lookup(env, sexp->cdr->car->sym)) != &nil) {
                *a = *(sexp->cdr->cdr->car);
            }
            if(sexp->cdr->cdr->car->car->type == ATOM &&
                    strcmp(sexp->cdr->cdr->cdr->car->car->sym, "lam"))
            {
                append(&env, ncons(sexp->cdr->car, eval(sexp->cdr->cdr->car, env)));
            } else {
                append(&env, ncons(sexp->cdr->car, sexp->cdr->cdr->car));
            }
            return &nil;
        } else if (sexp->car->type == ATOM && strcmp(sexp->car->sym, "cond") == 0) {
            atom *l = sexp->cdr;
            if(l == &nil) return &nil;
            if(l->car != &nil) {
                if (eval(l->car->car, env) == &tee) return eval(l->car->cdr->car, env);
            }
            while((l = l->cdr) != &nil) {
                if (eval(l->car->car, env) == &tee) return eval(l->car->cdr->car, env);
            }
            return &nil;
        } else {
            atom *a = ncons(eval(sexp->car, env), &nil);
            sexp = sexp->cdr;

            while (sexp != &nil && sexp->type == CONS){
                append(&a, eval(sexp->car,env));
                sexp = sexp->cdr;
            }

            return eval_fn(a, env);
        }
    } else {
        atom *v = lookup(env, sexp->sym);
        if(v == &nil) {
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
        case '\0':
        case EOF:
            return NULL;
        case '\n':
        case '\t':
        case ' ':
            NEXTCHAR;
            return nexttok(p);
        case '(':
            NEXTCHAR; return "(";
        case ')':
            NEXTCHAR; return ")";
        default:
            // THIS IS SO CRAP I CAN'T EVEN
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
    if(!t) {
        fprintf(stderr, "unexpected symbol '%s'\n", t);
        return &nil;
    }

    switch(t[0]) {
        case ')':
            return &nil;
        case '(':
            a = parse_rest(p);
            b = parse_rest(p);
            return ncons(a, b);
        default:
            a = natom(t);
            b = parse_rest(p);
            return ncons(a, b);
    }
}

atom *parse(char *p) {
    char *t = nexttok(&p);
    if(t == NULL || t[0] == '\0') return &nil;
    if(t[0] == ' ') return parse(p);
    if(t[0] == '(') {
        return parse_rest(&p);
    }
    return natom(t);
}

int main(int ac, char **av) {
    atom *env = ncons(ncons(natom(strdup("atom?")), nffi(&is_atom)), &nil);
    append(&env, ncons(natom(strdup("eq?")), nffi(&eq)));
    append(&env, ncons(natom(strdup("car")), nffi(&car)));
    append(&env, ncons(natom(strdup("cdr")), nffi(&cdr)));
    append(&env, ncons(natom(strdup("cons")), nffi(&cons)));

#define MAX_LINE_LEN 128
    char *p = malloc(MAX_LINE_LEN);
    FILE *f = stdin;

    if(ac > 1) {
#ifdef __linux__
        // TODO: move this ifdef somewhere else to abstract the problem
        GUARD(access(av[1], F_OK) != -1, "file '%s' does not exist", av[1]);
#elif _WIN32
#error "no support for files on windows yet"
#else
#error "no support for your os yet"
#endif
        f = fopen(av[1], "r");
    }

    while(fgets(p, MAX_LINE_LEN, f) != NULL) {
        atom *r = parse(p);
        atom *s = eval(r, env);
        P(env);
        P(s);
    }

    //TODO: figure out how to clean up. Maybe track in adup or malloc??
    //adel(r);
    //adel(s);

    free(p);
    adel(env);
    fclose(f);
}
