// TODO: generalize this code a bit so it can be used for more than
// just the sym table (i.e. allow full hash tables in the lang itself)
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "symtab.h"
#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))

extern sym_table *symtab;

// TODO: test that this works
char *lstr_to_cstr(lstr *l) {
    char *c = malloc(l->n + 1);
    memcpy(c, l + offsetof(lstr, str), l->n);
    c[l->n] = 0;
    return c;
}

// TODO: test that this works
lstr *cstr_to_lstr(char *c) {
    size_t s = strlen(c);
    lstr *l = malloc(sizeof(short) + sizeof(char) * s);
    return l;
}

sym_table st_create(unsigned short size) {
    sym_table t;
    t.size = size;
    t.table = calloc(size, sizeof(bucket));
    return t;
}

unsigned int st_hash(lstr *k) {
    // djb2
    unsigned long hash = 5381;
    for(int i = 0; i < k->n; i++)
        hash = ((hash << 5) + hash) + k->str[i];
    return hash % symtab->size;
}

void* st_get_or_set(lstr *k) {
    int bin = st_hash(k);
    bucket *b = &(symtab->table[bin]);

    if(b->vals == NULL) {
        b->vals = NIL;
    }

    for(cons *c = b->vals; c != NIL; c = c->cdr) {
        lstr *l = ((lstr *) c->car);
        if(strncmp(l->str, k->str, MIN(l->n, k->n)) == 0)
            return (void *) c->car;
    }

    cons *n = malloc(sizeof(cons));
    n->car = k;
    n->cdr = b->vals;
    b->vals = n;
    return n->car;
}
