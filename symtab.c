// TODO: write the rest of this
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#define NIL (void *) 0x1

typedef struct {
    void *car;
    void *cdr;
} cons;

typedef struct {
    char *key;
    cons *vals;
} bucket;

typedef struct {
    short size;
    bucket *table;
} sym_table;

typedef struct lstr {
    unsigned short n;
    char *str;
} lstr;

char *lstr_to_cstr(lstr *l) {
    char *c = malloc(l->n + 1);
    memcpy(c, l->str, l->n);
    c[l->n] = 0;
    return c;
}

lstr cstr_to_lstr(char *c) {
    size_t s = strlen(c);
    lstr l = {(short) s, strndup(c, s)};
    return l;
}

sym_table st_create(unsigned short size) {
    sym_table t;
    t.size = size;
    t.table = calloc(size, sizeof(bucket));
    return t;
}

unsigned int st_hash(sym_table *t, lstr *k) {
    // djb2
    unsigned long hash = 5381;
    for(int i = 0; i < k->n; i++)
        hash = ((hash << 5) + hash) + k->str[i];
    return hash % t->size;
}

void* st_get_or_set(sym_table *t, lstr *k) {
    int bin = st_hash(t, k);
    bucket *b = &t->table[bin];

    if(b->vals == NULL) {
        b->vals = NIL;
    }

    for(cons *c = b->vals; c != NIL; c = c->cdr) {
        if(strncmp((char *) c->car, k->str, k->n) == 0)
            return (void *) c->car;
    }

    cons *n = malloc(sizeof(cons));
    n->car = k;
    n->cdr = b->vals;
    b->vals = n;
    return n->car;
}

int main() {
    sym_table t = st_create(256);
    lstr k = cstr_to_lstr("test");
    printf("%p\n", st_get_or_set(&t, &k));
    printf("%p\n", st_get_or_set(&t, &k));
    char *x = lstr_to_cstr(st_get_or_set(&t, &k));
    printf("%s\n", x);
}
