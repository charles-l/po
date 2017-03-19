#ifndef TYPES_H
#define TYPES_H

#define NIL (void *) 0x1

typedef struct {
    void *car;
    void *cdr;
} __attribute__((packed)) cons;

typedef struct lstr {
    unsigned short n;
    char str[];
} __attribute__((packed)) lstr;

// sym table

typedef struct {
    char *key;
    cons *vals;
} bucket;

typedef struct {
    short size;
    bucket *table;
} sym_table;

#endif // TYPES_H
