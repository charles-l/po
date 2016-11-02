#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
//#define DEBUG

#define HEAP_SIZE (16 * 1024 * 1024) // normal page size

// immediate data types

#define fixnum_mask    3
#define fixnum_shift   2

#define char_mask    255
#define char_shift     8

#define boolean_mask 127
#define boolean_shift  7

#define null_mask    255
#define null_shift     0

// heap data types

#define heap_mask      7
#define heap_shift     3

#define ENUM_LABEL(v) [v] = #v

typedef enum {
    FIXNUM     = 0,
    CHAR       = 15,
    BOOL       = 31,
    PNULL      = 47,

    // heap types
    PAIR       = 1,
    VECTOR     = 2,
    STRING     = 3,
    SYMBOL     = 5,
    CLOSURE    = 6,
} tags;

char *tags_label[] = {
    ENUM_LABEL(FIXNUM),
    ENUM_LABEL(CHAR),
    ENUM_LABEL(BOOL),
    ENUM_LABEL(PNULL),
    ENUM_LABEL(PAIR),
    ENUM_LABEL(VECTOR),
    ENUM_LABEL(STRING),
    ENUM_LABEL(SYMBOL),
    ENUM_LABEL(CLOSURE),
};


typedef int po_immediate;
extern po_immediate scheme_entry(void *h);

//

//assumes little endian
void print_bits(size_t const size, void const * const ptr)
{
    unsigned char *b = (unsigned char*) ptr;
    unsigned char byte;
    int i, j;

    for (i=size-1;i>=0;i--) {
        for (j=7;j>=0;j--) {
            byte = (b[i] >> j) & 1;
            fprintf(stderr, "%u", byte);
        }
    }
    puts("");
}

po_immediate car(po_immediate val) {
    assert((val & heap_mask) == PAIR);
    // DOOD THIS CASTING IS HARD
    return *((po_immediate *) ((unsigned char *) val - 1));
}

po_immediate cdr(po_immediate val) {
    assert((val & heap_mask) == PAIR);
    return *((po_immediate *) ((unsigned char *) val + 3));
}

unsigned int len(po_immediate val) {
    assert((val & heap_mask) == STRING || (val & heap_mask) == VECTOR);
    int shift = (val & heap_mask);
    return *((po_immediate *) ((unsigned char *) val - shift));
}

char string_ref(po_immediate val, unsigned int i) {
    assert((val & heap_mask) == STRING);
    return *((char *) ((unsigned char *) val - STRING + 4 + i));
}

po_immediate vector_ref(po_immediate val, unsigned int i) {
    return *((unsigned char *) val + i + 1);
}

void dump_heap(void *heap) {
    for(int i = 0; i < 20; i++) {
        int t = ((po_immediate *) heap)[i] & heap_mask;
        fprintf(stderr, "<%-6s> ", tags_label[t]);
        print_bits(sizeof(po_immediate), ((po_immediate *) heap + i));
    }
}

void print_val(po_immediate val) {
    if((val & fixnum_mask) == FIXNUM) {
        printf("%d", val >> fixnum_shift);
    } else if((val & char_mask) == CHAR) {
        printf("#\\%c", val >> char_shift);
    } else if((val & boolean_mask) == BOOL) {
        int b = val >> boolean_shift;
        if(b)
            printf("#t");
        else
            printf("#f");
    } else if((val & null_mask) == PNULL) {
        printf("'()");
    } else {
        // probably a heap value
        if((val & heap_mask) == PAIR) {
            printf("(");
            print_val(car(val));
            printf(".");
            print_val(cdr(val));
            printf(")");
        } else if((val & heap_mask) == VECTOR) {
            printf("#()");
            printf("\nlen: %i", len(val));
        } else if((val & heap_mask) == STRING) {
            putchar('"');
            for(int i = 0; i < len(val); i++) {
                putchar(string_ref(val, i));
            }
            putchar('"');
        } else if((val & heap_mask) == SYMBOL)
            printf("SYM");
        else if((val & heap_mask) == CLOSURE)
            printf("CLOSURE");
        else {
            val &= heap_mask;
            print_bits(sizeof(val), &val);
        }
        //printf("(0x%x)\n", val);
        //print_bits(sizeof(int), ((char *) val - 1));
        //print_bits(sizeof(int), ((char *) val + 3));
    }
}

int main() {
    int *heap = malloc(HEAP_SIZE);
    po_immediate val = scheme_entry(heap);
#ifdef DEBUG
    dump_heap(heap);
#endif
    print_val(val);
    putchar('\n');
    free(heap);
    return 0;
}
