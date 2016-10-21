#include <stdio.h>
#include <stdlib.h>

#define HEAP_SIZE (16 * 1024 * 1024) // normal page size

// immediate data types

#define fixnum_mask    3
#define fixnum_tag     0
#define fixnum_shift   2

#define char_mask    255
#define char_tag      15
#define char_shift     8

#define boolean_mask 127
#define boolean_tag   31
#define boolean_shift  7

#define null_mask    255
#define null_tag      47
#define null_shift     0

// heap data types

#define heap_mask      7
#define heap_shift     3

#define pair_tag       1
#define vector_tag     2
#define string_tag     3
#define symbol_tag     5
#define closure_tag    6

//

//assumes little endian
void print_bits(size_t const size, void const * const ptr)
{
    unsigned char *b = (unsigned char*) ptr;
    unsigned char byte;
    int i, j;

	for (i=size-1;i>=0;i--)
	{
		for (j=7;j>=0;j--)
		{
			byte = (b[i] >> j) & 1;
			printf("%u", byte);
		}
	}
	puts("");
}

void print_val(int val) {
    if((val & fixnum_mask) == fixnum_tag) {
        printf("%d\n", val >> fixnum_shift);
    } else if((val & char_mask) == char_tag) {
        printf("#\\%c\n", val >> char_shift);
    } else if((val & boolean_mask) == boolean_tag) {
        int b = val >> boolean_shift;
        if(b)
            puts("#t");
        else
            puts("#f");
    } else if((val & null_mask) == null_tag) {
        puts("'()");
    } else {
        // probably a heap value
        if((val & heap_mask) == pair_tag)
            printf("PAIR");
        else if((val & heap_mask) == vector_tag)
            printf("VECT");
        else if((val & heap_mask) == string_tag)
            printf("STR");
        else if((val & heap_mask) == symbol_tag)
            printf("SYM");
        else if((val & heap_mask) == closure_tag)
            printf("CLOSURE");
        else {
            val &= heap_mask;
            print_bits(sizeof(val), &val);
        }
        printf("(0x%x)\n", val);
        print_val(*((char *) val - 1));
        print_val(*((char *) val + 3));
        //print_bits(sizeof(int), ((char *) val - 1));
        //print_bits(sizeof(int), ((char *) val + 3));
    }
}

int main() {
    void *heap = malloc(HEAP_SIZE);
    int val = scheme_entry(heap);
    print_val(val);
    free(heap);
    return 0;
}
