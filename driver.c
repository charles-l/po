#include <stdio.h>

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

int main() {
    int val = scheme_entry();
    //printf("D: %d\n", val);
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
    }
    return 0;
}
