#include <stdio.h>
#include "symtab.h"
#include "types.h"

sym_table *symtab;

extern void po_entry();

int main() {
    sym_table t = st_create(256);
    symtab = &t;
    po_entry();
}
