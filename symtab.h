#ifndef SYMTAB_H
#define SYMTAB_H

#include "types.h"

sym_table st_create(unsigned short size);
void* st_get_or_set(sym_table *t, lstr *k);

#endif // SYMTAB
