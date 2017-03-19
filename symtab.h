#ifndef SYMTAB_H
#define SYMTAB_H

#include "types.h"

sym_table st_create(unsigned short size);
void* st_get_or_set(lstr *k);

#endif // SYMTAB
