#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct obj {
    enum {NUM, SYM, STR, CONS} type;
    union {
        double num;
        char *sym;
        char *str; // TODO: probably should use a smarter string obj here
        struct {
            struct obj *car;
            struct obj *cdr;
        };
    };
} obj;

typedef struct {
    char *p; // position in string
    char o;  // old character
} holder;

obj true  = {SYM,  .sym = "t"};
obj false = {SYM,  .sym = "f"};
obj empty = {CONS, .car = NULL, .cdr = NULL};

void freeobj(obj *o) {
    if(!o || o == &true || o == &false || o == &empty) return;
    if(o->type == SYM)
        free(o->sym);
    if(o->type == STR)
        free(o->str);
    free(o);
}

void printobj(obj *o) {
    if(o->type == NUM)
        printf("%lf\n", o->num);
    else if(o->type == SYM)
        printf("#%s\n", o->sym);
    else if(o->type == STR)
        printf("\"%s\"\n", o->str);
    else if(o->type == CONS)
        if(o == &empty)
            printf("()\n");
}

char *dupword(char *s) {
    char *r = malloc(16); // longest token size.... LOLZ
    int i = 0;
    while(isalnum(s[0])) {
        r[i] = s[0];
        i++; s++;
    }
    r[i++] = '\0';
    return r;
}

void resetstr(char *s, holder h) {
    *(h.p) = h.o;
}

obj *parse_word(char *s) {
    obj *o = malloc(sizeof(obj));
    if((s[0] == '-' && isdigit(s[1])) || isdigit(s[0])) {
        o->type = NUM;
        o->num = (double) atoi(s);
    } else if (s[0] == '"') {
        o->type = STR;
        char *m = strchr(s + 1, '"');
        if(m) {
            unsigned long n = strchr(s + 1, '"') - s - 1;
            o->str = malloc(n + 1);
            strncpy(o->str, s + 1, n);
        } else {
            o->str = "";
            // TODO: throw error
        }
    } else if (strncmp(s, "()", 2) == 0) {
        free(o);
        return &empty;
    } else {
        o->type = SYM;
        char *w = dupword(s);
        int t = (strcmp(w, "t") == 0);
        int f = (strcmp(w, "f") == 0);
        if(t || f) {
            free(w);
            free(o);
            if(t) return &true;
            if(f) return &false;
        }
        o->sym = w;
    }
    return o;
}

char *prevwrd(char *e) { // get word from right of string (and move null terminator)
    while(isspace(e[0])) e--;
    e[1] = '\0';
    while(!isspace(e[0]))
        if(e[0] == '(')
            return e + 1;
        else
            e--;
    e++; // bump back space
    return e;
}

char *rempar(char *s) { // remove right paren ')'
    char *e = strrchr(s, ')');
    e[0] = '\0';
    e--;
    return e;
}

void backchar(char **s) {
    (*s)--;
    **s = '\0';
}

obj *parse(char *s) {
    if(s[0] == '(') {
        char *e = rempar(s); // remove right paren and get end of str
        while(1) {
            char *m = prevwrd(e);
            puts(m);
            if(m == s + 1) break;
            backchar(&m);
        }
    } else {
        return parse_word(s);
    }
    return NULL;
}

obj *eval (obj *p) {
    return p;
}

int main() {
    char buf[128] = "";
    do {
        parse(buf);
        obj *o = eval(parse_word(buf));
        printobj(o);
        //freeobj(o);
        printf("> ");
    } while(fgets(buf, 128, stdin));
    return 0;
}
