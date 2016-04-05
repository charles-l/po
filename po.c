#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct obj {
    enum {NUM, SYM, STR, CH, CONS} type;
    union {
        double num;
        char *sym;
        char *str; // TODO: probably should use a smarter string obj here
        char ch;
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
    else if(o->type == CH)
        printf("%c\n", o->ch);
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

char *endstr(char *s) {
    return s + (strlen(s) - 1);
}

char *endpar(char *s) {
    char *e = endstr(s);
    while(e[0] != ')')
    if(e == s)
        return NULL;
    else
        e--;
    return e;
}

obj *parse_word(char *s) {
    puts(s);
    obj *o = malloc(sizeof(obj));
    if((s[0] == '-' && isdigit(s[1])) || isdigit(s[0])) {
        o->type = NUM;
        o->num = (double) atoi(s);
    } else if (s[0] == '#') {
        if(s[1] == '\\') {
            o->type = CH;
            char *w = dupword(s + 2);
            if(strcmp(w, "newline") == 0)
                o->ch = '\n';
            else if (strcmp(w, "space") == 0)
                o->ch = ' ';
            else
                o->ch = w[0];
            free(w);
        } else {
            o->type = SYM;
            char *w = dupword(s + 1);
            if(strcmp(w, "t") == 0) {
                free(w);
                free(o);
                return &true;
            }
            if(strcmp(w, "f") == 0) {
                free(w);
                free(o);
                return &false;
            }
            o->sym = w;
        }
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
    } else {
        free(o);
        o = &empty;
    }
    return o;
}

char *rgetword(char *s) { // get word from right of string
    char *e = endpar(s);
    e--;
    e[1] = '\0'; // remove paren
    if(e == s) return NULL; // catch ')' <- TODO: test
    while(isalnum(e[0]) && e[-1] != '(') e--;
    return e;
}


obj *parse(char *s) {
    if(s[0] == '(') {
        char *e = strrchr(s, ')');
        puts(rgetword(s));
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
        //obj *o = eval(parse_word(buf));
        //printobj(o);
        //freeobj(o);
        printf("> ");
    } while(fgets(buf, 128, stdin));
    return 0;
}
