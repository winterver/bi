#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define isdigit(c)                      \
        ('0' <= (c) && (c) <= '9')
#define isalpha(c)                      \
        (('a' <= (c) && (c) <= 'z') ||  \
         ('A' <= (c) && (c) <= 'Z') ||  \
         ('_' == (c)))
#define isalnum(c)                      \
        (('a' <= (c) && (c) <= 'z') ||  \
         ('A' <= (c) && (c) <= 'Z') ||  \
         ('0' <= (c) && (c) <= '9') ||  \
         ('_' == (c)))
#define error(...)                      \
        { printf("(%d) ", no);          \
          printf(__VA_ARGS__);          \
          putchar('\n');                \
          exit(-1); }

const char  *p;
int         tk, no;
intptr_t    val;
intptr_t    *e, *le;
char        *data, *ldata;
intptr_t    *id, *sym;
int         loc;

int prefix(const char* s) {
    const char* pp = p;
    while (*p && *p == *s) { p++; s++; }
    if (*s == 0) { return 1; }
    else { p = pp; return 0; }
}

int idcmp(const char* s1, const char* s2) {
    while (isalnum(*s1) && isalnum(*s2) && *s1 == *s2) { s1++; s2++; }
    return !isalnum(*s1) && !isalnum(*s2);
}

enum {
    Num = 128, Id, Extrn, Auto, If, Else, While, Switch, Case, Goto, Return,
    Assign, Cond, Or, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul,
    Div, Mod, Inc, Dec, Brak,
};

enum { Name, Addr, HName, HAddr, Idsz };

void next() {
    while (tk = *p) {
        if (strchr(" \t\r", *p)) { p++; }
        else if (strchr("\n", *p)) { p++; no++; }
        else if (prefix("/*")) { while (*p && !prefix("*/")) { no += *p++ == '\n'; } }
        else if (prefix("<<")) { tk = Shl; return; }
        else if (prefix(">>")) { tk = Shr; return; }
        else if (prefix("<=")) { tk = Le; return; }
        else if (prefix(">=")) { tk = Ge; return; }
        else if (prefix("!=")) { tk = Ne; return; }
        else if (prefix("==")) { tk = Eq; return; }
        else if (prefix("++")) { tk = Inc; return; }
        else if (prefix("--")) { tk = Dec; return; }
        else if (prefix("+")) { tk = Add; return; }
        else if (prefix("-")) { tk = Sub; return; }
        else if (prefix("*")) { tk = Mul; return; }
        else if (prefix("/")) { tk = Div; return; }
        else if (prefix("%")) { tk = Mod; return; }
        else if (prefix("&")) { tk = And; return; }
        else if (prefix("|")) { tk = Or; return; }
        else if (prefix("<")) { tk = Lt; return; }
        else if (prefix(">")) { tk = Gt; return; }
        else if (prefix("[")) { tk = Brak; return; }
        else if (prefix("?")) { tk = Cond; return; }
        else if (prefix("=")) { tk = Assign; return; }
        else if (strchr("!](){},:;", *p)) { p++; return; }
        else if (isalpha(*p)) {
            const char* pp = p;
            while (isalnum(*p)) { p++; }
            if (idcmp("extrn", pp)) { tk = Extrn; return; }
            else if (idcmp("auto", pp)) { tk = Auto; return; }
            else if (idcmp("if", pp)) { tk = If; return; }
            else if (idcmp("else", pp)) { tk = Else; return; }
            else if (idcmp("while", pp)) { tk = While; return; }
            else if (idcmp("switch", pp)) { tk = Switch; return; }
            else if (idcmp("case", pp)) { tk = Case; return; }
            else if (idcmp("goto", pp)) { tk = Goto; return; }
            else if (idcmp("return", pp)) { tk = Return; return; }
            tk = Id;
            for (id = sym; id[Name]; id += Idsz) {
                if (idcmp((const char*)id[Name], pp)) { return; }
            }
            id[Name] = (intptr_t)pp;
            return;
        }
        else if (isdigit(*p)) {
            int b = *p == '0' ? 8 : 10;
            for (val = 0; isdigit(*p); p++) {
                val = val * b + *p - '0';
            }
            tk = Num;
            return;
        }
        else if (tk == '\'' || tk == '"') {
            p++;
            const char* pp = data;
            while (*p != 0 && *p != tk) {
                if ((val = *p++) == '*') {
                    if (*p == '0') { val = '\0'; }
                    else if (*p == 'e') { val = '\0'; }
                    else if (*p == '(') { val = '{'; }
                    else if (*p == ')') { val = '}'; }
                    else if (*p == 't') { val = '\t'; }
                    else if (*p == '*') { val = '*'; }
                    else if (*p == '\'') { val = '\''; }
                    else if (*p == '\"') { val = '\"'; }
                    else if (*p == 'n') { val = '\n'; }
                    else { error("unknown escape sequence *%c", *p); }
                    if (*++p != '\'' && tk == '\'') { error("bad char literal"); }
                }
                if (tk == '"') *data++ = val;
            }
            if (*p++ == 0) { error("unexpected EOF"); }
            if (tk == '"') { val = (intptr_t)pp; }
            else { tk = Num; }
            return;
        }
        else { error("unknown character %c ascii(%d)", *p, *p); }
    }
}

void compile(const char* src) {
    no = 1;
    p = src;
    next();
    while (tk) {
        printf("%c tk(%d) val(%ld)\n", tk, tk, val);
        next();
    }
}

int main(int argc, char** argv) {
    if (argc < 2) { printf("usage: bi <source.b>\n"); return -1; }
    FILE* f = fopen(argv[1], "r");
    if (f == NULL) { printf("no such file: %s\n", argv[1]); return -1; }

    fseek(f, 0, SEEK_END);
    int len = ftell(f);
    fseek(f, 0, SEEK_SET);

    char* buf = malloc(len+1);
    fread(buf, 1, len, f);
    buf[len] = 0;
    fclose(f);

    e = le = malloc(10 * 1024 * sizeof(intptr_t));
    data = ldata = malloc(20 * 1024 * sizeof(intptr_t));
    id = sym = malloc(1024 * Idsz * sizeof(intptr_t));

    memset(e, 0, 10 * 1024 * sizeof(intptr_t));
    memset(data, 0, 20 * 1024 * sizeof(intptr_t));
    memset(sym, 0, 1024 * Idsz * sizeof(intptr_t));

    compile(buf);
}
