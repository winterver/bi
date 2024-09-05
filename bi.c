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
#define long int64_t

const char  *p;
int         tk, no;
long        val;
long        *e, *le;
long        *data, *ldata;
long        *id, *sym;
int         loc, call;

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

enum { Name, Addr, IsAuto, HAddr, HIsAuto, Idsz };

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
            id[Name] = (long)pp;
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
                if (*p != '\'' && tk == '\'') { error("bad char literal"); }
                if (tk == '"') { *(*(char**)&data)++ = val; }
            }
            if (*p++ == 0) { error("unexpected EOF"); }
            if (tk == '"') { val = (long)pp; }
            else { tk = Num; }
            return;
        }
        else { error("unknown character %c ascii(%d)", *p, *p); }
    }
}

void expr(int lev) {
    if (tk == Num) {
        *e++ = IMM;
        *e++ = val;
        next();
    }
    else if (tk == '"') {
        *e++ = IMM;
        *e++ = val;
        next();
        while (tk == '"') { next(); }
        data = (long*)
            ( (long)data 
            +  sizeof(long) 
            & -sizeof(long));
    }
    else if (tk == Id) {
        if (!id[Addr]) { error("undefined identifier"); }
        if (id[IsAuto]) { *e++ = LEA; *e++ = id[Addr]; }
        else { *e++ = IMM; *e++ = id[Addr]; }
        *e++ = LI;
        next();
    }
    else if (tk == '(') {
        expr(Assign);
        if (tk != ')') { error("')' expected"); }
        next();
    }
}

void stmt() {
    if (tk == ';') {
        next();
    }
    else if (tk == '{') {
        next();
        while (tk && tk != '}') { stmt(); }
        if (tk == 0) { error("unexpected EOF"); }
        next();
    }
    else if (tk == Extrn) {
        next();
        while (tk && tk != ';') {
            if (tk != Id) { error("bad extrn list"); }
            // TODO: currently does nothing
            next();
            if (tk == ',') { next(); }
        }
        if (tk == 0) { error("unexpected EOF"); }
        next();
    }
    else if (tk == Auto) {
        next();
        while (tk && tk != ';') {
            if (tk != Id) { error("bad auto list"); }
            if (id[IsAuto]) { error("duplicate auto variable"); }
            id[HAddr]   = id[Addr];   id[Addr]   = --loc;
            id[HIsAuto] = id[IsAuto]; id[IsAuto] = 1;
            next();
            if (tk == Num) {
                *e++ = LEA;
                *e++ = id[Addr];
                *e++ = PSH;
                *e++ = IMM;
                *e++ = val;
                *e++ = SI;
            }
            next();
            if (tk == ',') { next(); }
        }
        if (tk == 0) { error("unexpected EOF"); }
        next();
    }
    else if (tk == If) {
        next();
        if (tk == '(') { next(); } else { error("'(' expected"); }
        expr(Assign);
        if (tk == ')') { next(); } else { error("')' expected"); }
        *e++ = BZ;
        long* b = e++;
        stmt();
        if (tk == Else) {
            next();
            *b = e + 2;
            *e++ = JMP;
            b = e++;
            stmt();
        }
        *b = e;
    }
    else if (tk == While) {
        next();
        if (tk == '(') { next(); } else { error("'(' expected"); }
        long* s = e;
        expr(Assign);
        if (tk == ')') { next(); } else { error("')' expected"); }
        *e++ = BZ;
        long* b = e++;
        stmt();
        *e++ = JMP;
        *e++ = s;
        *b = e;
    }
    else if (tk == Return) {
        next();
        if (tk != ';') { expr(Assign); }
        *e++ = LEV;
        if (tk != ';') { error("';' expected"); }
        next();
    }
    else {
        expr(Assign);
        if (tk != ';') { error("';' expected"); }
        next();
    }
}

void compile(const char* src) {
    no = 1;
    p = src;
    next();
    while (tk) {
        if (tk != Id) { error("bad definition"); }
        long* d = id;
        next();
        
        if (tk == ';') {
            d[Addr] = (long)data;
            *data++ = 0;
            next();
        }
        else if (tk == Num) {
            d[Addr] = (long)data;
            *data++ = val;
            next();
            if (tk != ';') { error("';' expected"); }
            next();
        }
        else if (tk == Brak) {
            next();
            int size = 0;
            if (tk == Num) { size = val; next(); }
            if (tk != ']') { error("']' expected"); }
            next();

            int i;
            long* pp = data;
            d[Addr] = (long)data;
            for (i = 0; tk && tk != ';'; i++) {
                if (tk == Num) { *pp++ = val; }
                else if (tk == Id) {
                    if (!id[Addr]) { error("undefined identifier\n"); }
                    *pp++ = id[Addr];
                }
                else { error("bad initializer list"); }
                next();
                if (tk == ',') { next(); }
            }
            if (tk == 0) { error("unexpected EOF"); }
            data += i > size ? i : size;
            next();
        }
        else if (tk == '(') {
            next();
            loc = 2;
            d[Addr] = (long)e;
            while (tk && tk != ')') {
                if (tk != Id) { error("bad parameter list"); }
                if (id[IsAuto]) { error("duplicate parameter"); }
                id[HAddr]   = id[Addr];   id[Addr]   = loc++;
                id[HIsAuto] = id[IsAuto]; id[IsAuto] = 1;
                next();
                if (tk == ',') { next(); }
            }
            if (tk == 0) { error("unexpected EOF"); }
            next();

            loc = 0;
            *e++ = ENT;
            long* opr = e++;
            stmt();
            *opr = -loc;
            *e++ = LEV;

            for (id = sym; id[Name]; id += Idsz) {
                if (id[IsAuto]) {
                    id[Addr]   = id[HAddr];
                    id[IsAuto] = id[HIsAuto];
                }
            }
        }
        else { error("bad definition"); }
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

    e = le = malloc(10 * 1024 * sizeof(long));
    data = ldata = malloc(20 * 1024 * sizeof(long));
    id = sym = malloc(1024 * Idsz * sizeof(long));

    memset(e, 0, 10 * 1024 * sizeof(long));
    memset(data, 0, 20 * 1024 * sizeof(long));
    memset(sym, 0, 1024 * Idsz * sizeof(long));

    compile(buf);
}
