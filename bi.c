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
long        *e, *le, *end;
long        *data;
long        *id, *sym;
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
    Div, Mod, Inc, Dec, Brak, _Putchar,
};

enum { Name, Addr, IsAuto, HAddr, HIsAuto, Idsz };

enum {
    NOP = 0, IMM, LEA, LOAD, STO, PSH, ADD, SUB, MUL, DIV, MOD, AND, OR,
    EQ, NE, LT, GT, LE, GE, SHL, SHR, NOT, NEG, FINC, FDEC, BINC, BDEC,
    BZ, JMP, ENT, LEV, JSR, _PUTCHAR,
};

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
            else if (idcmp("_putchar", pp)) { tk = _Putchar; return; }
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
            const char* pp = (char*)data;
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
    if (tk == Num) { *e++ = IMM; *e++ = val; next(); }  
    else if (tk == '!') { next(); expr(Inc); *e++ = NOT; }
    else if (tk == Sub) { next(); expr(Inc); *e++ = NEG; }
    else if (tk == Mul) { next(); expr(Inc); *e++ = LOAD; }
    else if (tk == And) { next(); expr(Inc); if (*--e != LOAD) { error("bad address-of"); } }
    else if (tk == Inc) { next(); expr(Inc); if (*--e != LOAD) { error("bad lvalue in increment"); } *e++ = FINC; }
    else if (tk == Dec) { next(); expr(Inc); if (*--e != LOAD) { error("bad lvalue in decrement"); } *e++ = FDEC; }
    else if (tk == '(') { next(); expr(Assign); if (tk != ')') { error("')' expected"); } next(); }
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
        *e++ = LOAD;
        next();
    }
    else { error("bad expression"); }

    while (tk >= lev || tk == '(') {
        if (tk == Assign) {
            next();
            if (*--e != LOAD) { error("bad lvalue in decrement"); }
            *e++ = PSH; expr(Assign); *e++ = STO;
        }
        else if (tk == Cond) {
            next();
            *e++ = BZ; long* d = e++; expr(Assign);
            if (tk != ':') { error("bad conditional expression"); }
            next();
            *d = (long)(e + 2); *e++ = JMP;
            d = e++; expr(Cond); *d = (long)e;
        }
        else if (tk == Or) { next(); *e++ = PSH; expr(And); *e++ = OR; }
        else if (tk == And) { next(); *e++ = PSH; expr(Eq); *e++ = AND; }
        else if (tk == Eq) { next(); *e++ = PSH; expr(Lt); *e++ = EQ; }
        else if (tk == Ne) { next(); *e++ = PSH; expr(Lt); *e++ = NE; }
        else if (tk == Lt) { next(); *e++ = PSH; expr(Shl); *e++ = LT; }
        else if (tk == Gt) { next(); *e++ = PSH; expr(Shl); *e++ = GT; }
        else if (tk == Le) { next(); *e++ = PSH; expr(Shl); *e++ = LE; }
        else if (tk == Ge) { next(); *e++ = PSH; expr(Shl); *e++ = GE; }
        else if (tk == Shl) { next(); *e++ = PSH; expr(Add); *e++ = SHL; }
        else if (tk == Shr) { next(); *e++ = PSH; expr(Add); *e++ = SHR; }
        else if (tk == Add) { next(); *e++ = PSH; expr(Mul); *e++ = ADD; }
        else if (tk == Sub) { next(); *e++ = PSH; expr(Mul); *e++ = SUB; }
        else if (tk == Mul) { next(); *e++ = PSH; expr(Inc); *e++ = MUL; }
        else if (tk == Div) { next(); *e++ = PSH; expr(Inc); *e++ = DIV; }
        else if (tk == Mod) { next(); *e++ = PSH; expr(Inc); *e++ = MOD; }
        else if (tk == Inc) { next(); if (*--e != LOAD) { error("bad lvalue in increment"); } *e++ = BINC; }
        else if (tk == Dec) { next(); if (*--e != LOAD) { error("bad lvalue in decrement"); } *e++ = BDEC; }
        else if (tk == Brak) {
            next();
            if (*--e != LOAD) { error("bad lvalue in subscription"); }
            *e++ = PSH; expr(Assign); *e++ = ADD; *e++ = LOAD;
            if (tk != ']') { error("']' expected"); }
            next();
        }
        else if (tk == '(') {
            if (*--e != LOAD) { error("bad lvalue in function call"); }
            *e++ = PSH;
            next();
            long* be = e;
            long* bend = end;
            int t;
            for (t = 0; tk && tk != ')'; t++) {
                expr(Assign);
                *e++ = PSH;

                end -= e - be;
                if (end < e) { error("text segment too short"); }
                memcpy(end, be, (e - be) * sizeof(long));
                e = be;

                if (tk == ',') { next(); }
            }
            memcpy(e, end, (bend - end) * sizeof(long));
            e += bend - end;
            end = bend;
            next();
            //if (d->kind == Sys) { *e8++ = SYS; *e16++ = d->val; }
            //else if (d->kind == Fun) { *e8++ = JSR; *e32++ = d->val; }
            //else { error("bad function call"); }
            //if (t) { *e++ = ADJ; *e++ = t; }
            *e++ = JSR; *e++ = t;
        }
        else { error("bad expression"); }
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
            if (id[IsAuto]) { error("duplicate variable"); }
            id[HAddr]   = id[Addr];   id[Addr]   = --loc;
            id[HIsAuto] = id[IsAuto]; id[IsAuto] = 1;
            next();
            if (tk == Num) {
                *e++ = LEA;
                *e++ = id[Addr];
                *e++ = PSH;
                *e++ = IMM;
                *e++ = val;
                *e++ = STO;
            }
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
            *b = (long)(e + 2);
            *e++ = JMP;
            b = e++;
            stmt();
        }
        *b = (long)e;
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
        *e++ = (long)s;
        *b = (long)e;
    }
    else if (tk == Return) {
        next();
        if (tk != ';') { expr(Assign); }
        *e++ = LEV;
        if (tk != ';') { error("';' expected"); }
        next();
    }
    else if (tk == _Putchar) {
        next();
        expr(Assign);
        *e++ = _PUTCHAR;
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
        if (id[Addr]) { error("duplicate declaration"); }
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
            long* b = e++;
            stmt();
            *b = -loc;
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
    data = malloc(20 * 1024 * sizeof(long));
    id = sym = malloc(1024 * Idsz * sizeof(long));
    end = e + 10 * 1024;

    memset(e, 0, 10 * 1024 * sizeof(long));
    memset(data, 0, 20 * 1024 * sizeof(long));
    memset(sym, 0, 1024 * Idsz * sizeof(long));

    compile("putchar(n) _putchar n;");
    compile(buf);

    for (long* op = le; *op; op++) {
        /*IMM, LEA, LOAD, STO, PSH, ADD, SUB, MUL, DIV, MOD, AND, OR,
        EQ, NE, LT, GT, LE, GE, SHL, SHR, NOT, NEG, FINC, FDEC, BINC, BDEC,
        BZ, JMP, ENT, LEV, JSR, _PUTCHAR,*/
        printf("%p: ", op);
        switch (*op) {
        case IMM: printf("IMM %p\n", *++op); break;
        case LEA: printf("LEA %ld\n", *++op); break;
        case LOAD: printf("LOAD\n"); break;
        case STO: printf("STO\n"); break;
        case PSH: printf("PSH\n"); break;
        case ADD: printf("ADD\n"); break;
        case SUB: printf("SUB\n"); break;
        case MUL: printf("MUL\n"); break;
        case DIV: printf("DIV\n"); break;
        case MOD: printf("MOD\n"); break;
        case AND: printf("AND\n"); break;
        case OR: printf("OR\n"); break;
        case EQ: printf("EQ\n"); break;
        case NE: printf("NE\n"); break;
        case LT: printf("LT\n"); break;
        case GT: printf("GT\n"); break;
        case LE: printf("LE\n"); break;
        case GE: printf("GE\n"); break;
        case SHL: printf("SHL\n"); break;
        case SHR: printf("SHR\n"); break;
        case NOT: printf("NOT\n"); break;
        case NEG: printf("NEG\n"); break;
        case FINC: printf("FINC\n"); break;
        case FDEC: printf("FDEC\n"); break;
        case BINC: printf("BINC\n"); break;
        case BDEC: printf("BDEC\n"); break;
        case BZ: printf("BZ %p\n", *++op); break;
        case JMP: printf("JMP %p\n", *++op); break;
        case ENT: printf("ENT %ld\n", *++op); break;
        case LEV: printf("LEV\n"); break;
        case JSR: printf("JSR %ld\n", *++op); break;
        case _PUTCHAR: printf("_PUTCHAR\n"); break;
        default: printf("unknown opcode\n"); break;
        }
    }
}
