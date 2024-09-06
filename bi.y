%locations

%{
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
typedef struct node {
    enum {
        N_EMPTY = 1,
        T_NAME,
        T_NUM,
        T_STR,

        N_INDEX,
        N_CALL,
        N_POST_INC,
        N_POST_DEC,

        N_ADDROF,
        N_DEREF,
        N_NEGATE,
        N_PRE_NOT,
        N_PRE_INC,
        N_PRE_DEC,

        N_ADD,
        N_SUB,
        N_MUL,
        N_DIV,
        N_MOD,
        N_AND,
        N_OR,
        N_SHL,
        N_SHR,
        N_LT,
        N_GT,
        N_LE,
        N_GE,
        N_EQ,
        N_NE,
        N_ADDA,
        N_SUBA,
        N_MULA,
        N_DIVA,
        N_MODA,
        N_ANDA,
        N_ORA,
        N_SHLA,
        N_SHRA,

        N_COND,

        N_AUTO,
        N_EXTRN,
        N_NAMECONST,
        N_NAME,

        N_CASE,
        N_LABEL,
        N_IF,
        N_WHILE,
        N_SWITCH,
        N_RETURN,
        N_GOTO,
        N_RVALUE,
        N_STMT,
        N_PUTCHAR,

        N_VAR,
        N_ARRAY,
        N_FUNC,
        N_IVAL_CONST,
        N_IVAL_NAME,
        N_DEFINITION,
    } type;
    struct node* next;
    union {
        char* t_name;
        int64_t t_num;
        char* t_str;
        struct {
            struct node* left;
            struct node* right;
        } index;
        struct {
            struct node* left;
            struct node* right;
        } call;
        struct {
            struct node* left;
        } post;
        struct {
            struct node* right;
        } unary;
        struct {
            struct node* left;
            struct node* right;
        } binary;
        struct {
            struct node* cond;
            struct node* left;
            struct node* right;
        } cond;
        struct {
            struct node* nameconsts;
        } auto_;
        struct {
            struct node* names;
        } extrn;
        struct {
            char* t_name;
            struct node* const_;
        } nameconst;
        struct {
            struct node* const_;
        } case_;
        struct {
            char* t_name;
        } label;
        struct {
            struct node* cond;
            struct node* body;
            struct node* else_;
        } if_;
        struct {
            struct node* cond;
            struct node* body;
        } while_;
        struct {
            struct node* cond;
            struct node* body;
        } switch_;
        struct {
            struct node* expr;
        } return_;
        struct {
            struct node* right;
        } goto_;
        struct node* rvalue;
        struct node* stmt;
        struct {
            struct node* right;
        } _putchar;
        struct {
            char* t_name;
            struct node* const_;
        } var;
        struct {
            char* t_name;
            struct node* const_;
            struct node* ivals;
        } array;
        struct {
            char* t_name;
            struct node* const_;
        } ival;
        struct {
            char* t_name;
            struct node* params;
            struct node* stmt;
        } func;
        struct node* definition;
    };
} node_t;
%}

%token NAME NUM STR
%token AUTO EXTRN IF ELSE WHILE
%token SWITCH CASE GOTO RETURN
%token EQ NE LE GE SHL SHR INC DEC
%token ADDA SUBA MULA DIVA MODA SHLA SHRA ANDA ORA
%token _PUTCHAR

%union{
    char* sval;
    int64_t ival;
    node_t* node;
};

%%
program: | definitions;

definitions: definitions definition
           | definition
           ;

definition: NAME ';'
          | NAME const ';'
          | NAME '[' ']' ivals ';'
          | NAME '[' const ']' ';'
          | NAME '[' const ']' ivals ';'
          | NAME '(' ')' statement
          | NAME '(' names ')' statement
          ;

const: NUM
     | STR
     ;

ivals: ivals ',' ival
     | ival
     ;

ival: const
    | NAME
    ;

names: names ',' NAME
     | NAME
     ;

statement: AUTO nameconsts ';'
         | EXTRN names ';'
         | CASE const ':'
         | NAME ':'
         | IF '(' rvalue ')' statement ELSE statement
         | IF '(' rvalue ')' statement
         | WHILE '(' rvalue ')' statement
         | SWITCH '(' rvalue ')' statement
         | RETURN rvalue ';'
         | GOTO NAME ';'
         | rvalue ';'
         | '{' statements '}'
         | '{' '}'
         | _PUTCHAR rvalue ';'
         ;

nameconsts: nameconsts ',' nameconst
          | nameconst 
          ;

nameconst: NAME const
         | NAME
         ;

statements: statements statement
          | statement
          ;

rvalue: unary assign rvalue
      | cond
      ;

assign: '='
      | ADDA
      | SUBA
      | MULA
      | DIVA
      | MODA
      | SHLA
      | SHRA
      | ANDA
      | ORA
      ;

cond: or '?' rvalue ':' cond
    | or
    ;

or: or '|' and
  | and
  ;

and: and '&' equ
   | equ
   ;

equ: equ EQ rel
   | equ NE rel
   | rel
   ;

rel: rel '<' shift
   | rel '>' shift
   | rel LE shift
   | rel GE shift
   | shift
   ;

shift: shift SHL add
     | shift SHR add
     | add
     ;

add: add '+' mul
   | add '-' mul
   | mul
   ;

mul: mul '*' unary
   | mul '/' unary
   | mul '%' unary
   | unary
   ;

unary: unaryop unary
     | INC unary
     | DEC unary
     | post
     ;

unaryop: '&'
       | '*'
       | '-'
       | '!'
       ;

post: post '[' rvalue ']'
    | post '(' args ')'
    | post '(' ')'
    | post INC
    | post DEC
    | prime
    ;

args: rvalue ',' args
    | rvalue
    ;

prime: '(' rvalue ')'
     | const
     | NAME
     ;

%%

void YYERROR_DECL() {
    printf("(%d) %s\n", loc->first_line, s);
    exit(-1);
}

const char* p;

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

int YYLEX_DECL() {
    while (*p) {
        if (strchr(" \t\r", *p)) { p++; }
        else if (strchr("\n", *p)) { p++; yylloc.first_line++; }
        else if (prefix("/*")) {
            while (*p && !prefix("*/")) {
                yylloc.first_line += *p++ == '\n';
            }
        }
        else if (prefix("=<<")) { return SHLA; }
        else if (prefix("=>>")) { return SHRA; }
        else if (prefix("=+")) { return ADDA; }
        else if (prefix("=-")) { return SUBA; }
        else if (prefix("=*")) { return MULA; }
        else if (prefix("=/")) { return DIVA; }
        else if (prefix("=%")) { return MODA; }
        else if (prefix("=&")) { return ANDA; }
        else if (prefix("=|")) { return ORA; }
        else if (prefix("<<")) { return SHL; }
        else if (prefix(">>")) { return SHR; }
        else if (prefix("<=")) { return LE; }
        else if (prefix(">=")) { return GE; }
        else if (prefix("!=")) { return NE; }
        else if (prefix("==")) { return EQ; }
        else if (prefix("++")) { return INC; }
        else if (prefix("--")) { return DEC; }
        else if (isalpha(*p)) {
            const char* pp = p;
            while (isalnum(*p)) { p++; }
            if (idcmp("extrn", pp)) { return EXTRN; }
            else if (idcmp("auto", pp)) { return AUTO; }
            else if (idcmp("if", pp)) { return IF; }
            else if (idcmp("else", pp)) { return ELSE; }
            else if (idcmp("while", pp)) { return WHILE; }
            else if (idcmp("switch", pp)) { return SWITCH; }
            else if (idcmp("case", pp)) { return CASE; }
            else if (idcmp("goto", pp)) { return GOTO; }
            else if (idcmp("return", pp)) { return RETURN; }
            else if (idcmp("_putchar", pp)) { return _PUTCHAR; }
            yylval.sval = malloc(p-pp+1);
            memcpy(yylval.sval, pp, p-pp);
            yylval.sval[p-pp] = 0;
            return NAME;
        }
        else if (isdigit(*p)) {
            int b = *p == '0' ? 8 : 10;
            for (yylval.ival = 0; isdigit(*p); p++) {
                yylval.ival = yylval.ival * b + *p - '0';
            }
            return NUM;
        }
        else if (*p == '\'' || *p == '"') {
            static char str[4096];
            char* pp = str;
            int tk = *p++;
            yylval.ival = 0;
            while (*p != 0 && *p != tk) {
                if ((yylval.ival = *p++) == '*') {
                    if (*p == '0') { yylval.ival = '\0'; }
                    else if (*p == 'e') { yylval.ival = '\0'; }
                    else if (*p == '(') { yylval.ival = '{'; }
                    else if (*p == ')') { yylval.ival = '}'; }
                    else if (*p == 't') { yylval.ival = '\t'; }
                    else if (*p == '*') { yylval.ival = '*'; }
                    else if (*p == '\'') { yylval.ival = '\''; }
                    else if (*p == '\"') { yylval.ival = '\"'; }
                    else if (*p == 'n') { yylval.ival = '\n'; }
                    else { YYERROR_CALL("unknown escape sequence"); }
                    if (*++p != '\'' && tk == '\'') {
                        YYERROR_CALL("bad char literal");
                    }
                }
                if (tk == '"') { *pp++ = yylval.ival; }
                if (*p != '\'' && tk == '\'') {
                    YYERROR_CALL("bad char literal");
                }
            }
            if (*p++ == 0) { YYERROR_CALL("unexpected EOF"); }
            if (tk == '"') {*pp++=0;yylval.sval=strdup(str);return STR;}
            else { return NUM; }
        }
        else { return *p++; }
    }
    return YYEOF;
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

    yylloc.first_line = 1;
    p = buf;
    yyparse();
}
