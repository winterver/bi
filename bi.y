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
        N_NOT,
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
        N_ASSIGN,
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

        N_CASE,
        N_LABEL,
        N_IF,
        N_WHILE,
        N_SWITCH,
        N_RETURN,
        N_GOTO,
        N_RVALUE,
        N_BLOCK,
        N_PUTCHAR,

        N_VAR,
        N_ARRAY,
        N_FUNC,
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
            struct node* args;
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
            struct node* right;
        } return_;
        struct {
            char* t_name;
        } goto_;
        struct node* rvalue;
        struct {
            struct node* stmts;
        } block;
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
            struct node* params;
            struct node* stmt;
        } func;
    };
} node_t;

node_t* mknode(int type) {
    node_t* n = malloc(sizeof(node_t));
    memset(n, 0, sizeof(node_t));
    n->type = type;
    return n;
}

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

%type<ival> NUM
%type<sval> NAME STR
%type<node> program definitions definition
%type<node> const ivals ival names
%type<node> statements statement nameconsts nameconst
%type<node> rvalue cond or and equ rel shift
%type<node> add mul post unary post args prime
%type<ival> unaryop assign

%%
program: { $$ = NULL; }
       | definitions;

definitions: definitions definition
            {
                $$ = $1;
                node_t* n;
                for (n = $$; n->next; n = n->next);
                n->next = $2;
            }
           | definition
           ;

definition: NAME ';'
            { $$ = mknode(N_VAR); $$->var.t_name = $1; }
          | NAME const ';'
            { $$ = mknode(N_VAR); $$->var.t_name = $1; $$->var.const_ = $2;}
          | NAME '[' ']' ivals ';'
            { $$ = mknode(N_ARRAY); $$->array.t_name = $1; $$->array.ivals = $4; }
          | NAME '[' const ']' ';'
            { $$ = mknode(N_ARRAY); $$->array.t_name = $1; $$->array.const_ = $3; }
          | NAME '[' const ']' ivals ';'
            { $$ = mknode(N_ARRAY); $$->array.t_name = $1; $$->array.const_ = $3; $$->array.ivals = $5; }
          | NAME '(' ')' statement
            { $$ = mknode(N_FUNC); $$->func.t_name = $1; $$->func.stmt =$4;}
          | NAME '(' names ')' statement
            { $$ = mknode(N_FUNC); $$->func.t_name = $1; $$->func.params=$3; $$->func.stmt =$5; }
          ;

const: NUM { $$ = mknode(T_NUM); $$->t_num = $1; }
     | STR { $$ = mknode(T_STR); $$->t_str = $1; }
     ;

ivals: ivals ',' ival
        {
            $$ = $1;
            node_t* n;
            for (n = $$; n->next; n = n->next);
            n->next = $3;
        }
     | ival
     ;

ival: const
    | NAME { $$ = mknode(T_NAME); $$->t_name = $1; }
    ;

names: names ',' NAME
        {
            $$ = $1;
            node_t* n;
            for (n = $$; n->next; n = n->next);
            n->next = mknode(T_NAME);
            n->next->t_name = $3;
        }
     | NAME { $$ = mknode(T_NAME); $$->t_name = $1; }
     ;

statement: AUTO nameconsts ';'
            { $$ = mknode(N_AUTO); $$->auto_.nameconsts = $2; }
         | EXTRN names ';'
            { $$ = mknode(N_EXTRN); $$->extrn.names = $2; }
         | CASE const ':'
            { $$ = mknode(N_CASE); $$->case_.const_ = $2; }
         | NAME ':'
            { $$ = mknode(N_LABEL); $$->label.t_name = $1; }
         | IF '(' rvalue ')' statement ELSE statement
            { $$ = mknode(N_IF); $$->if_.cond = $3; $$->if_.body = $5; $$->if_.else_ = $7; }
         | IF '(' rvalue ')' statement
            { $$ = mknode(N_IF); $$->if_.cond = $3; $$->if_.body = $5; }
         | WHILE '(' rvalue ')' statement
            { $$ = mknode(N_WHILE); $$->while_.cond = $3; $$->while_.body = $5; }
         | SWITCH '(' rvalue ')' statement
            { $$ = mknode(N_SWITCH); $$->switch_.cond = $3; $$->switch_.body = $5; }
         | RETURN rvalue ';'
            { $$ = mknode(N_RETURN); $$->return_.right = $2; }
         | GOTO NAME ';'
            { $$ = mknode(N_GOTO); $$->goto_.t_name = $2; }
         | rvalue ';'
            { $$ = mknode(N_RVALUE); $$->rvalue = $1; }
         | '{' statements '}'
            { $$ = mknode(N_BLOCK); $$->block.stmts = $2; }
         | '{' '}'
            { $$ = mknode(N_EMPTY); }
         | _PUTCHAR rvalue ';'
            { $$ = mknode(N_PUTCHAR); $$->_putchar.right = $2; }
         ;

nameconsts: nameconsts ',' nameconst
            {
                $$ = $1;
                node_t* n;
                for (n = $$; n->next; n = n->next);
                n->next = $3;
            }
          | nameconst 
          ;

nameconst: NAME const
            {
                $$ = mknode(N_NAMECONST);
                $$->nameconst.t_name = $1;
                $$->nameconst.const_ = $2;
            }
         | NAME
            {
                $$ = mknode(N_NAMECONST);
                $$->nameconst.t_name = $1;
            }
         ;

statements: statements statement
            {
                $$ = $1;
                node_t* n;
                for (n = $$; n->next; n = n->next);
                n->next = $2;
            }
          | statement
          ;

rvalue: unary assign rvalue
        { $$ = mknode($2); $$->binary.left = $1; $$->binary.right = $3; }
      | cond
      ;

assign: '=' { $$ = N_ASSIGN; }
      | ADDA { $$ = N_ADDA; }
      | SUBA { $$ = N_SUBA; }
      | MULA { $$ = N_MULA; }
      | DIVA { $$ = N_DIVA; }
      | MODA { $$ = N_MODA; }
      | SHLA { $$ = N_SHLA; }
      | SHRA { $$ = N_SHRA; }
      | ANDA { $$ = N_ANDA; }
      | ORA { $$ = N_ORA; }
      ;

cond: or '?' rvalue ':' cond
        {
            $$ = mknode(N_COND);
            $$->cond.cond = $1;
            $$->binary.left = $3;
            $$->binary.right = $5;
        }
    | or
    ;

or: or '|' and
       { $$ = mknode(N_OR); $$->binary.left = $1; $$->binary.right = $3; }
  | and
  ;

and: and '&' equ
       { $$ = mknode(N_AND); $$->binary.left = $1; $$->binary.right = $3; }
   | equ
   ;

equ: equ EQ rel
       { $$ = mknode(N_EQ); $$->binary.left = $1; $$->binary.right = $3; }
   | equ NE rel
       { $$ = mknode(N_NE); $$->binary.left = $1; $$->binary.right = $3; }
   | rel
   ;

rel: rel '<' shift
       { $$ = mknode(N_LT); $$->binary.left = $1; $$->binary.right = $3; }
   | rel '>' shift
       { $$ = mknode(N_GT); $$->binary.left = $1; $$->binary.right = $3; }
   | rel LE shift
       { $$ = mknode(N_LE); $$->binary.left = $1; $$->binary.right = $3; }
   | rel GE shift
       { $$ = mknode(N_GE); $$->binary.left = $1; $$->binary.right = $3; }
   | shift
   ;

shift: shift SHL add
       { $$ = mknode(N_SHL); $$->binary.left = $1; $$->binary.right = $3; }
     | shift SHR add
       { $$ = mknode(N_SHR); $$->binary.left = $1; $$->binary.right = $3; }
     | add
     ;

add: add '+' mul
       { $$ = mknode(N_ADD); $$->binary.left = $1; $$->binary.right = $3; }
   | add '-' mul
       { $$ = mknode(N_SUB); $$->binary.left = $1; $$->binary.right = $3; }
   | mul
   ;

mul: mul '*' unary
       { $$ = mknode(N_MUL); $$->binary.left = $1; $$->binary.right = $3; }
   | mul '/' unary
       { $$ = mknode(N_DIV); $$->binary.left = $1; $$->binary.right = $3; }
   | mul '%' unary
       { $$ = mknode(N_MOD); $$->binary.left = $1; $$->binary.right = $3; }
   | unary
   ;

unary: unaryop unary { $$ = mknode($1); $$->unary.right = $2; }
     | INC unary { $$ = mknode(N_PRE_INC); $$->unary.right = $2; }
     | DEC unary { $$ = mknode(N_PRE_DEC); $$->unary.right = $2; }
     | post
     ;

unaryop: '&' { $$ = N_ADDROF; }
       | '*' { $$ = N_DEREF; }
       | '-' { $$ = N_NEGATE; }
       | '!' { $$ = N_NOT; }
       ;

post: post '[' rvalue ']'
        { $$ = mknode(N_INDEX); $$->index.left = $1; $$->index.right = $3; }
    | post '(' args ')'
        { $$ = mknode(N_CALL); $$->call.left = $1; $$->call.args = $3; }
    | post '(' ')'
        { $$ = mknode(N_CALL); $$->call.left = $1; }
    | post INC
        { $$ = mknode(N_POST_INC); $$->post.left = $1; }
    | post DEC
        { $$ = mknode(N_POST_DEC); $$->post.left = $1; }
    | prime
    ;

args: rvalue ',' args
        {
            $$ = $3;
            node_t* n;
            for (n = $$; n->next; n = n->next);
            n->next = $1;
        }
    | rvalue
    ;

prime: '(' rvalue ')' { $$ = $2; }
     | const
     | NAME { $$ = mknode(T_NAME); $$->t_name = $1; }
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
