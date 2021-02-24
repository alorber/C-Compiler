/* Andrew Lorber */
/* Compilers - Parser */

/* ------------- */
/*  Definitions  */
/* ------------- */

%{
#include "parser.h"
#include "astFunctions.h"
%}

%union {
    num_type number;
    char c;
    char *string;
    char *ident;
}

/* Tokens */
%token IDENT CHARLIT STRING NUMBER INDSEL PLUSPLUS MINUSMINUS SHL SHR
%token LTEQ GTEQ EQEQ NOTEQ LOGAND LOGOR ELLIPSIS TIMESEQ DIVEQ MODEQ
%token PLUSEQ MINUSEQ SHLEQ SHREQ ANDEQ OREQ XOREQ AUTO BREAK CASE
%token CHAR CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN FLOAT
%token FOR GOTO IF INLINE INT LONG REGISTER RESTRICT RETURN SHORT
%token SIGNED SIZEOF STATIC STRUCT SWITCH TYPEDEF UNION UNSIGNED
%token VOID VOLATILE WHILE _BOOL _COMPLEX _IMAGINARY

/* Types */


/* Operator Precedence & Associativity */
/* From cpp website */
%left ','
%right '=' PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ SHLEQ SHREQ ANDEQ XOREQ OREQ
%right '?' ":"
%left LOGOR
%left LOGAND
%left '|'
%left '^'
%left '&'
%left EQEQ NOTEQ
%left '<' '>' LTEQ GTEQ
%left SHL SHR
%left '+' '-'
%left '*' '/' '%'
%right PLUSPLUS MINUSMINUS /* prefix */ '!' '~' /* +a, -a, &a, a* */
%left '(' ')' '[' ']' /* .a, ->a */
%left IF
%left ELSE

%%
/* ------- */
/*  RULES  */
/* ------- */

primary_expr: IDENT           {$$ = create_ident_node($1);}
            | CHARLIT         {$$ = create_char_node($1);}
            | NUMBER          {$$ = create_number_node($1);}
            | STRING          {$$ = create_string_node($1);}
            | '(' expr ')'    {$$ = $2;}
            ;

postfix_expr: primary_expr                  {$$ = $1;}
            | postfix_expr '[' expr ']'     {}
            | function_call                 {}
            | postfix_expr '.' IDENT        {}
            | postfix_expr INDSEL IDENT     {}
            | postfix_expr PLUSPLUS         {}
            | postfix_expr MINUSMINUS       {}
            ;

function_call: postfix_expr '(' expr_list ')'   {}
             | postfix_expr '(' ')'             {}
             ;

expr_list: assignment_expr                  {$$ = $1;}
         | expr_list ',' assignment_expr    {}
         ;

unary_expr: postfix_expr            {$$ = $1;}
          | PLUSPLUS unary_expr     {}
          | MINUSMINUS unary_expr   {}
          | unary_op cast_expr      {$$ = create_unary_node($1,$2);}
          | SIZEOF unary_expr       {$$ = create_unary_node($1,$2);}
          ;

unary_op: '&'   {$$ = $1;}
        | '*'   {$$ = $1;}
        | '+'   {$$ = $1;}
        | '-'   {$$ = $1;}
        | '~'   {$$ = $1;}
        | '!'   {$$ = $1;}
        ;

cast_expr: unary_expr   {$$ = $1;}
         ;

multiplicative_expr: cast_expr                           {$$ = $1;}
                   | multiplicative_expr '*' cast_expr   {$$ = create_binary_node($2,$1,$3);}
                   | multiplicative_expr '/' cast_expr   {$$ = create_binary_node($2,$1,$3);}
                   | multiplicative_expr '%' cast_expr   {$$ = create_binary_node($2,$1,$3);}
                   ;

additive_expr: multiplicative_expr                      {$$ = $1;}
             | additive_expr '+' multiplicative_expr    {$$ = create_binary_node($2,$1,$3);}
             | additive_expr '-' multiplicative_expr    {$$ = create_binary_node($2,$1,$3);}
             ;

shift_expr: additive_expr                   {$$ = $1;}
          | shift_expr SHL additive_expr    {$$ = create_binary_node($2,$1,$3);}
          | shift_expr SHR additive_expr    {$$ = create_binary_node($2,$1,$3);}
          ;

relational_expr: shift_expr                         {$$ = $1;}
               | relational_expr '<' shift_expr     {$$ = create_binary_node($2,$1,$3);}
               | relational_expr '>' shift_expr     {$$ = create_binary_node($2,$1,$3);}
               | relational_expr LTEQ shift_expr    {$$ = create_binary_node($2,$1,$3);}
               | relational_expr GTEQ shift_expr    {$$ = create_binary_node($2,$1,$3);}
               ;

equality_expr: relational_expr                          {$$ = $1;}
             | equality_expr EQEQ relational_expr       {$$ = create_binary_node($2,$1,$3);}
             | equality_expr NOTEQ relational_expr      {$$ = create_binary_node($2,$1,$3);}
             ;

bitwise_and_expr: equality_expr                         {$$ = $1;}
                | bitwise_and_expr '&' equality_expr    {$$ = create_binary_node($2,$1,$3);}
                ;

bitwise_xor_expr: bitwise_and_expr                          {$$ = $1;}
                | bitwise_xor_expr '^' bitwise_and_expr     {$$ = create_binary_node($2,$1,$3);}
                ;

bitwise_or_expr: bitwise_xor_expr                           {$$ = $1;}
               | bitwise_or_expr '|' bitwise_xor_expr       {$$ = create_binary_node($2,$1,$3);}
               ;

logical_and_expr: bitwise_or_expr                             {$$ = $1;}
                | logical_and_expr LOGAND bitwise_or_expr     {$$ = create_binary_node($2,$1,$3);}
                ;

logical_or_expr: logical_and_expr                            {$$ = $1;}
               | logical_or_expr LOGOR logical_and_expr      {$$ = create_binary_node($2,$1,$3);}
               ;

conditional_expr: logical_or_expr                               {$$ = $1;}
                | logical_or_expr '?' expr : conditional_expr   {$$ = create_ternary_node($1,$3,$5);}
                ;

assignment_expr: conditional_expr                           {$$ = $1;}
               | unary_expr assignment_op assignment_expr   {$$ = create_binary_node($2,$1,$3);}
               ;

assignment_op: '='          {$$ = $1;}
             | TIMESEQ      {$$ = $1;}
             | DIVEQ        {$$ = $1;}
             | MODEQ        {$$ = $1;}
             | PLUSEQ       {$$ = $1;}
             | MINUSEQ      {$$ = $1;}
             | SHLEQ        {$$ = $1;}
             | SHREQ        {$$ = $1;}
             | ANDEQ        {$$ = $1;}
             | XOREQ        {$$ = $1;}
             | OREQ         {$$ = $1;}
             ;

expr: assignment_expr               {$$ = $1;}
    | expr ',' assignment_expr      {$$ = create_binary_node($2,$1,$3);}
    ;

%%
/* ----------- */
/*  USER CODE  */
/* ----------- */