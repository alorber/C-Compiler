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
    int op;
    astnode *node;
}

/* Tokens */
%token<ident> IDENT 
%token<c> CHARLIT 
%token<string> STRING 
%token<number> NUMBER 
%token<op> INDSEL PLUSPLUS MINUSMINUS SHL SHR LTEQ GTEQ EQEQ NOTEQ
%token<op> LOGAND LOGOR TIMESEQ DIVEQ MODEQ PLUSEQ MINUSEQ SHLEQ SHREQ
%token<op> ANDEQ OREQ XOREQ SIZEOF
%token ELLIPSIS AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT DO DOUBLE 
%token ELSE ENUM EXTERN FLOAT FOR GOTO IF INLINE INT LONG REGISTER 
%token RESTRICT RETURN SHORT SIGNED STATIC STRUCT SWITCH TYPEDEF UNION 
%token UNSIGNED VOID VOLATILE WHILE _BOOL _COMPLEX _IMAGINARY
2
/* Types (in order of grammar) */
%type<node> primary_expr postfix_expr function_call expr_list unary_expr
%type<op> unary_op
%type<node> cast_expr multiplicative_expr additive_expr shift_expr
%type<node> relational_expr equality_expr bitwise_and_expr bitwise_xor_expr
%type<node> bitwise_or_expr logical_and_expr logical_or_expr conditional_expr
%type<node> assignment_expr expr
%type<op> assignment_op
%type<op> '=' '<' '>' '!' '~' '(' ')' '[' ']'

/* Operator Precedence & Associativity */
/* From cpp website */
%left <op> ','
%right '=' PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ SHLEQ SHREQ ANDEQ XOREQ OREQ
%right <op> '?' ":"
%left LOGOR
%left LOGAND
%left <op> '|'
%left <op> '^'
%left <op> '&'
%left EQEQ NOTEQ
%left '<' '>' LTEQ GTEQ
%left SHL SHR
%left <op> '+' '-'
%left <op> '*' '/' '%'
%right PLUSPLUS MINUSMINUS /* prefix */ SIZEOF '!' '~' /* +a, -a, &a, a* */
%left INDSEL '(' ')' '[' ']' /* .a, ->a */
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
            | function_call                 {$$ = $1;}
            | postfix_expr '.' IDENT        {}
            | postfix_expr INDSEL IDENT     {}
            | postfix_expr PLUSPLUS         {$$ = create_unary_node($2,$1);}
            | postfix_expr MINUSMINUS       {$$ = create_unary_node($2,$1);}
            ;

function_call: postfix_expr '(' expr_list ')'   {$$ = create_fnc_call_node($1,$3);}
             | postfix_expr '(' ')'             {$$ = create_fnc_call_node($1,NULL);}
             ;

expr_list: assignment_expr                  {$$ = init_expr_list($1);}
         | expr_list ',' assignment_expr    {$$ = add_argument_to_list($1, $3);}
         ;

unary_expr: postfix_expr            {$$ = $1;}
          | PLUSPLUS unary_expr     {astnode *num_one = create_num_one_node();
                                     $$ = simplify_compound_op('+', $2, num_one);}
          | MINUSMINUS unary_expr   {astnode *num_one = create_num_one_node();
                                     $$ = simplify_compound_op('-', $2, num_one);}
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

conditional_expr: logical_or_expr                                 {$$ = $1;}
                | logical_or_expr '?' expr ':' conditional_expr   {$$ = create_ternary_node($1,$3,$5);}
                ;

assignment_expr: conditional_expr                           {$$ = $1;}
               | unary_expr '=' assignment_expr             {$$ = create_binary_node($2,$1,$3);}
               | unary_expr assignment_op assignment_expr   {$$ = simplify_compound_op($2,$1,$3);}
               ;

assignment_op: TIMESEQ      {$$ = '*';}
             | DIVEQ        {$$ = '/';}
             | MODEQ        {$$ = '%';}
             | PLUSEQ       {$$ = '+';}
             | MINUSEQ      {$$ = '-';}
             | SHLEQ        {$$ = SHL;}
             | SHREQ        {$$ = SHR;}
             | ANDEQ        {$$ = '&';}
             | XOREQ        {$$ = '^';}
             | OREQ         {$$ = '|';}
             ;

expr: assignment_expr               {$$ = $1;}
    | expr ',' assignment_expr      {$$ = create_binary_node($2,$1,$3);}
    ;

%%
/* ----------- */
/*  USER CODE  */
/* ----------- */