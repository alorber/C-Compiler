/* Andrew Lorber */
/* Compilers - Parser */

/* ------------- */
/*  Definitions  */
/* ------------- */

%{
#include "parser.h"
#include "astFunctions.h"
#include "../Lexer/lexerFunctions.h"
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

start_expr: expr                {print_ast($1,0);}
          | start_expr expr     {print_ast($1,0);}

primary_expr: IDENT           {$$ = create_ident_node($1);}
            | CHARLIT         {$$ = create_char_node($1);}
            | NUMBER          {$$ = create_number_node($1);}
            | STRING          {$$ = create_string_node($1);}
            | '(' expr ')'    {$$ = $2;}
            ;

postfix_expr: primary_expr                  {$$ = $1;}
            | postfix_expr '[' expr ']'     {/* a[b] ===> *(a+b) */
                                             astnode *pointer = create_binary_node('+',$1,$3);
                                             $$ = create_unary_node('*',pointer);}
            | function_call                 {$$ = $1;}
            | postfix_expr '.' IDENT        {astnode *ident_node = create_ident_node($3);
                                             $$ = create_binary_node($2,$1,$3);}
            | postfix_expr INDSEL IDENT     {/* a->b ===> (*a).b */
                                             astnode *pointer = create_unary_node('*',$1);
                                             astnode *ident_node = create_ident_node($3);
                                             $$ = create_binary_node('.',pointer,ident_node);}
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

int main() {
    yyparse();
    return 0;
}

// Prints number of indents given
void print_indents(num_indents) {
    // Indents
    for(int i = 0; i < num_indents; i++) {
        fprintf(stdout,"\t");
    }
}

// Prints AST
void print_ast(astnode *node, int num_indents) {
    // Prints indents
    print_indents(num_indents);

    // Checks type for printing
    switch(node->node_type) {
        case UNARY_TYPE:
            // Checks for special operators
            int op = node->ast_unary_op.op;
            switch(op) {
                // Address Of (&)
                case '&':
                    fprintf(stdout, "ADDRESSOF\n");
                    break;

                // Dereference (*)
                case '*':
                    fprintf(stdout, "DEREF\n");
                    break;

                // Sizeof
                case SIZEOF:
                    fprintf(stdout, "SIZEOF\n");
                    break;

                // a++ & a--
                case PLUSPLUS:
                    fprintf(stdout, "UNARY OP POSTINC\n");
                    break;
                case MINUSMINUS:
                    fprintf(stdout, "UNARY OP POSTDEC\n");
                    break;

                // Other
                default:
                    if(op < 256) {
                        fprintf(stdout, "UNARY OP %c\n", op);
                    } else {
                        // TODO: PRINT STRING OF SYMBOL
                        fprintf(stdout, "UNARY OP %s\n", print_keyword(op));
                    }
            }

            // Prints sub-node
            print_ast(node->ast_unary_op.expr,num_indents+1);

            break;

        case BINARY_TYPE:
            // Checks for various operators
            int op = node->ast_binary_op.op;
            switch(op) {
                // Assignment
                case '=':
                    fprintf(stdout, "ASSIGNMENT\n");
                    break;

                // Comparison
                case '<':
                case '>':
                    fprintf(stdout, "COMPARISON OP %c\n", op);
                    break;
                case LTEQ:
                    fprintf(stdout, "COMPARISON OP <=\n");
                    break;
                case GTEQ:
                    fprintf(stdout, "COMPARISON OP >=\n");
                    break;
                case EQEQ:
                    fprintf(stdout, "COMPARISON OP ==\n");
                    break;
                case NOTEQ:
                    fprintf(stdout, "COMPARISON OP !=\n");
                    break;

                // Logical
                case LOGAND:
                    fprintf(stdout, "LOGICAL OP &&\n");
                    break;
                case LOGOR:
                    fprintf(stdout, "LOGICAL OP ||\n");
                    break;

                // Selection
                case ".":
                    fprintf(stdout, "SELECT\n");
                    break;

                // Other
                default:   
                    if(op < 256) {
                        fprintf(stdout, "BINARY OP %c\n", op);
                    } else {
                        // TODO: PRINT STRING OF SYMBOL
                        fprintf(stdout, "BINARY OP %s\n", print_keyword(op));
                    }
            }

            // Prints sub-nodes
            print_ast(node->ast_binary_op.left_expr, num_indents+1);
            print_ast(node->ast_binary_op.right_expr, num_indents+1);

            break;

        case TERNARY TYPE:
            // IF
            fprintf(stdout, "TERNARY OP, IF:\n");
            print_ast(node->ast_ternary_op.if_expr, num_indents+1);

            // THEN
            print_indents(num_indents);
            fprintf(stdout, "THEN:\n");
            print_ast(node->ast_ternary_op.then_expr, num_indents+1);

            // ELSE
            print_indents(num_indents);
            fprintf(stdout, "ELSE:\n");
            print_ast(node->ast_ternary_op.else_expr, num_indents+1);

            break;

        case NUMBER_TYPE:
            fprintf(stdout, "CONSTANT: (type=");

            // Checks number type
            int number = node->ast_number.number;
            switch(number.size_specifier) {
                case INT_TYPE:
                    fprintf(stdout, "int)%lli\n", number.i_value);
                    break;
                case LONG_TYPE:
                        fprintf(stdout, "long)%lli\n", number.i_value);
                        break;
                case LONGLONG_TYPE:
                        fprintf(stdout, "longlong)%lli\n", number.i_value);
                        break;
                case FLOAT_TYPE:
                        fprintf(stdout, "float)%Lf\n", number.d_value);
                        break;
                case DOUBLE_TYPE:
                        fprintf(stdout, "double)%Lf\n", number.d_value);
                        break;
                case LONGDOUBLE_TYPE:
                        fprintf(stdout, "longdouble)%Lf\n", number.d_value);
                        break;
            }

            break;

        case IDENT_TYPE:
            fprintf(stdout, "IDENT %s\n", node->ast_ident.ident);
            break;

        case STRING_TYPE:
            fprintf(stdout, "STRING ");
            print_string(node->ast_string->string);
            fprintf(stdout, "\n");

            break;

        case CHARLIT_TYPE:
            fprintf(stdout, "CHARLIT ");
            print_string(node->ast_charlit->charlit);
            fprintf(stdout, "\n");

            break;

        case FUNCTION_TYPE:
            fprintf(stdout, "FNCALL, %i arguments\n", node->ast_fnc_call.num_arguments);

            // Prints function name
            print_ast(node->ast_fnc_call.function_name, num_indents+1);

            // Checks for empty expr_list
            if(node->ast_fnc_call.expr_list_head == NULL) {
                break;
            }

            // Prints arguments
            astnode_argument *curr_argument = &(node->ast_fnc_call.expr_list_head->ast_expr_list_head)
            for(arg_number = 1; curr_argument != NULL; arg_number++, curr_argument = curr_argument->next) {
                print_indents(num_indents);
                fprintf(stdout, "arg #%i=\n", arg_number);
                print_ast(curr_argument->expr, num_indents+1);
            }
            
            break;

        default:
            fprintf(stdout, "ERROR: UNKNOWN NODE\n");
    }

    // Separates expressions
    if(num_indents == 0) {
        fprintf(stdout, "\n\n");
    }
}