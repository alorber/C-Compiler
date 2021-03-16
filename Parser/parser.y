/* Andrew Lorber */
/* Compilers - Parser */

/* ------------- */
/*  Definitions  */
/* ------------- */

%{
#include "numType.h"
#include "parser.h"
#include "astFunctions.h"
#include "../Lexer/lexerFunctions.h"

/* Remove comments to enable debugging */
/* #define YYDEBUG	1 */
%}
/* %define parse.error verbose */

%union {
    struct num_type number;
    char *string;
    char *ident;
    int op;
    struct astnode *node;
}

/* Tokens */
%token<ident> IDENT 
%token<string> CHARLIT 
%token<string> STRING 
%token<number> NUMBER 
%token<op> INDSEL PLUSPLUS MINUSMINUS SHL SHR LTEQ GTEQ EQEQ NOTEQ
%token<op> LOGAND LOGOR TIMESEQ DIVEQ MODEQ PLUSEQ MINUSEQ SHLEQ SHREQ
%token<op> ANDEQ OREQ XOREQ SIZEOF
%token ELLIPSIS AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT DO DOUBLE 
%token ELSE ENUM EXTERN FLOAT FOR GOTO IF INLINE INT LONG REGISTER 
%token RESTRICT RETURN SHORT SIGNED STATIC STRUCT SWITCH TYPEDEF UNION 
%token UNSIGNED VOID VOLATILE WHILE _BOOL _COMPLEX _IMAGINARY

/* Types (in order of grammar) */
%type<node> start_expr primary_expr postfix_expr function_call expr_list unary_expr
%type<op> unary_op
%type<node> cast_expr multiplicative_expr additive_expr shift_expr
%type<node> relational_expr equality_expr bitwise_and_expr bitwise_xor_expr
%type<node> bitwise_or_expr logical_and_expr logical_or_expr conditional_expr
%type<node> assignment_expr expr
%type<op> assignment_op
%type<op> '=' '<' '>' '!' '~' '(' ')' '[' ']' '.'
%type<node> decl_or_fnc_def declaration decl_specifier init_decl_list init_decl
%type<node> storage_class_specifier type_specifier struct_union_specifier
%type<node> struct_union struct_decl_list struct_decl spec_qual_list 
%type<node> struct_declarator_list struct_declarator enum_specifier 
%type<op> type_qualifier
%type<node> fnc_specifier declarator dir_declarator pointer type_qualifier_list
%type<node> type_name abstr_declarator dir_abstr_declarator typedef_name
%type<node> initializer fnc_def compound_stmt decl_or_stmt_list decl_or_stmt
%type<node> statement

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
%right SIZEOF '!' '~' /* +a, -a, &a, a* */
%left PLUSPLUS MINUSMINUS /* postfix */ INDSEL '(' ')' '[' ']' /* .a, ->a */
%left IF
%left ELSE

%start decl_or_fnc_def

%%
/* ------- */
/*  RULES  */
/* ------- */

/* EXPRESSIONS
* -------------- */

start_expr: expr ';'                {print_ast($1,0);}
          | start_expr expr ';'     {print_ast($2,0);}
          ;

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
                                             $$ = create_binary_node('.',$1,ident_node);}
            | postfix_expr INDSEL IDENT     {/* a->b ===> (*a).b */
                                             astnode *pointer = create_unary_node('*',$1);
                                             astnode *ident_node = create_ident_node($3);
                                             $$ = create_binary_node('.',pointer,ident_node);}
            | postfix_expr PLUSPLUS         {$$ = create_unary_node(PLUSPLUS,$1);}
            | postfix_expr MINUSMINUS       {$$ = create_unary_node(MINUSMINUS,$1);}
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
          | SIZEOF unary_expr       {$$ = create_unary_node(SIZEOF,$2);}
          ;

unary_op: '&'   {$$ = '&';}
        | '*'   {$$ = '*';}
        | '+'   {$$ = '+';}
        | '-'   {$$ = '-';}
        | '~'   {$$ = '~';}
        | '!'   {$$ = '!';}
        ;

cast_expr: unary_expr   {$$ = $1;}
         ;

multiplicative_expr: cast_expr                           {$$ = $1;}
                   | multiplicative_expr '*' cast_expr   {$$ = create_binary_node('*',$1,$3);}
                   | multiplicative_expr '/' cast_expr   {$$ = create_binary_node('/',$1,$3);}
                   | multiplicative_expr '%' cast_expr   {$$ = create_binary_node('%',$1,$3);}
                   ;

additive_expr: multiplicative_expr                      {$$ = $1;}
             | additive_expr '+' multiplicative_expr    {$$ = create_binary_node('+',$1,$3);}
             | additive_expr '-' multiplicative_expr    {$$ = create_binary_node('-',$1,$3);}
             ;

shift_expr: additive_expr                   {$$ = $1;}
          | shift_expr SHL additive_expr    {$$ = create_binary_node(SHL,$1,$3);}
          | shift_expr SHR additive_expr    {$$ = create_binary_node(SHR,$1,$3);}
          ;

relational_expr: shift_expr                         {$$ = $1;}
               | relational_expr '<' shift_expr     {$$ = create_binary_node('<',$1,$3);}
               | relational_expr '>' shift_expr     {$$ = create_binary_node('>',$1,$3);}
               | relational_expr LTEQ shift_expr    {$$ = create_binary_node(LTEQ,$1,$3);}
               | relational_expr GTEQ shift_expr    {$$ = create_binary_node(GTEQ,$1,$3);}
               ;

equality_expr: relational_expr                          {$$ = $1;}
             | equality_expr EQEQ relational_expr       {$$ = create_binary_node(EQEQ,$1,$3);}
             | equality_expr NOTEQ relational_expr      {$$ = create_binary_node(NOTEQ,$1,$3);}
             ;

bitwise_and_expr: equality_expr                         {$$ = $1;}
                | bitwise_and_expr '&' equality_expr    {$$ = create_binary_node('&',$1,$3);}
                ;

bitwise_xor_expr: bitwise_and_expr                          {$$ = $1;}
                | bitwise_xor_expr '^' bitwise_and_expr     {$$ = create_binary_node('^',$1,$3);}
                ;

bitwise_or_expr: bitwise_xor_expr                           {$$ = $1;}
               | bitwise_or_expr '|' bitwise_xor_expr       {$$ = create_binary_node('|',$1,$3);}
               ;

logical_and_expr: bitwise_or_expr                             {$$ = $1;}
                | logical_and_expr LOGAND bitwise_or_expr     {$$ = create_binary_node(LOGAND,$1,$3);}
                ;

logical_or_expr: logical_and_expr                            {$$ = $1;}
               | logical_or_expr LOGOR logical_and_expr      {$$ = create_binary_node(LOGOR,$1,$3);}
               ;

conditional_expr: logical_or_expr                                 {$$ = $1;}
                | logical_or_expr '?' expr ':' conditional_expr   {$$ = create_ternary_node($1,$3,$5);}
                ;

assignment_expr: conditional_expr                           {$$ = $1;}
               | unary_expr '=' assignment_expr             {$$ = create_binary_node('=',$1,$3);}
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
    | expr ',' assignment_expr      {$$ = create_binary_node(',',$1,$3);}
    ;

/* DECLARATIONS
* -------------- */

// Top level of language
decl_or_fnc_def: declaration    {}
               | fnc_def        {}
               ;

declaration: decl_specifier ';'                   {}
           | decl_specifier init_decl_list ';'    {$$ = merge_spec_decl_list($1,$2);}
           ;

decl_specifier: storage_class_specifier                     {$$ = $1;}
              | storage_class_specifier decl_specifier      {$$ = merge_decl_spec_nodes($1,$2);}
              | type_specifier                              {$$ = create_decl_spec_node($1,UNKNOWN_SC,NONE_TQ);}
              | type_specifier decl_specifier               {astnode *type_spec = create_decl_spec_node($1,NULL,NONE_TQ);
                                                             $$ = merge_decl_spec_nodes(type_spec, $2);}
              | type_qualifier                              {$$ = create_decl_spec_node(NULL,UNKNOWN_SC,$1);}
              | type_qualifier decl_specifier               {astnode *type_qual = create_decl_spec_node(NULL,UNKNOWN_SC,$1);
                                                             $$ = merge_decl_spec_nodes(type_qual,$2);}
              | fnc_specifier                               {$$ = create_decl_spec_node(NULL,UNKNOWN_SC,NONE_TQ);
                                                             set_decl_spec_node_inline($$);}
              | fnc_specifier decl_specifier                {$$ = $1;
                                                             set_decl_spec_node_inline($$);}
              ;

init_decl_list: init_decl                       {}
              | init_decl_list ',' init_decl    {}
              ;

init_decl: declarator                   {}
         /*  Initialized declarations not supported */
         | declarator '=' initializer   {} 
         ;

storage_class_specifier: TYPEDEF   {}
                       | EXTERN    {$$ = create_decl_spec_node(NULL,EXTERN_SC,NONE_TQ);}
                       | STATIC    {$$ = create_decl_spec_node(NULL,STATIC_SC,NONE_TQ);}
                       | AUTO      {$$ = create_decl_spec_node(NULL,AUTO_SC,NONE_TQ);}
                       | REGISTER  {$$ = create_decl_spec_node(NULL,REGISTER_SC,NONE_TQ);}
                       ;

type_specifier: VOID                    {$$ = create_scalar_node(VOID_ST, UNKNOWN_SS);}
              | CHAR                    {$$ = create_scalar_node(CHAR_ST, UNKNOWN_SS);}
              | SHORT                   {$$ = create_scalar_node(SHORT_ST, UNKNOWN_SS);}
              | SHORT INT               {$$ = create_scalar_node(SHORT_ST, UNKNOWN_SS);}
              | INT                     {$$ = create_scalar_node(INT_ST, UNKNOWN_SS);}
              | LONG                    {$$ = create_scalar_node(LONG_ST, UNKNOWN_SS);}
              | LONG INT                {$$ = create_scalar_node(LONG_ST, UNKNOWN_SS);}
              | LONG LONG               {$$ = create_scalar_node(LONG_LONG_ST, UNKNOWN_SS);}
              | LONG LONG INT           {$$ = create_scalar_node(LONG_LONG_ST, UNKNOWN_SS);}
              | FLOAT                   {$$ = create_scalar_node(FLOAT_ST, UNKNOWN_SS);}
              | DOUBLE                  {$$ = create_scalar_node(DOUBLE_ST, UNKNOWN_SS);}
              | LONG DOUBLE             {$$ = create_scalar_node(LONG_DOUBLE_ST, UNKNOWN_SS);}
              | SIGNED                  {$$ = create_scalar_node(UNKNOWN_ST, SIGNED_SS);}
              | UNSIGNED                {$$ = create_scalar_node(UNKNOWN_ST, UNSIGNED_SS);}
              | BOOL                    {$$ = create_scalar_node(BOOL_ST, UNKNOWN_SS);}
              | COMPLEX                 {/* Not supported currently */}
              | struct_union_specifier  {$$ = $1;}
              | enum_specifier          {$$ = $1;}
              | typedef_name            {$$ = $1;}
              ;

struct_union_specifier: struct_union '{' struct_decl_list '}'           {}
                      | struct_union IDENT '{' struct_decl_list '}'     {}
                      | struct_union IDENT   /* Reference */            {}
                      ;

struct_union: STRUCT    {}
            | UNION     {}
            ;

struct_decl_list: struct_decl                    {}
                | struct_decl_list struct_decl   {}
                ;

struct_decl: spec_qual_list ';'                     {}
           | spec_qual_list struct_declarator_list ';'    {}
           ;

spec_qual_list: type_specifier                  {}
              | type_specifier spec_qual_list   {}
              | type_qualifier                  {}
              | type_qualifier spec_qual_list   {}
              ;

struct_declarator_list: struct_declarator                             {}
                      | struct_declarator_list ',' struct_declarator  {}
                      ;

struct_declarator: declarator   {}
                 ; /* Bit fields not supported in this compiler */

enum_specifier: ; /* Enums aren't supported in this compiler */

type_qualifier: CONST       {$$ = CONST_TQ;}
              | RESTRICT    {$$ = RESTRICT_TQ;}
              | VOLATILE    {$$ = VOLATILE_TQ;}
              ;

fnc_specifier: INLINE   {/* Nothing needs to be done */}
             ;

declarator: dir_declarator           {}
          | pointer dir_declarator   {}
          ;

dir_declarator: IDENT                                {}
                 | '(' declarator ')'                {}
                 /* More complex array expressions not supported */
                 | dir_declarator '[' ']'            {}
                 | dir_declarator '[' NUMBER ']'     {}  
                 /* Compiler assumes all function declarators are () */
                 | dir_declarator '(' ')'            {}
                 ;

pointer: '*'                                {}
       | '*' type_qualifier_list            {}
       | '*' pointer                        {}
       | '*' type_qualifier_list pointer    {}
       ;

type_qualifier_list: type_qualifier                       {}
                   | type_qualifier_list type_qualifier   {}
                   ;

type_name: spec_qual_list                       {}
         | spec_qual_list abstr_declarator      {}
         ;

abstr_declarator: pointer                        {}
                | dir_abstr_declarator           {}
                | pointer dir_abstr_declarator   {}
                ;

dir_abstr_declarator: '(' abstr_declarator ')'              {}
                    /* More complex array expressions not supported */
                    | '[' ']'                               {}
                    | dir_abstr_declarator '[' ']'          {}
                    | '[' NUMBER ']'                        {}
                    | dir_abstr_declarator '[' NUMBER ']'   {}
                    /* Compiler assumes all function declarators are () */
                    | '(' ')'                               {}
                    | dir_abstr_declarator '(' ')'          {}
                    ;

typedef_name: IDENT   {}
            ;

initializer: assignment_expr   {} 
           /*  Initialized declarations not supported */
           ;

fnc_def: decl_specifier declarator compound_stmt    {}
       ;
    
compound_stmt: '{' decl_or_stmt_list '}'    {}
             ;

decl_or_stmt_list: decl_or_stmt                     {}
                 | decl_or_stmt_list decl_or_stmt   {}
                 ;

decl_or_stmt: declaration   {}
            | statement     {}
            ;

statement: compound_stmt    {}
         | expr ';'         {}
         ;


%%
/* ----------- */
/*  USER CODE  */
/* ----------- */

int main() {
    //yydebug = 0;   // Set value to 1 to enable debugging
    yyparse();
    return 0;
}

int yyerror (char const *s) {
    fprintf(stderr, "%s\n", s);
}

// Prints number of indents given
void print_indents(int num_indents) {
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
        case UNARY_TYPE: ;
            // Checks for special operators
            int u_op = node->ast_unary_op.op;
            switch(u_op) {
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
                    if(u_op < 256) {
                        fprintf(stdout, "UNARY OP %c\n", u_op);
                    } else {
                        // TODO: PRINT STRING OF SYMBOL
                        fprintf(stdout, "UNARY OP ");
                        print_keyword(u_op);
                        fprintf(stdout, "\n");
                    }
            }

            // Prints sub-node
            print_ast(node->ast_unary_op.expr,num_indents+1);

            break;

        case BINARY_TYPE: ;
            // Checks for various operators
            int b_op = node->ast_binary_op.op;
            switch(b_op) {
                // Assignment
                case '=':
                    fprintf(stdout, "ASSIGNMENT\n");
                    break;

                // Comparison
                case '<':
                case '>':
                    fprintf(stdout, "COMPARISON OP %c\n", b_op);
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
                case '.':
                    fprintf(stdout, "SELECT\n");
                    break;

                // Other
                default:   
                    if(b_op < 256) {
                        fprintf(stdout, "BINARY OP %c\n", b_op);
                    } else {
                        // TODO: PRINT STRING OF SYMBOL
                        fprintf(stdout, "BINARY OP "); 
                        print_keyword(b_op);
                        fprintf(stdout, "\n");
                    }
            }

            // Prints sub-nodes
            print_ast(node->ast_binary_op.left_expr, num_indents+1);
            print_ast(node->ast_binary_op.right_expr, num_indents+1);

            break;

        case TERNARY_TYPE:
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
            num_type number = node->ast_number.number;
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
            print_string(node->ast_string.string);
            fprintf(stdout, "\n");

            break;

        case CHARLIT_TYPE:
            fprintf(stdout, "CHARLIT ");
            print_string(node->ast_charlit.charlit);
            fprintf(stdout, "\n");

            break;

        case FUNCTION_CALL_TYPE:
            fprintf(stdout, "FNCALL, %i arguments\n", node->ast_fnc_call.num_arguments);

            // Prints function name
            print_ast(node->ast_fnc_call.function_name, num_indents+1);

            // Checks for empty expr_list
            if(node->ast_fnc_call.expr_list_head == NULL) {
                break;
            }

            // Prints arguments
            astnode_argument *curr_argument = &(node->ast_fnc_call.expr_list_head->ast_expr_list_head);
            for(int arg_number = 1; curr_argument != NULL; arg_number++, curr_argument = curr_argument->next) {
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