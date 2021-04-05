/* Andrew Lorber */
/* Compilers - Parser */

/* ------------- */
/*  Definitions  */
/* ------------- */

%{
#include "numType.h"
#include "parser.h"
#include "astFunctions.h"
#include "symbol_table.h"
#include "../Lexer/lexerFunctions.h"

/* Remove comments to enable debugging */
#define YYDEBUG	1
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
%token<op> ELLIPSIS AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT DO DOUBLE 
%token<op> ELSE ENUM EXTERN FLOAT FOR GOTO IF INLINE INT LONG REGISTER 
%token<op> RESTRICT RETURN SHORT SIGNED STATIC STRUCT SWITCH TYPEDEF UNION 
%token<op> UNSIGNED VOID VOLATILE WHILE _BOOL _COMPLEX _IMAGINARY

/* Types (in order of grammar) */
%type<node> start_expr primary_expr postfix_expr function_call expr_list unary_expr
%type<op> unary_op
%type<node> cast_expr multiplicative_expr additive_expr shift_expr
%type<node> relational_expr equality_expr bitwise_and_expr bitwise_xor_expr
%type<node> bitwise_or_expr logical_and_expr logical_or_expr conditional_expr
%type<node> assignment_expr
%type<op> assignment_op
%type<node> expr const_expr
%type<op> '=' '<' '>' '!' '~' '(' ')' '[' ']' '.'
%type<node> decl_or_fnc_def_list decl_or_fnc_def declaration decl_specifier 
%type<node> init_decl_list init_decl storage_class_specifier type_specifier 
%type<node> struct_union_specifier
%type<op> struct_union
%type<node> struct_decl_list struct_decl spec_qual_list struct_declarator_list
%type<node> struct_declarator enum_specifier 
%type<op> type_qualifier
%type<node> fnc_specifier declarator dir_declarator pointer type_qualifier_list
%type<node> type_name abstr_declarator dir_abstr_declarator typedef_name
%type<node> initializer fnc_def fnc_block statement labeled_stmt compound_stmt
%type<node> block_item_list block_item expr_stmt selection_stmt iter_stmt jump_stmt

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

%start decl_or_fnc_def_list

%%
/* ------- */
/*  RULES  */
/* ------- */

/* EXPRESSIONS
* -------------- */

start_expr: expr ';'                {print_ast($1,0,0);}
          | start_expr expr ';'     {print_ast($2,0,0);}
          ;

primary_expr: IDENT           { /* Searches symbol table */
                                astnode *sym_entry = searchScopeStack($1,OTHER_NS);
                                // If not found, error
                                if(sym_entry == NULL) {
                                    fprintf(stderr, "ERROR: UNKNOWN IDENT %s.\n",$1);
                                }
                                $$ = sym_entry;
                              }
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

expr_list: assignment_expr                  {$$ = init_node_list($1);}
         | expr_list ',' assignment_expr    {$$ = add_node_to_list($1, $3);}
         ;

unary_expr: postfix_expr                {$$ = $1;}
          | PLUSPLUS unary_expr         {astnode *num_one = create_num_one_node();
                                        $$ = simplify_compound_op('+', $2, num_one);}
          | MINUSMINUS unary_expr       {astnode *num_one = create_num_one_node();
                                        $$ = simplify_compound_op('-', $2, num_one);}
          | unary_op cast_expr          {$$ = create_unary_node($1,$2);}
          | SIZEOF unary_expr           {$$ = create_unary_node(SIZEOF,$2);}
          | SIZEOF '(' type_name ')'    {$$ = create_unary_node(SIZEOF,$3);}
          ;

unary_op: '&'   {$$ = '&';}
        | '*'   {$$ = '*';}
        | '+'   {$$ = '+';}
        | '-'   {$$ = '-';}
        | '~'   {$$ = '~';}
        | '!'   {$$ = '!';}
        ;

cast_expr: unary_expr                   {$$ = $1;}
         | '(' type_name ')' cast_expr  {/* How should this be represented */}
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

const_expr: conditional_expr    {$$ = $1;}
          ;

/* DECLARATIONS
* -------------- */

// Top level of language
decl_or_fnc_def_list: decl_or_fnc_def                       {fprintf(stderr,"\n\n");}
                    | decl_or_fnc_def_list decl_or_fnc_def  {fprintf(stderr,"\n\n");}


decl_or_fnc_def: declaration    {print_ast($1,0,0);}
               | fnc_def        {}
               ;

declaration: decl_specifier ';'                   {$$ = $1;}
           | decl_specifier init_decl_list ';'    {$$ = merge_spec_decl_list($1,$2);
                                                   addEntryToNamespace(OTHER_NS,$$,0);}
           ;

decl_specifier: storage_class_specifier                     {$$ = $1;}
              | storage_class_specifier decl_specifier      {$$ = merge_decl_spec_nodes($1,$2);}
              | type_specifier                              {$$ = create_decl_spec_node($1,UNKNOWN_SC,NONE_TQ);}
              | type_specifier decl_specifier               {astnode *type_spec = create_decl_spec_node($1,UNKNOWN_SC,NONE_TQ);
                                                             $$ = merge_decl_spec_nodes(type_spec, $2);}
              | type_qualifier                              {$$ = create_decl_spec_node(0,UNKNOWN_SC,$1);}
              | type_qualifier decl_specifier               {astnode *type_qual = create_decl_spec_node(0,UNKNOWN_SC,$1);
                                                             $$ = merge_decl_spec_nodes(type_qual,$2);}
              | fnc_specifier                               {$$ = create_decl_spec_node(0,UNKNOWN_SC,NONE_TQ);
                                                             set_decl_spec_node_inline($$);}
              | fnc_specifier decl_specifier                {$$ = $2;
                                                             set_decl_spec_node_inline($$);}
              ;

init_decl_list: init_decl                       {$$ = init_node_list($1);}
              | init_decl_list ',' init_decl    {$$ = add_node_to_list($1,$3);}
              ;

init_decl: declarator                   {$$ = $1;}
         | declarator '=' initializer   {/*  Initialized declarations not supported */} 
         ;

storage_class_specifier: TYPEDEF   {}
                       | EXTERN    {$$ = create_decl_spec_node(0,EXTERN_SC,NONE_TQ);}
                       | STATIC    {$$ = create_decl_spec_node(0,STATIC_SC,NONE_TQ);}
                       | AUTO      {$$ = create_decl_spec_node(0,AUTO_SC,NONE_TQ);}
                       | REGISTER  {$$ = create_decl_spec_node(0,REGISTER_SC,NONE_TQ);}
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
              | _BOOL                    {$$ = create_scalar_node(BOOL_ST, UNKNOWN_SS);}
              | _COMPLEX                 {/* Not supported currently */}
              | struct_union_specifier  {$$ = $1;}
              | enum_specifier          {$$ = $1;}
              /*| typedef_name            {$$ = $1;} Typedefs not supported*/
              ;

struct_union_specifier: struct_union '{' struct_decl_list '}'           {$$ = create_struct_union_sym_entry($1,NULL,1);
                                                                         add_struct_union_members($$,$3);}
                      | struct_union IDENT                              {$<node>$ = create_struct_union_sym_entry($1,$2,0);}
                        '{' struct_decl_list '}'                        {add_struct_union_members($<node>3,$5);
                                                                         $$ = $<node>3;
                                                                         /* Prints Struct / Union */
                                                                         print_ast($<node>$,0,0);
                                                                         fprintf(stdout,"MEMBERS:\n");
                                                                         print_ast($5,1,1);
                                                                         fprintf(stdout,"\n");}
                      | struct_union IDENT   /* Reference */            {$$ = create_struct_union_sym_entry($1,$2,0);}
                      ;

struct_union: STRUCT    {$$ = 1;}
            | UNION     {$$ = 0;}
            ;

struct_decl_list: struct_decl                    {$$ = $1;}
                | struct_decl_list struct_decl   {$$ = merge_node_lists($1,$2);}
                ;

struct_decl: spec_qual_list ';'                         {$$ = $1;}
           | spec_qual_list struct_declarator_list ';'  {$$ = merge_spec_decl_list($1,$2);}
           ;

spec_qual_list: type_specifier                  {$$ = create_decl_spec_node($1,UNKNOWN_SC,NONE_TQ);}
              | type_specifier spec_qual_list   {astnode *type_spec = create_decl_spec_node($1,0,NONE_TQ);
                                                 $$ = merge_decl_spec_nodes(type_spec, $2);}
              | type_qualifier                  {$$ = create_decl_spec_node(0,UNKNOWN_SC,$1);}
              | type_qualifier spec_qual_list   {astnode *type_qual = create_decl_spec_node(0,UNKNOWN_SC,$1);
                                                 $$ = merge_decl_spec_nodes(type_qual,$2);}
              ;

struct_declarator_list: struct_declarator                             {$$ = init_node_list($1);}
                      | struct_declarator_list ',' struct_declarator  {$$ = add_node_to_list($1,$3);}
                      ;

struct_declarator: declarator   {$$ = $1;}
                 ; /* Bit fields not supported in this compiler */

enum_specifier: ; /* Enums aren't supported in this compiler */

type_qualifier: CONST       {$$ = CONST_TQ;}
              | RESTRICT    {$$ = RESTRICT_TQ;}
              | VOLATILE    {$$ = VOLATILE_TQ;}
              ;

fnc_specifier: INLINE   {/* Nothing needs to be done */}
             ;

declarator: dir_declarator           {$$ = $1;}
          | pointer dir_declarator   {$$ = build_declarator($1,$2);}
          ;

dir_declarator: IDENT                             {$$ = create_sym_table_entry($1);}
              | '(' declarator ')'                {$$ = $2;}
              /* More complex array expressions not supported */
              | dir_declarator '[' ']'            {$$ = create_arr_fnc_sym_entry($1,ARRAY_TYPE,-1);}
              | dir_declarator '[' NUMBER ']'     {$$ = create_arr_fnc_sym_entry($1,ARRAY_TYPE,$3.i_value);}  
              /* Compiler assumes all function declarators are () */
              | dir_declarator '(' ')'            {$$ = create_arr_fnc_sym_entry($1,FUNCTION_TYPE,-1);}
              ;

pointer: '*'                                {$$ = create_pointer_node(NULL,NULL);}
       | '*' type_qualifier_list            {$$ = create_pointer_node(NULL,$2);}
       | '*' pointer                        {$$ = create_pointer_node($2,NULL);}
       | '*' type_qualifier_list pointer    {$$ = create_pointer_node($3,$2);}
       ;

type_qualifier_list: type_qualifier                       {$$ = create_decl_spec_node(0,UNKNOWN_SC,$1);}
                   | type_qualifier_list type_qualifier   {astnode *type_qual = create_decl_spec_node(0,UNKNOWN_SC,$2);
                                                           $$ = merge_decl_spec_nodes(type_qual,$1);}
                   ;

type_name: spec_qual_list                       {$$ = create_type_name_node($1,NULL);}
         | spec_qual_list abstr_declarator      {$$ = create_type_name_node($1,$2);}
         ;

abstr_declarator: pointer                        {$$ = $1;}
                | dir_abstr_declarator           {$$ = $1;}
                | pointer dir_abstr_declarator   {$$ = build_abstract_declarator($1,$2);}
                ;

dir_abstr_declarator: '(' abstr_declarator ')'              {$$ = $2;}
                    /* More complex array expressions not supported */
                    | '[' ']'                               {$$ = create_array_node(-1,NULL);}
                    | dir_abstr_declarator '[' ']'          {$$ = create_array_node(-1,$1);}
                    | '[' NUMBER ']'                        {$$ = create_array_node($2.i_value,NULL);}
                    | dir_abstr_declarator '[' NUMBER ']'   {$$ = create_array_node($3.i_value,$1);}
                    /* Compiler assumes all function declarators are () */
                    | '(' ')'                               {$$ = create_function_node(-1,NULL,NULL);}
                    | dir_abstr_declarator '(' ')'          {// Function won't work if dir_abstr_declarator is a function.
                                                             // Isn't syntactically allowed, so checks here for error
                                                             if($1->node_type == FUNCTION_TYPE) {
                                                                // ERROR - Function cannot have return type of function
                                                             } else {
                                                                $$ = add_to_arr_ptr_chain($1, FUNCTION_TYPE);
                                                             };
                                                            }
                    ;

typedef_name: IDENT   {/* Should this return value from symbol table? */}
            ;

initializer: assignment_expr   {$$ = $1;} 
           /*  Initialized declarations not supported */
           ;

fnc_def: decl_specifier declarator  {/* Checks if function is in symbol table */
                                     /* If yes, updates entry fields */
                                     $<node>$ = searchScopeStack($2->ast_sym_entry.symbol,OTHER_NS);
                                     /* If no, uses new entry & adds to scope */
                                     int add_to_scope = 0;
                                     if($<node>$ == NULL) {
                                         $<node>$ = $2; 
                                         add_to_scope = 1;
                                     }
                                     /* Merges decl_specifier & declarator */
                                     $<node>$->ast_sym_entry.sym_type = FNC_NAME_TYPE;
                                     $<node>$->ast_sym_entry.filename = strdup(filename);
                                     $<node>$->ast_sym_entry.line_num = line_number;
                                     $<node>$->ast_sym_entry.ident_fnc_name.storage_class = EXTERN_SC;
                                     $<node>$->ast_sym_entry.ident_fnc_name.is_inline = $1->ast_decl_spec.is_inline;
                                     $<node>$->ast_sym_entry.ident_fnc_name.is_defined = 1;
                                     $<node>$->ast_sym_entry.ident_fnc_name.arg_types = NULL;

                                     // Sets return type
                                     build_declarator($1->ast_decl_spec.type_specifier,$<node>$); 

                                     if(add_to_scope) {
                                         addEntryToNamespace(OTHER_NS,$<node>$,0);
                                     }
                                    
                                     // Prints function declaration
                                     print_ast($<node>$,0,0);
                                    }
         fnc_block                   {$$ = $<node>3;
                                     $$->ast_sym_entry.sym_node = $4;
                                    }
       ;

fnc_block: '{'                       {/* Creates new scope */
                                     createNewScope(FUNCTION_SCOPE);} 
           block_item_list '}'      {$$ = create_compound_stmt_node($3,getInnerScope());
                                     /* Removes inner scope */
                                     deleteInnerScope();
                                    }
        ;

/* STATEMENTS
* -------------- */

statement: labeled_stmt     {$$ = $1;}
         | compound_stmt    {$$ = $1;}
         | expr_stmt        {$$ = $1;}
         | selection_stmt   {$$ = $1;}
         | iter_stmt        {$$ = $1;}
         | jump_stmt        {$$ = $1;}
         ;

labeled_stmt: IDENT ':' statement           {}
            | CASE const_expr ':' statement {}
            | DEFAULT ':' statement         {}
            ;

compound_stmt: '{' '}'                  {$$ = create_compound_stmt_node(NULL,NULL);}
             | '{'                      {/* Creates new scope */
                                         createNewScope(BLOCK_SCOPE);} 
                block_item_list '}'     {$$ = create_compound_stmt_node($3,getInnerScope());
                                         /* Removes inner scope */
                                         deleteInnerScope();
                                        }
             ;

block_item_list: block_item                     {$$ = init_node_list($1);
                                                 print_ast($1,0,0);}
               | block_item_list block_item     {$$ = add_node_to_list($1,$2);
                                                 print_ast($2,0,0);}
               ;

block_item: declaration   {$$ = $1;}
          | statement     {$$ = $1;}
          ;

expr_stmt: ';'        {/* Do Nothing */}
         | expr ';'   {$$ = $1;}
         ;

selection_stmt: IF '(' expr ')' statement                   {$$ = create_if_else_node($3,$5,NULL);}
              | IF '(' expr ')' statement ELSE statement    {$$ = create_if_else_node($3,$5,$7);}
              | SWITCH '(' expr ')' statement               {$$ = create_switch_node($3,$5);}
              ;

iter_stmt: WHILE '(' expr ')' statement                     {$$ = create_while_loop_node(0,$3,$5);}
         | DO statement WHILE '(' expr ')' ';'              {$$ = create_while_loop_node(1,$5,$2);}
         | FOR '(' ';' ';' ')' statement                    {$$ = create_for_loop_node(NULL,NULL,NULL,$6);}
         | FOR '(' ';' ';' expr ')' statement               {$$ = create_for_loop_node(NULL,NULL,$5,$7);}
         | FOR '(' ';' expr ';' ')' statement               {$$ = create_for_loop_node(NULL,$4,NULL,$7);}
         | FOR '(' ';' expr ';' expr ')' statement          {$$ = create_for_loop_node(NULL,$4,$6,$8);}
         | FOR '(' expr ';' ';' ')' statement               {$$ = create_for_loop_node($3,NULL,NULL,$7);}
         | FOR '(' expr ';' ';' expr ')' statement          {$$ = create_for_loop_node($3,NULL,$6,$8);}
         | FOR '(' expr ';' expr ';' ')' statement          {$$ = create_for_loop_node($3,$5,NULL,$8);}
         | FOR '(' expr ';' expr ';' expr ')' statement     {$$ = create_for_loop_node($3,$5,$7,$9);}
         | FOR '(' declaration ';' ')'                      {/* Not sure what to do */}
         | FOR '(' declaration ';' expr ')'                 {/* Not sure what to do */}
         | FOR '(' declaration expr ';' ')'                 {/* Not sure what to do */}
         | FOR '(' declaration expr ';' expr ')'            {/* Not sure what to do */}
         ;

jump_stmt: GOTO IDENT ';'     {$$ = create_goto_stmt_node($2);}
         | CONTINUE ';'       {$$ = create_continue_break_stmt_node(CONTINUE_STMT);}
         | BREAK    ';'       {$$ = create_continue_break_stmt_node(BREAK_STMT);}
         | RETURN ';'         {$$ = create_return_node(NULL);}
         | RETURN expr ';'    {$$ = create_return_node($2);}
         ;


%%
/* ----------- */
/*  USER CODE  */
/* ----------- */

int main() {
    yydebug = 1;   // Set value to 1 to enable debugging
    initScopeStack();  // Creates Scope Stack
    yyparse();
    return 0;
}

int yyerror (char const *s) {
    fprintf(stderr, "%s\n", s);
    return 0;
}

// Prints number of indents given
void print_indents(int num_indents) {
    // Indents
    for(int i = 0; i < num_indents; i++) {
        fprintf(stdout,"\t");
    }
}

// Prints AST
void print_ast(astnode *node, int num_indents, int is_struct_union_member) {
    fprintf(stderr,"PRINTING node type %i\n",node->node_type);

    // Prints indents
    if(node->node_type != NODE_LIST_TYPE) {
        print_indents(num_indents);
    }

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
            print_ast(node->ast_unary_op.expr,num_indents+1, is_struct_union_member);

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
            print_ast(node->ast_binary_op.left_expr, num_indents+1, is_struct_union_member);
            print_ast(node->ast_binary_op.right_expr, num_indents+1, is_struct_union_member);

            break;

        case TERNARY_TYPE:
            // IF
            fprintf(stdout, "TERNARY OP, IF:\n");
            print_ast(node->ast_ternary_op.if_expr, num_indents+1, is_struct_union_member);

            // THEN
            print_indents(num_indents);
            fprintf(stdout, "THEN:\n");
            print_ast(node->ast_ternary_op.then_expr, num_indents+1, is_struct_union_member);

            // ELSE
            print_indents(num_indents);
            fprintf(stdout, "ELSE:\n");
            print_ast(node->ast_ternary_op.else_expr, num_indents+1, is_struct_union_member);

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
            print_ast(node->ast_fnc_call.function_name, num_indents+1, is_struct_union_member);

            // Checks for empty expr_list
            if(node->ast_fnc_call.expr_list_head == NULL) {
                break;
            }

            // Prints arguments
            astnode_list_entry *curr_argument = &(node->ast_fnc_call.expr_list_head->ast_node_list_head);
            for(int arg_number = 1; curr_argument != NULL; arg_number++, curr_argument = curr_argument->next) {
                print_indents(num_indents);
                fprintf(stdout, "arg #%i=\n", arg_number);
                print_ast(curr_argument->node, num_indents+1, is_struct_union_member);
            }
            
            break;

        case NODE_LIST_TYPE: ;
            // Print each node in list
            astnode_list_entry *curr_node = &(node->ast_node_list_head);
            while(curr_node != NULL) {
                print_ast(curr_node->node, num_indents, is_struct_union_member);
                curr_node = curr_node->next;
            }

            break;

        case DECL_SPEC_TYPE:
            fprintf(stdout, "DECLARATION SPECIFIER\n");

            break;

        case TYPE_NAME_TYPE:
            fprintf(stdout, "TYPENAME\n");

            break;

        case SCALAR_TYPE:
            fprintf(stdout, "%s\n", scalarToString(node));
            break;

        case POINTER_TYPE:
            fprintf(stdout, "POINTER to:\n");
            print_ast(node->ast_pointer.pointer_type, num_indents+1, is_struct_union_member);

            break;

        case ARRAY_TYPE:
            fprintf(stdout, "ARRAY of length %i of type:\n", node->ast_array.arr_size);
            print_ast(node->ast_array.arr_type, num_indents+1, is_struct_union_member);

            break;

        case FUNCTION_TYPE:
            fprintf(stdout, "FUNCTION with return type:\n");
            print_ast(node->ast_function.return_type, num_indents+1, is_struct_union_member);

            break;

        case SYM_ENTRY_TYPE:
            fprintf(stdout, "SYMBOL %s as %s @ line %i in file %s.\n", node->ast_sym_entry.symbol, identTypeToString(node), node->ast_sym_entry.line_num, node->ast_sym_entry.filename);
            print_indents(num_indents);

            // Doesn't print if struct / union member
            if(is_struct_union_member) {
                fprintf(stdout, "In STRUCT / UNION scope.\n");
            } else {
                // Gets scope of variable
                scopeEntry *curr_scope = getInnerScope();
                fprintf(stdout, "In %s scope, which began @ line %i in file %s.\n", scopeTypeToString(curr_scope->scope), curr_scope->scope_start_line, curr_scope->scope_start_file);
            }
            
            switch(node->ast_sym_entry.sym_type) {
                case VAR_TYPE:
                    if(is_struct_union_member == 0) {
                        print_indents(num_indents);
                        fprintf(stdout, "Storage Class: %s.\n", storageClassToString(node->ast_sym_entry.ident_var.storage_class));
                    }

                    print_indents(num_indents);
                    fprintf(stdout, "DATA TYPE:\n");

                    if(node->ast_sym_entry.ident_var.type_qual != NONE_TQ) {
                        print_indents(num_indents+1);
                        fprintf(stdout,"%s",typeQualToString(node->ast_sym_entry.ident_var.type_qual));
                    }
                    
                    print_ast(node->ast_sym_entry.sym_node, num_indents+1, is_struct_union_member);

                    break;
                    
                case FNC_NAME_TYPE:
                    print_indents(num_indents);
                    fprintf(stdout, "Storage Class: %s.\n", storageClassToString(node->ast_sym_entry.ident_fnc_name.storage_class));
                    print_indents(num_indents);
                    fprintf(stdout, "RETURN TYPE:\n");
                    print_ast(node->ast_sym_entry.ident_fnc_name.return_type, num_indents+1, is_struct_union_member);

                    break;

                case STRUCT_UNION_TAG_TYPE:
                    // Checks if defined && is anonymous (otherwise, the members were already printed)
                    if(node->ast_sym_entry.ident_struct_union_tag.is_defined == 0) {
                        print_indents(num_indents);
                        fprintf(stdout, "INCOMPLETE STRUCT / UNION\n");
                    } else if(node->ast_sym_entry.symbol == NULL) {
                        print_indents(num_indents);
                        fprintf(stdout, "MEMBERS: \n");
                        print_ast(getTableMembers(node->ast_sym_entry.ident_struct_union_tag.sym_table),num_indents+1,1);   
                    } else {
                        return;
                    }

                    break;
            }

            break;

        default:
            fprintf(stdout, "ERROR: UNKNOWN NODE\n");
    }

    // Separates expressions in output
    if(num_indents == 0) {
        fprintf(stdout, "\n");
        fprintf(stderr, "\n\n");
    }
}