/* Andrew Lorber */
/* Compilers - Parser */
/* AST Functions Header File */

#ifndef ASTFUNCTIONS_H
#define ASTFUNCTIONS_H

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "numType.h"
#include "symbol_table.h"

// Uses values from lexer
extern char filename[256];
extern int line_number;

extern int yylex();

// Enum of AST node types
enum nodetype {
    UNARY_TYPE = 1,
    BINARY_TYPE,
    TERNARY_TYPE,
    NUMBER_TYPE,
    IDENT_TYPE,  // 5
    STRING_TYPE,
    CHARLIT_TYPE,
    FUNCTION_CALL_TYPE,
    NODE_LIST_TYPE,
    DECL_SPEC_TYPE,  // 10
    TYPE_NAME_TYPE,
    LABEL_STMT_TYPE,
    COMPOUND_STMT_TYPE,
    IF_ELSE_TYPE,
    WHILE_LOOP_TYPE,  // 15
    FOR_LOOP_TYPE,
    SWITCH_TYPE,
    GOTO_STMT_TYPE,
    CONTINUE_BREAK_STMT_TYPE,
    RETURN_TYPE,  // 20
    SCALAR_TYPE,
    POINTER_TYPE,
    ARRAY_TYPE,
    FUNCTION_TYPE,
    TEMP_TYPE, // 25
    STRUCT_UNION_TYPE,
    SYM_ENTRY_TYPE
};

// --------------------
//      AST Nodes
// --------------------

// Expression Nodes
// ----------------

// Unary Op
typedef struct astnode_unary_op {
    int op;
    struct astnode *expr;
} astnode_unary_op;

// Binary Op
typedef struct astnode_binary_op {
    int op;
    struct astnode *left_expr, *right_expr;
} astnode_binary_op;

// Ternary Op
typedef struct astnode_ternary_op {
    struct astnode *if_expr, *then_expr, *else_expr;
} astnode_ternary_op;

// Number
typedef struct astnode_number {
    num_type number;
} astnode_number;

// Ident
typedef struct astnode_ident {
    char* ident;
} astnode_ident;

// String
typedef struct astnode_string {
    char *string;
} astnode_string;

// Char
typedef struct astnode_char {
    char *charlit;
} astnode_char;

// Function Call
typedef struct astnode_function_call {
    struct astnode *function_name;
    int num_arguments;
    struct astnode *expr_list_head; // List of arguments
} astnode_function_call;

// Node List - Used for function argument, decl_stmt_list
typedef struct astnode_list_entry {
    struct astnode *node;
    struct astnode_list_entry *next;
} astnode_list_entry;

// Declaration Nodes
// -----------------

// Enum of possible storage classes
enum storage_class_specifier {
    AUTO_SC = 1,
    REGISTER_SC,
    EXTERN_SC,
    STATIC_SC,
    UNKNOWN_SC
};

// Enum of possible type qualifiers
// Powers of two, so they can be compared and combined with bitwise operators
enum type_qualifier {
    NONE_TQ = 1,
    CONST_TQ = 2,
    VOLATILE_TQ = 4,
    RESTRICT_TQ = 8
};

// Declaration Specifier
typedef struct astnode_decl_specifier {
    struct astnode *type_specifier;
    int storage_class;  // Using storage_class_specifier enum
    int type_qual;      // Using type_qualifier enum (AND of all qualifiers)
    int is_inline;      // Function specifier (1 = true)
} astnode_decl_specifier;

// Type Name
typedef struct astnode_type_name {
    struct astnode *spec_qual_list;
    struct astnode *abstr_declarator;
} astnode_type_name;

// Statement Nodes
// ---------------

// Enum for label types
enum label_type {
    GOTO_LABEL = 1,
    CASE_LABEL,
    DEFAULT_LABEL
};

// Label Statement
typedef struct astnode_label_stmt {
    int label_type;   // Type of label, using label_type enum above
    struct astnode *label;   // Either the symbol table entry of the label, or constant expresssion for "case"
    struct astnode *stmt;
} astnode_label_stmt;

// Compound Statement
typedef struct astnode_compound_stmt {
    struct astnode *statement_block; // List of statements in block;
    struct scope_entry *block_scope;  // Symbol table of block scope
} astnode_compound_stmt;

// If Else statement
typedef struct astnode_if_else {
    struct astnode *if_condition;
    struct astnode *if_body;
    struct astnode *else_body;   // Either if_else node for "else if" statment, or statement node for "else"
} astnode_if_else;

// While Loop
typedef struct astnode_while_loop {
    int is_do_while;    // 1 if "do while", 0 if regular "while" loop
    struct astnode *condition;
    struct astnode *body;
} astnode_while_loop;

// For Loop
typedef struct astnode_for_loop {
    struct astnode *initialization;
    struct astnode *condition;
    struct astnode *update;
    struct astnode *body;
} astnode_for_loop;

// Switch Statment
typedef struct astnode_switch {
    struct astnode *expr;
    struct astnode *label_list;
} astnode_switch;

// Goto Statement
typedef struct astnode_goto_stmt {
    struct astnode *label;
} astnode_goto_stmt;

// Enum for continue / break node type
enum continue_break {
    CONTINUE_STMT = 1,
    BREAK_STMT
};

typedef struct astnode_continue_break_stmt {
    int type; // Uses continue_break enum above
} astnode_continue_break_stmt;

// Return Statment
typedef struct astnode_return {
    struct astnode *return_expr;
} astnode_return;

// Type Nodes
// ----------

// Enum of Scalar types
enum scalar_types {
    VOID_ST = 1,
    CHAR_ST,
    SHORT_ST,
    INT_ST,
    LONG_ST,
    LONG_LONG_ST,
    FLOAT_ST,
    DOUBLE_ST,
    LONG_DOUBLE_ST,
    BOOL_ST,
    UNKNOWN_ST
};

// Enum for signed-ness of scalar type
enum scalar_sign {
    SIGNED_SS = 1,
    UNSIGNED_SS,
    UNKNOWN_SS
};

// Scalar Type
typedef struct astnode_scalar {
    int scalar_type;  // Type of scalar (Scalar_type enum above)
    int is_signed;    // Whether scalar is signed (scalar_sign enum above)
} astnode_scalar;

// Pointer Type
typedef struct astnode_pointer {
    struct astnode *pointer_type;  // Type of pointer
    int type_qual;          // Using type_qualifier enum (AND of all qualifiers)
} astnode_pointer;

// Array Type
typedef struct astnode_array {
    int arr_size;       // Size of array (-1 = undefined)
    struct astnode *arr_type;  // Type of array
} astnode_array;

// Function Type
typedef struct astnode_function {
    int num_args;                  // Number of arguments (-1 = undefined)
    struct astnode *return_type;   // Return type
    struct astnode *arg_types;     // Argument types (ast node list)
} astnode_function;

// IR Gen Nodes
// ------------

// Temp node used for IR generation
typedef struct astnode_temp_node {
    char *name;
    astnode *temp_value; // Can store whatever value needs to be stored
} astnode_temp_node;

// Symbol Table Nodes
// ------------------

// Variable
typedef struct astnode_ident_var {
    struct astnode *var_type;      // Type of variable
    int storage_class;      // Storage Class (storage_class_specifier enum)
    int type_qual;          // Type Qualifier (type_qualifier enum)
    int stack_frame_offset; // Offset within stack frame (AUTO storage class only)
} astnode_ident_var;

// Function Name
typedef struct astnode_ident_fn_name {
    int storage_class;      // Storage Class enum
    struct astnode *return_type;   // Return Type
    struct astnode *arg_types;     // Argument types (ast node list)
    int is_inline;          // Whether function was declared as inline (1 = yes)
    int is_defined;         // Whether function definition has been seen (1 = yes)
} astnode_ident_fn_name;

// Typedef
typedef struct astnode_ident_typedef {
    struct astnode *equivalent_type;  // Equivalent type to ident
} astnode_ident_typedef;

// Enum Constant
typedef struct astnode_ident_enum_const {
    struct astnode_sym_table_entry *enum_tag;  // Enum tag of enum constant
    int enum_value;                     // Value of enum constant
} astnode_ident_enum_const;

// Struct & Union Tag
typedef struct astnode_ident_struct_union_tag {
    int is_struct;                   // 1 if struct, 0 if union
    struct symbol_table *sym_table;   // Symbol Table of member definitions
    int is_defined;                  // Whether definition is complete (1 = yes)
} astnode_ident_struct_union_tag;

// Enum Tag
typedef struct astnode_ident_enum_tag {
    int is_defined;  // Whether definition is complete (1 = yes)
} astnode_ident_enum_tag;

// Label
typedef struct astnode_ident_label {
    // Intermediate code or assembly language label
} astnode_ident_label;

// Struct & Union Member
typedef struct astnode_ident_struct_union_member {
    int offset;                // Offset within struct or union
    // Bit field width (Not supported)
    // Bit offset (Not supported)
} astnode_ident_struct_union_member;

// Enum of IDENT node types
enum ident_type {
    VAR_TYPE = 1,
    FNC_NAME_TYPE,
    TYPEDEF_TYPE,
    ENUM_CONST_TYPE,
    STRUCT_UNION_TAG_TYPE,
    ENUM_TAG_TYPE,
    LABEL_TYPE,
    STRUCT_UNION_MEMBER_TYPE
};

// Symbol table entry struct
typedef struct astnode_sym_table_entry {
    char *symbol;               // IDENT symbol
    struct astnode *sym_node;   // Value of symbol
    int sym_type;               // Type of symbol

    char *filename;             // File of symbol's first def.
    int line_num;               // Line # of symbol's first def.

    // Structs for possible IDENT types
    union {
        astnode_ident_var ident_var;
        astnode_ident_fn_name ident_fnc_name;
        astnode_ident_typedef ident_typedef;
        astnode_ident_enum_const ident_enum_const;
        astnode_ident_struct_union_tag ident_struct_union_tag;
        astnode_ident_enum_tag ident_enum_tag;
        astnode_ident_label ident_label;
        astnode_ident_struct_union_member ident_struct_union_member;
    };
} astnode_sym_table_entry;

// Main AST Node Struct
typedef struct astnode {
    int node_type;

    // Union of possible nodes
    union {
        // Expression Nodes
        astnode_unary_op ast_unary_op;
        astnode_binary_op ast_binary_op;
        astnode_ternary_op ast_ternary_op;
        astnode_number ast_number;
        astnode_ident ast_ident;
        astnode_string ast_string;
        astnode_char ast_charlit;
        astnode_function_call ast_fnc_call;
        astnode_list_entry ast_node_list_head;

        // Declaration Nodes
        astnode_decl_specifier ast_decl_spec;
        astnode_type_name ast_type_name;

        // Statement Nodes
        astnode_label_stmt ast_label_stmt;
        astnode_compound_stmt ast_compound_stmt;
        astnode_if_else ast_if_else;
        astnode_while_loop ast_while_loop;
        astnode_for_loop ast_for_loop;
        astnode_switch ast_switch;
        astnode_goto_stmt ast_goto_stmt;
        astnode_continue_break_stmt ast_continue_break_stmt;
        astnode_return ast_return;

        // Type Nodes
        astnode_scalar ast_scalar;
        astnode_pointer ast_pointer;
        astnode_array ast_array;
        astnode_function ast_function;

        // IR Gen Nodes
        astnode_temp_node ast_temp_node;

        // Symbol Table Node
        astnode_sym_table_entry ast_sym_entry;
    };
} astnode;

// -------------------------
//      AST Functions
// -------------------------

// Expression Nodes
// -----------------

astnode *allocate_node_mem();
astnode *create_unary_node(int op, astnode *expr); 
astnode *create_binary_node(int op, astnode *left, astnode *right); 
astnode *simplify_compound_op(int op, astnode *left, astnode *right); // For compound operators (i.e +=)
astnode *create_ternary_node(astnode *if_expr, astnode *then_expr, astnode *else_expr); 
astnode *create_number_node(num_type number); 
astnode *create_ident_node(char *ident); 
astnode *create_string_node(char *string); 
astnode *create_char_node(char *charlit);
astnode *create_fnc_call_node(astnode *function_name, astnode *expr_list);
astnode *init_node_list(astnode *node_list_head);
astnode *add_node_to_list(astnode *node_list, astnode *new_argument);
astnode *merge_node_lists(astnode *node_list, astnode *list_to_merge);

// Helper function to create number node with value of 1 (for ++ & --)
astnode *create_num_one_node();

// Declaration Nodes
// -----------------

// Creates declaration specifier node
astnode *create_decl_spec_node(astnode* type_spec, int storage_class, int type_qual);

// Sets decl_spec_node function specifier to 'inline'
void set_decl_spec_node_inline(astnode *decl_spec);

// Merges declarator specifiers
// Adds updates from "addition" to "decl_spec" and frees "addition"
astnode *merge_decl_spec_nodes(astnode* addition, astnode *decl_spec);

// Merges declarator specifiers with declarator list
astnode *merge_spec_decl_list(astnode *spec, astnode* decl_list);

// Fills declarator specifier with necessary default values
void fill_defaults(astnode *specifier);

// Combines pointer into declarator symbol table entry
astnode *build_declarator(astnode *ptr, astnode *declarator);

// Combines pointer into abstract declarator
astnode *build_abstract_declarator(astnode *ptr, astnode *declarator);

// Creates type-name node
astnode *create_type_name_node(astnode *spec_qual_list, astnode *abstr_decl);

// Statement Nodes
// ---------------

astnode *create_label_stmt_node(int label_type, astnode *label, astnode *statement);
astnode *create_compound_stmt_node(astnode *statement_block, struct scope_entry *block_scope);
astnode *create_if_else_node(astnode *if_condition, astnode *if_body, astnode *else_body);
astnode *create_while_loop_node(int is_do_while, astnode *condition, astnode *body);
astnode *create_for_loop_node(astnode *initialization, astnode *condition, astnode *update, astnode *body);
astnode *create_switch_node(astnode *expr, astnode *label_list);
astnode *create_goto_stmt_node(char *label);
astnode *create_continue_break_stmt_node(int type);
astnode *create_return_node(astnode *return_expr);

// Type Nodes
// ----------

astnode *create_scalar_node(int scalar_type, int is_signed);
astnode *create_pointer_node(astnode *ptr_type, astnode *type_qual_list);
astnode *create_array_node(int size, astnode *type);
astnode *create_function_node(int num_args, astnode *return_type, astnode *arg_types);

// Adds node to end of array / pointer chain
astnode *add_to_arr_ptr_chain(astnode*parent_node, int type_to_add);

// IR Gen Nodes
// ------------

// Creates a temp node to be used for IR generation
// temp_num is the number temp nodes, to be used for name creation
astnode *create_temp_node(int temp_num);

// Symbol Table Nodes
// ------------------

// Creates a new symbol table entry, only filling symbol field
// Sets default type to VAR TYPE
astnode *create_sym_table_entry(char *ident);

// Updates a symbol table entry by adding an array or function node
// If type_to_add = FUNCTION_TYPE & no symbol table node, then entry is set to FNC_NAME_TYPE,
//   otherwise new node is added to end of sym_node chain
// type_to_add parameter uses same type enum as astnode
// arr_size parameter is only used for array nodes
astnode *create_arr_fnc_sym_entry(astnode *sym_table_entry, int type_to_add, int arr_size);

// Creates struct / union symbol table entry
astnode *create_struct_union_sym_entry(int struct_or_union, char *ident, int is_abstract);

// Adds struct / union members to struct / union symbol table
int add_struct_union_members(astnode *struct_union_node, astnode *members);

// Printer Functions
// ------------------

// Converts storage class enum to string for printing
char *storage_class_to_string(int storage_class);

// Converts type qualifier enum to string for printing
char *type_qual_to_string(int type_qual);

// Converts scalar node to string representing value
char *scalar_to_string(astnode *scalar_node);

// Converts IDENT type enum to string for printing
char *ident_type_to_string(astnode *node);

#endif // ASTFUNCTIONS_H