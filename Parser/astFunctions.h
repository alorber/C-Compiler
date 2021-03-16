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

extern int yylex();

// Enum of AST node types
enum nodetype {
    UNARY_TYPE = 1,
    BINARY_TYPE,
    TERNARY_TYPE,
    NUMBER_TYPE,
    IDENT_TYPE,
    STRING_TYPE,
    CHARLIT_TYPE,
    FUNCTION_CALL_TYPE,
    EXPR_LIST_TYPE,
    DECL_SPEC_TYPE,
    SCALAR_TYPE,
    POINTER_TYPE,
    ARRAY_TYPE,
    FUNCTION_TYPE,
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

// Function Argument
typedef struct astnode_argument {
    struct astnode *expr;
    struct astnode_argument *next;
} astnode_argument;

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
enum type_qualifier {
    NONE_TQ = 1,
    CONST_TQ,
    VOLATILE_TQ,
    RESTRICT_TQ
};

typedef struct astnode_decl_specifier {
    astnode *type_specifier;
    int storage_class;  // Using storage_class_specifier enum
    int type_qual;      // Using type_qualifier enum
    int is_inline;      // Function specifier (1 = true)
} astnode_decl_specifier;

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
    astnode *pointer_type;  // Type of pointer
    // TODO: Add type qualifier
} astnode_pointer;

// Array Type
typedef struct astnode_array {
    int arr_size;       // Size of array
    astnode *arr_type;  // Type of array
} astnode_array;

// Function Type
typedef struct astnode_function {
    int num_args;           // Number of arguments
    astnode *return_type;   // Return type
    astnode **arg_types;    // Argument types
} astnode_function;

// Struct & Union Type
typedef struct astnode_struct_union {

} astnode_struct_union;


// Symbol Table Nodes
// ------------------

// Variable
typedef struct astnode_ident_var {
    astnode *var_type;      // Type of variable
    int storage_class;      // Storage Class (storage_class_specifier enum)
    int type_qual;          // Type Qualifier (type_qualifier enum)
    int stack_frame_offset; // Offset within stack frame (AUTO storage class only)
} astnode_ident_var;

// Function Name
typedef struct astnode_ident_fn_name {
    int storage_class;      // Storage Class enum
    astnode *return_type;   // Return Type
    astnode **arg_types;     // Argument types
    int is_inline;          // Whether function was declared as inline (1 = yes)
    int is_defined;         // Whether function definition has been seen (1 = yes)
} astnode_ident_fn_name;

// Typedef
typedef struct astnode_ident_typedef {
    astnode *equivalent_type;  // Equivalent type to ident
} astnode_ident_typedef;

// Enum Constant
typedef struct astnode_ident_enum_const {
    astnode_sym_table_entry *enum_tag;  // Enum tag of enum constant
    int enum_value;                     // Value of enum constant
} astnode_ident_enum_const;

// Struct & Union Tag
typedef struct astnode_ident_struct_union_tag {
    symbolTable *sym_table;   // Symbol Table of member definitions
    int is_defined;           // Whether definition is complete (1 = yes)
} astnode_ident_struct_union_tag;

// Enum Tag
typedef struct astnode_ident_enum_tag {
    int is_defined;  // Whether definition is complete (1 = yes)
} astnode_ident_enum_tag;

// Label
typedef struct astnode_ident_label {
    // Intermediate coe or assembly language label
} astnode_ident_label;

// Struct & Union Member
typedef struct astnode_ident_struct_union_member {
    astnode *type;      // Type of member
    int offset;         // Offset within struct or union
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
    char *symbol;       // IDENT symbol
    astnode *sym_node;  // Value of symbol
    int sym_type;       // Type of symbol

    char *filename;     // File of symbol's first def.
    char *line_num;     // Line # of symbol's first def.

    // Structs for possible IDENT types
    union {
        astnode_ident_var ident_var;
        astnode_ident_fn_name fnc_name;
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
        astnode_argument ast_expr_list_head;

        // Declaration Nodes
        astnode_decl_specifier ast_decl_spec;

        // Type Nodes
        astnode_scalar ast_scalar;
        astnode_pointer ast_pointer;
        astnode_array ast_array;
        astnode_function ast_function;
        astnode_struct_union ast_struct_union;

        // Symbol Table Node
        astnode_sym_table_entry ast_sym_entry;
    };
} astnode;

// -------------------------
//      AST Functions
// -------------------------

// Expression Nodes
// -----------------

astnode* allocate_node_mem();
astnode* create_unary_node(int op, astnode *expr); 
astnode* create_binary_node(int op, astnode *left, astnode *right); 
astnode* simplify_compound_op(int op, astnode *left, astnode *right); // For compound operators (i.e +=)
astnode* create_ternary_node(astnode *if_expr, astnode *then_expr, astnode *else_expr); 
astnode* create_number_node(num_type number); 
astnode* create_ident_node(char *ident); 
astnode* create_string_node(char *string); 
astnode* create_char_node(char *charlit);
astnode* create_fnc_call_node(astnode *function_name, astnode *expr_list);
astnode* init_expr_list(astnode* expr_list_head);
astnode* add_argument_to_list(astnode *expr_list, astnode *new_argument);

// Helper function to create number node with value of 1 (for ++ & --)
astnode* create_num_one_node();

// Declaration Nodes
// -----------------

// Creates declaration specifier node
astnode *create_decl_spec_node(astnode* type_spec, int storage_class, int type_qual);

// Sets decl_spec_node function specifier to 'inline'
void set_decl_spec_node_inline(astnode *decl_spec);

// Merges declarator specifiers
// Adds updates from "addition" to "decl_spec" and frees "addition"
astnode *merge_decl_spec_nodes(astnode* addition, astnode *decl_spec);

// Creates declarator list
astnode *create_decl_list(astnode *decl);

// Adds declarator to declarator list
astnode *add_decl_to_list(astnode *decl_list, astnode *decl);

// Merges declarator specifiers with declarator list
astnode *merge_spec_decl_list(astnode *spec, astnode* decl_list);

// Type Nodes
// ----------

astnode *create_scalar_node(int scalar_type, int is_signed);

#endif // ASTFUNCTIONS_H