/* Andrew Lorber */
/* Compilers - Parser Header File*/

#ifndef PARSER_H
#define PARSER_H

#include "../Lexer/lexer.h";

// Enum of AST node types
enum nodetype {
    UNARY_TYPE = 1,
    BINARY_TYPE,
    TERNARY_TYPE,
    NUMBER_TYPE,
    IDENT_TYPE,
    STRING_TYPE,
    CHARLIT_TYPE
};

// AST Nodes
// ---------

typedef struct astnode {
    int nodetype;
    // Union of possible nodes
    union {
        astnode_unary_op unary_op;
        astnode_binary_op binary_op;
        astnode_ternary_op ternary_op;
        astnode_number number;
        astnode_ident ident;
        astnode_string string;
        astnode_char charlit;
    };
} astnode;

typedef struct astnode_unary_op {
    int op;
    astnode *expr;
} astnode_unary_op;

typedef struct astnode_binary_op {
    int op;
    astnode *left_expr, *right_expr;
} astnode_binary_op;

typedef struct astnode_ternary_op {
    astnode *if_expr, *then_expr, *else_expr;
} astnode_ternary_op;

typedef struct astnode_number {
    num_type number;
} astnode_number;

typedef struct astnode_ident {
    char* ident;
} astnode_ident;

typedef struct astnode_string {
    char *string;
} astnode_string;

typedef struct astnode_char {
    char charlit;
} astnode_char;

#endif // PARSER_H