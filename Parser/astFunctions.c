/* Andrew Lorber */
/* Compilers - Parser */
/* AST Functions File */

#include "astFunctions.h"

// AST Functions
// -------------

// Allocates memory for new node & checks for errors
astnode* allocate_node_mem() {
    astnode *tmp_node;
    
    // Checks for error
    if((tmp_node = malloc(sizeof(astnode))) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for AST node.\n");
        exit(-1);
    }

    return tmp_node;
}

// The following node functions set the type and members of the created node
//  and then return it.
astnode* create_unary_node(int op, astnode *expr) {
    astnode *unary_node = allocate_node_mem();
    unary_node->node_type = UNARY_TYPE;
    unary_node->ast_unary_op.op = op;
    unary_node->ast_unary_op.expr = expr;

    return unary_node;
}

astnode* create_binary_node(int op, astnode *left, astnode *right) {
    astnode *binary_node = allocate_node_mem();
    binary_node->node_type = BINARY_TYPE;
    binary_node->ast_binary_op.op = op;
    binary_node->ast_binary_op.left_expr = left;
    binary_node->ast_binary_op.right_expr = right;

    return binary_node;
}

astnode* create_ternary_node(astnode *if_expr, astnode *then_expr, astnode *else_expr) {
    astnode *ternary_node = allocate_node_mem();
    ternary_node->node_type = TERNARY_TYPE;
    ternary_node->ast_ternary_op.if_expr = if_expr;
    ternary_node->ast_ternary_op.then_expr = then_expr;
    ternary_node->ast_ternary_op.else_expr = else_expr;

    return ternary_node;
}

astnode* create_number_node(num_type number) {
    astnode *number_node = allocate_node_mem();
    number_node->node_type = NUMBER_TYPE;
    number_node->ast_number.number = number;

    return number_node;
}

astnode* create_ident_node(char *ident) {
    astnode *ident_node = allocate_node_mem();
    ident_node->node_type = IDENT_TYPE;
    ident_node->ast_ident.ident = strdup(ident);

    return ident_node;
}

astnode* create_string_node(char *string) {
    astnode *string_node = allocate_node_mem();
    string_node->node_type = STRING_TYPE;
    string_node->ast_string.string = string;

    return string_node;
}

astnode* create_char_node(char charlit) {
    astnode *char_node = allocate_node_mem();
    char_node->node_type = CHARLIT_TYPE;
    char_node->ast_charlit.charlit = charlit;

    return char_node;
}
