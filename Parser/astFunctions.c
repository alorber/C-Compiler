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

// Builds a binary node for an expression with a compound operator (i.e +=)
astnode* simplify_compound_op(int op, astnode *left, astnode *right) {
        // Creates binary node for right side
        astnode *right_node = create_binary_node(op, left, right);
        
        // Creates main binary node
        astnode *binary_node = create_binary_node('=', left, right_node);

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

astnode* create_fnc_call_node(astnode *function_name, astnode *expr_list) {
    astnode *fnc_call_node = allocate_node_mem();
    fnc_call_node->node_type = FUNCTION_TYPE;
    fnc_call_node->ast_fnc_call.function_name = function_name;
    fnc_call_node->ast_fnc_call.expr_list_head = expr_list;
    
    // Calculates number of arguments
    if(expr_list == NULL) {
        fnc_call_node->ast_fnc_call.num_arguments = 0;
    } else {
        int num_arguments = 1; 

        astnode_argument *curr_argument = expr_list->ast_expr_list_head.next;
        while(curr_argument != NULL) {
            num_arguments++;
            curr_argument = curr_argument->next;
        }
        fnc_call_node->ast_fnc_call.num_arguments = num_arguments;
    }
    
    return fnc_call_node;
}

// Creates an expression list
astnode* init_expr_list(astnode* expr_list_head) {
    astnode *expr_list_node = allocate_node_mem();
    expr_list_node->node_type = EXPR_LIST_TYPE;
    expr_list_node->ast_expr_list_head.expr = expr_list_head;
    expr_list_node->ast_expr_list_head.next = NULL;

    return expr_list_node;
}

// Adds new argument node to expression list
astnode* add_argument_to_list(astnode *expr_list, astnode *new_argument) {
    // Creates new argument node
    astnode_argument *arg_node;
    
    // Checks for error
    if((arg_node = malloc(sizeof(astnode_argument))) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for AST Argument node.\n");
        exit(-1);
    }

    arg_node->expr = new_argument;
    arg_node->next = NULL;

    // Get last argument in list
    astnode_argument *curr_argument = &(expr_list->ast_expr_list_head);
    while(curr_argument->next != NULL) {
        curr_argument = curr_argument->next;
    }
    
    // Add new argument to list
    curr_argument->next = arg_node;

    // Returns head of list
    return expr_list;
}

// Creates number node with value of 1
// Helper function for "++" and "--"
astnode* create_num_one_node() {
    astnode *num_one = allocate_node_mem();
    num_one->node_type = NUMBER_TYPE;
    num_one->ast_number.number.is_signed = UNSIGNED_TYPE;
    num_one->ast_number.number.size_specifier = INT_TYPE;
    num_one->ast_number.number.i_value = 1;

    return num_one;
}
