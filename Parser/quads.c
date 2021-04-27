/* Andrew Lorber */
/* Compilers - Parser */
/* Quads File */

#include "quads.h"
#include "parser.tab.h"

// Global Variables
// ----------------

struct basic_block_list_entry *block_list_head;
struct basic_block *curr_block;
struct basic_block *break_block;
struct basic_block *continue_block;
struct quad_list_entry *curr_quad;

// Functions
// ----------

// Creates a new basic block
struct basic_block *create_basic_block(char *block_label) {
    basic_block *b_block;
    
    //  Checks for errors
    if((b_block = malloc(sizeof(b_block))) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for basic block.\n");
        exit(-1);
    }

    // TODO: Default name if none is given
    b_block->block_label = block_label;

    b_block->quad_list = NULL;
    b_block->next = NULL;

    return b_block;
}

// Sets block as current block and updates global variable
void set_block(basic_block *block) {
    curr_block = block;
    curr_quad = curr_block->quad_list;

    // Moves to end of quad list
    if(curr_quad == NULL) {
        return;
    }
    while(curr_quad->next != NULL) {
        curr_quad = curr_quad->next;
    }
}

// Generates the quads for a function and stores blocks in linked list
void generate_function_quads() {

}

// Given AST node, recursively generates quads
void generate_quads(struct astnode *node) {

    // Checks node type
    switch(node->node_type) {
        // Assignment
        case BINARY_TYPE:
            if(node->ast_binary_op.op == '=') {
                gen_assignment_IR(node);
                return NULL;
            }

        // If statement
        case IF_ELSE_TYPE:
            gen_if_stmt_IR(node);
            return NULL;
    }

    fprintf(stderr, "No quads to generate.\n");
}

// Gets r value of expression
struct astnode *get_rvalue(struct astnode *node, struct astnode *target) {
    
    // Checks type of node
    switch(node->node_type) {
        // Variables
        case SYM_ENTRY_TYPE:
            // Scalar Variable
            if(node->ast_sym_entry.sym_type == VAR_TYPE 
            && node->ast_sym_entry.ident_var.var_type->node_type == SCALAR_TYPE) {
                return node;
            }

            // Array Varible
            if(node->ast_sym_entry.sym_type == VAR_TYPE 
            && node->ast_sym_entry.ident_var.var_type->node_type == ARRAY_TYPE) {
                astnode *temp = create_temp_node();
                emit_quad(LEA_OC, NULL, node, NULL, temp);
                return temp;
            }
            break; // Will it ever get here?

        // Constants
        case NUMBER_TYPE:
        case CHARLIT_TYPE:
        case STRING_TYPE:
            return node;

        // Binary Operations
        case BINARY_TYPE:
            astnode *left = get_rvalue(node->ast_binary_op.left_expr, NULL);
            astnode *right = get_rvalue(node->ast_binary_op.right_expr, NULL);

            // Gets op code
            int op_code = -1;
            switch(node->ast_binary_op.op) {
                case '+':
                    op_code = ADD_OC;
                    break;
                case '-':
                    op_code = SUB_OC;
                    break;
                case '*':
                    op_code = MUL_OC;
                    break;
                case '/':
                    op_code = DIV_OC;
                    break;
                case '%':
                    op_code = MOD_OC;
                    break;
                case '^':
                    op_code = XOR_OC;
                    break;
                case '&':
                    op_code = AND_OC;
                    break;
                case '|':
                    op_code = OR_OC;
                    break;
                case SHL:
                    op_code = SHL_OC;
                    break;
                case SHR:
                    op_code = SHR_OC;
                    break;
            }

            // Check if binary expression
            if(op_code > 0) {
                // TODO: Check if pointer arithmetic is needed

                // Checks if target
                if(target == NULL) {
                    target = create_temp_node();
                }
                emit_quad(op_code, NULL, left, right, target);
                return target;
            }

            // Else, comparative expression & assignment

            return NULL;

        case UNARY_TYPE:
            // Pointer Deref
            if(node->ast_unary_op.op == '*') {
                astnode *addr = get_rvalue(node->ast_unary_op.expr,NULL);

                // Checks if target
                if(target == NULL) {
                    target = create_temp_node();
                }

                emit_quad(LOAD_OC, NULL, addr, NULL, target);
                return target;
            }
    }
}

// Gets l value of expression
struct astnode *get_lvalue(struct astnode *node, int *mode) {
    switch(node->node_type) {
        // Scalar Variable
        case SYM_ENTRY_TYPE:
            if(node->ast_sym_entry.sym_type == VAR_TYPE
                && node->ast_sym_entry.ident_var.var_type->node_type == SCALAR_TYPE) {
                *mode = DIRECT_MODE;
                return node;
            }
            break; // Will it ever get here?

        // Constants
        case NUMBER_TYPE:
        case CHARLIT_TYPE:
        case STRING_TYPE:
            return NULL;

        case UNARY_TYPE:
            // Pointer Deref
            if(node->ast_unary_op.op == '*') {
                *mode = INDIRECT_MODE;
                return get_rvalue(node->ast_unary_op.expr,NULL);
            }
            break; // Will it ever get here?
    }

    return NULL;
}

// Creates a new quad and appends it to linked list of quads
void *emit_quad(int op_code, int op_size, struct astnode *src1, struct astnode *src2, struct astnode *dest) {
    // Creates new quad
    quad *new_quad;

    // Checks for errors
    if((new_quad = malloc(sizeof(quad))) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for quad.\n");
        exit(-1);
    }

    new_quad->op_code = op_code;
    new_quad->op_size = op_size;
    new_quad->src1 = src1;
    new_quad->src2 = src2;
    new_quad->dest = dest;

    // Creates new linked list entry
    quad_list_entry *quad_entry;

    // Checks for errors
    if((quad_entry = malloc(sizeof(quad_list_entry))) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for quad list entry.\n");
        exit(-1);
    }

    quad_entry->quad = new_quad;

    // Appends to linked list
    if(curr_quad == NULL) {
        curr_block->quad_list = quad_entry;
    } else {
        curr_quad->next = quad_entry;
    }
    curr_quad = quad_entry;
}

// Creates a temporary node
struct astnode *create_temp_node() {

}

// Generates IR for assignments
void gen_assignment_IR(astnode *node) {
    int mode;
    astnode *dest = get_lvalue(node->ast_binary_op.left_expr, &mode);

    if(mode == DIRECT_MODE) {
        astnode *rvalue = get_rvalue(node->ast_binary_op.right_expr,dest);
    } else {
        astnode *rvalue = get_rvalue(node->ast_binary_op.right_expr,NULL);
        emit_quad(STORE_OC, NULL, rvalue, dest, NULL);
    }
}

// Generates IR for if statements
void gen_if_stmt_IR(astnode *node) {
    // Creates basic blocks for branches
    astnode *true_block = create_basic_block(NULL);
    astnode *false_block = create_basic_block(NULL);

    // Checks if else statement
    astnode *next_block;
    if(node->ast_if_else.else_body) {
        next_block = create_basic_block(NULL);
    } else {
        next_block = false_block;
    }

    // Creates quads for conditional expression
    gen_conditional_expr_IR(node->ast_if_else.if_condition, node->ast_if_else.if_body, node->ast_if_else.else_body);

    // Creates quads for true branch
    set_block(true_block);
    generate_quads(node->ast_if_else.if_body);

    // Attaches true block to next block
    link_blocks(NULL,next_block,NONE_OC);

    // Creates quads for false branch
    if(node->ast_if_else.else_body) {
        set_block(false_block);
        generate_quads(node->ast_if_else.else_body);
        
        // Attaches true block to next block
        link_blocks(NULL,next_block,NONE_OC);
    }

    set_block(next_block);
}

// Generates IR for conditional expression
void gen_conditional_expr_IR(astnode *cond_expr, astnode *true_branch, astnode *false_branch) {
    // Checks if comparative expression
    if(cond_expr->node_type == BINARY_TYPE) {
        int op_code = -1;
        switch(node->ast_binary_op.op) {
            case EQEQ:
                op_code = EQEQ_OC;
                break;

            case NOTEQ:
                op_code = NEQ_OC;
                break;
            
            case '<':
                op_code = LT_OC;
                break;
            
            case '>':
                op_code = GT_OC;
                break;

            case LTEQ:
                op_code = LTEQ_OC;
                break;

            case GTEQ:
                op_code = GTEQ_OC;
                break; 
        }

        // Confirms comparative expression
        if(op_code > 0) {
            // TODO: Gets rvalue of sub expressions
            astnode *left_expr = cond_expr->ast_binary_op.left_expr;
            astnode *right_expr = cond_expr->ast_binary_op.right_expr;

            // Compare quad
            emit_quad(CMP_OC, NULL, left_expr, right_expr, NULL);

            // Attaches true and false branches to current block
            link_blocks(true_branch, false_branch, op_code);

            return;
        }
    }
    // If single expression, compare to 0
    num_type num;
    num.i_value = 0;
    num.is_signed = UNSIGNED_TYPE;
    num.size_specifier = INT_TYPE;
    astnode *num_zero = create_number_node(num);

    // Checks if constant variable
    if(cond_expr->node_type == SYM_ENTRY_TYPE && cond_expr->ast_sym_entry.sym_type == VAR_TYPE
    && cond_expr->ast_sym_entry.ident_var.var_type->node_type == SCALAR_TYPE) {
        // Compares with zero
        emit_quad(CMP_OC, NULL, cond_expr);
    } else {
        // Otherwise, attempts to determine value
        emit_quad(CMP_OC, NULL, get_rvalue(cond_expr,NULL), num_zero, NULL);
    }
    
    // Attaches true and false branches to current block
    link_blocks(true_branch, false_branch, NEQ_OC);
}

// Links given block(s) with current basic block
// Branch is set to the true branch, while next is set to the false branch
void link_blocks(astnode *true_branch, astnode *false_branch, int op_code) {
    curr_block->next = false_branch;
    curr_block->branch = true_branch;
    curr_block->branch_condition = op_code;
}

