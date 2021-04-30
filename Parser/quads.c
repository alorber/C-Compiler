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

// Links given block(s) with current basic block
// Branch is set to the true branch, while next is set to the false branch
void link_blocks(basic_block *true_branch, basic_block *false_branch, int op_code) {
    curr_block->next = false_branch;
    curr_block->branch = true_branch;
    curr_block->branch_condition = op_code;
}

// Generates the quads for a function and stores blocks in linked list
void generate_function_quads() {

}

// Given AST node, recursively generates quads
void generate_quads(astnode *node) {

    // Checks node type
    switch(node->node_type) {
        // TODO: ++ & --

        // Assignment
        case BINARY_TYPE:
            if(node->ast_binary_op.op == '=') {
                gen_assignment_IR(node);
                return;
            }

        // If statement
        case IF_ELSE_TYPE:
            gen_if_stmt_IR(node);
            return;

        // (Do) While loop
        case WHILE_LOOP_TYPE:
            // Do while loop
            if(node->ast_while_loop.is_do_while) {
                gen_do_while_loop_IR(node)
            }
            // While loop
            else {
                gen_while_loop_IR(node);
            }
            
            return;

        // For loop
        case FOR_LOOP_TYPE:
            gen_for_loop_IR(node);
            return;

        // Break / Continue statement
        case CONTINUE_BREAK_STMT_TYPE:
            // Break Statement
            if(node->ast_continue_break_stmt.type == BREAK_STMT) {
                gen_break_stmt_IR();
            }
            // Continue Statement
            else {
                gen_continue_stmt_IR();
            }
            
            return;

        // Return statement
        case RETURN_TYPE:
            gen_return_stmt_IR(node);
            return;

        // Function call
        case FUNCTION_CALL_TYPE:
            gen_fnc_call_IR(node);
            return;

        // Compound statement
        case COMPOUND_STMT_TYPE:
            // Loop through statements
            astnode_list_entry *curr_statement = node->ast_compound_stmt.statement_block->ast_node_list_head;
            while(curr_statement != NULL) {
                // Don't want to pass null node
                if(curr_statement->node != NULL) {
                    generate_quads(curr_statement->node);
                }
                curr_statement = curr_statement->next;
            }
            return;
    }

    fprintf(stderr, "No quads to generate.\n");
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

// Gets r value of expression
struct astnode *get_rvalue(struct astnode *node, struct astnode *target) {
    
    // Checks type of node
    switch(node->node_type) {
        // Variables
        case SYM_ENTRY_TYPE:
            // Scalar Variable
            if(node->ast_sym_entry.sym_type == VAR_TYPE 
            && node->ast_sym_entry.sym_node->node_type == SCALAR_TYPE) {
                return node; // Should I return the scalar node or sym entry node?
            }

            // Array Varible
            if(node->ast_sym_entry.sym_type == VAR_TYPE 
            && node->ast_sym_entry.sym_node->node_type == ARRAY_TYPE) {
                astnode *temp = create_temp_node();
                emit_quad(LEA_OC, NULL, node, NULL, temp); // Should src1 be the array node?
                return temp;
            }
            break; // Will it ever get here?

        // Constants
        case NUMBER_TYPE:
        case CHARLIT_TYPE:
        case STRING_TYPE:
        case IDENT_TYPE:
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
                // Checks if pointer arithmetic is needed
                if(op_code == ADD_OC || op_code == SUB_OC) {

                }

                // Checks if target
                if(target == NULL) {
                    target = create_temp_node();
                }
                emit_quad(op_code, NULL, left, right, target);
                return target;
            }

            // Else, comparative expression & assignment

            return NULL;

        // Unary Operations
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
            // Address of operator
            if(node->ast_unary_op.op == '&') {
                
            }
            // Increment & Decrement
            if(node->ast_unary_op.op == PLUSPLUS || node->ast_unary_op.op == MINUSMINUS) {

            }
            // Sizeof operator
            if(node->ast_unary_op.op == SIZEOF) {
                return get_size_of(node->ast_unary_op.expr);
            }
            // Other Unary Operators
            int op_code = -1;
            switch(node->ast_unary_op.op) {
                case '+':
                case '-':
                case '!':
                case '~':
            }

            // Checks if target
            if(target == NULL) {
                target = create_temp_node();
            }

            // Checks if expression was found
            if(op > 0) {
                emit_quad(op_code, NULL, get_rvalue(node->ast_unary_op.expr, NULL), NULL, target);
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
                && node->ast_sym_entry.sym_node->node_type == SCALAR_TYPE) {
                *mode = DIRECT_MODE;
                return node; // Should this return the scalar node instead of the sym entry?
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

// Creates a temporary node
struct astnode *create_temp_node() {

}

// Determines size of value
// No variable lengths, so will return constant value
// Returns astnode with constant value
astnode *get_size_of(astnode *node) {
    num_type size_value;
    size_value.is_signed = SIGNED_TYPE;
    size_value.size_specifier = INT_TYPE;

    enum type_sizes {
        CHAR_SIZE = 1,
        SHORT_SIZE = 2,
        INT_SIZE = 4,
        FLOAT_SIZE = 4,
        DOUBLE_SIZE = 8,
        LONG_SIZE = 8,
        LONGLONG_SIZE = 8,
        POINTER_SIZE = 8,
        LONGDOUBLE_SIZE = 16
    };

    // If variable, get type (WON"T ALWAYS WORK)
    while(node->ast_type_name == SYM_ENTRY_TYPE) {
        node = node->ast_sym_entry.sym_node;
    }

    // Checks type
    switch(node->node_type) {
        case NUMBER_TYPE:
            // Gets size
            switch(node->ast_number.number.size_specifier) {
                case INT_TYPE:
                    size_value.i_value = INT_SIZE;
                    break;

                case FLOAT_TYPE:
                    size_value.i_value = FLOAT_SIZE;
                    break;

                case DOUBLE_TYPE:
                    size_value.i_value = DOUBLE_SIZE;
                    break;

                case LONG_TYPE:
                    size_value.i_value = LONG_SIZE;
                    break;

                case LONGLONG_TYPE:
                    size_value.i_value = LONGLONG_SIZE;
                    break;

                case LONGDOUBLE_TYPE:
                    size_value.i_value = LONGDOUBLE_SIZE;
                    break;
                
                default:
                    fprintf(stderr, "ERROR: Size of NUMBER_TYPE unknown.\n");
            }
            break;

        case SCALAR_TYPE:
            // Gets size
            switch(node->ast_scalar.scalar_type) {
                case VOID_ST:
                    size_value.i_value = POINTER_SIZE; // Is this right?
                    break;

                case CHAR_ST:
                    size_value.i_value = CHAR_SIZE;
                    break;

                case SHORT_ST:
                    size_value.i_value = SHORT_SIZE;
                    break;

                case INT_ST:
                    size_value.i_value = INT_SIZE;
                    break;

                case LONG_ST:
                    size_value.i_value = LONG_SIZE;
                    break;

                case LONG_LONG_ST:
                    size_value.i_value = LONGLONG_SIZE;
                    break;

                case FLOAT_ST:
                    size_value.i_value = FLOAT_SIZE;
                    break;

                case DOUBLE_ST:
                    size_value.i_value = DOUBLE_SIZE;
                    break;

                case LONG_DOUBLE_ST:
                    size_value.i_value = LONGDOUBLE_SIZE;
                    break;

                case BOOL_ST:
                    size_value.i_value = CHAR_SIZE;
                    break;

                default:
                    fprintf(stderr, "ERROR: Size of SCALAR_TYPE unknown.\n");
            }
            break;

        case POINTER_TYPE:
            size_value.i_value = POINTER_SIZE;
            break;

        case ARRAY_TYPE:
            size_value.i_value = node->ast_array.arr_size * get_size_of(node->ast_array.arr_type)->ast_number.number.i_value;
            break;

        default:
            fprintf(stderr, "ERROR: Cannot determine size.\n");
            return NULL; // Should this kill the program?
    }

    // Returns new constant astnode
    return create_number_node(size_value);
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
    // Checks if logical and / or
    if(node->ast_if_else.if_condition->node_type == BINARY_TYPE) {
        // Splits into separate if statements
        if(node->ast_if_else.if_condition->ast_binary_op.op == LOGAND) {
            astnode *expr2 = create_if_else_node(node->ast_if_else.if_condition->ast_binary_op.right_expr, node->ast_if_else.if_body, node->ast_if_else.else_body);
            astnode *expr1 = create_if_else_node(node->ast_if_else.if_condition->ast_binary_op.left_expr, expr2, node->ast_if_else.else_body);
            gen_if_stmt_IR(expr1);
            return;
        }
        if(node->ast_if_else.if_condition->ast_binary_op.op == LOGOR) {
            astnode *expr2 = create_if_else_node(node->ast_if_else.if_condition->ast_binary_op.right_expr, node->ast_if_else.if_body, node->ast_if_else.else_body);
            astnode *expr1 = create_if_else_node(node->ast_if_else.if_condition->ast_binary_op.left_expr, node->ast_if_else.if_body, expr2);
            gen_if_stmt_IR(expr1);
            return;
        }
    }

    // Creates basic blocks for branches
    basic_block *true_block = create_basic_block(NULL);
    basic_block *false_block = create_basic_block(NULL);

    // Checks if else statement
    astnode *next_block;
    if(node->ast_if_else.else_body) {
        next_block = create_basic_block(NULL);
    } else {
        next_block = false_block;
    }

    // Creates quads for conditional expression
    gen_conditional_expr_IR(node->ast_if_else.if_condition, true_block, false_block);

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
void gen_conditional_expr_IR(astnode *cond_expr, basic_block *true_branch, basic_block *false_branch) {
    // Checks if comparative expression
    if(cond_expr->node_type == BINARY_TYPE) {
        int op_code = -1;
        switch(cond_expr->ast_binary_op.op) {
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
    num.is_signed = SIGNED_TYPE;
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

// Generates IR for while loop
void gen_while_loop_IR(astnode *node) {
    // Creates blocks
    basic_block *condition_block = create_basic_block(NULL);
    basic_block *loop_block = create_basic_block(NULL);
    basic_block *next_block = create_basic_block(NULL);

    // Links current block to condition block
    link_blocks(NULL, condition_block, NONE_OC);
    set_block(condition_block);

    // Stores previous values for break and continue blocks
    basic_block *prev_break_block = break_block ? break_block : NULL;
    basic_block *prev_continue_block = continue_block ? continue_block : NULL;
    break_block = next_block;
    continue_block = condition_block;

    gen_conditional_expr_IR(node->ast_while_loop.condition, loop_block, next_block);

    // Generates quads for loop
    set_block(loop_block);
    generate_quads(node->ast_while_loop.body);

    // Resets break and continue blocks to previous values
    break_block = prev_break_block ? prev_break_block : NULL;
    continue_block = prev_continue_block ? prev_continue_block : NULL;

    set_block(next_block);
}

// Generates IR for do while loop
void gen_do_while_loop_IR(astnode *node) {
    // Creates blocks
    basic_block *condition_block = create_basic_block(NULL);
    basic_block *loop_block = create_basic_block(NULL);
    basic_block *next_block = create_basic_block(NULL);

    // Links current block -> loop block -> condition block
    link_blocks(NULL, loop_block, NONE_OC);
    set_block(loop_block);
    link_blocks(NULL, condition_block, NONE_OC);
    
    // Stores previous values for break and continue blocks
    basic_block *prev_break_block = break_block ? break_block : NULL;
    basic_block *prev_continue_block = continue_block ? continue_block : NULL;
    break_block = next_block;
    continue_block = condition_block;

    generate_quads(node->ast_while_loop.body);

    set_block(condition_block);
    gen_conditional_expr_IR(node->ast_while_loop.condition, loop_block, next_block);

    // Resets break and continue blocks to previous values
    break_block = prev_break_block ? prev_break_block : NULL;
    continue_block = prev_continue_block ? prev_continue_block : NULL;

    set_block(next_block);
}

// Generates IR for for loop
void gen_for_loop_IR(astnode *node) {
    // Creates blocks
    basic_block *condition_block = create_basic_block(NULL);
    basic_block *update_block = create_basic_block(NULL);
    basic_block *loop_block = create_basic_block(NULL);
    basic_block *next_block = create_basic_block(NULL);
    
    // Initialize variables
    generate_quads(node->ast_for_loop.initialization);

    // Links current block to condition block
    link_blocks(NULL, condition_block, NONE_OC);
    set_block(condition_block);

    // Stores previous values for break and continue blocks
    basic_block *prev_break_block = break_block ? break_block : NULL;
    basic_block *prev_continue_block = continue_block ? continue_block : NULL;
    break_block = next_block;
    continue_block = condition_block;

    gen_conditional_expr_IR(node->ast_for_loop.condition, loop_block, next_block);

    // Generates quads for loop
    set_block(loop_block);
    generate_quads(node->ast_for_loop.body);

    // Links loop block to update block
    link_blocks(NULL, update_block, NONE_OC);

    // Generates quads for update
    set_block(update_block);
    generate_quads(node->ast_for_loop.update);

    // Links update block to condition block
    link_blocks(NULL, condition_block, NONE_OC);

    // Resets break and continue blocks to previous values
    break_block = prev_break_block ? prev_break_block : NULL;
    continue_block = prev_continue_block ? prev_continue_block : NULL;

    set_block(next_block);
}

// Generates IR for break
void gen_break_stmt_IR() {
    // Checks if break block has been set
    if(break_block != NULL) {
        link_blocks(NULL, break_block, NONE_OC);
        // Do I make a new block?
    } else {
        fprintf(stderr, "ERROR: No break block set.\n");
    }
}

// Generates IR for continue
void gen_continue_stmt_IR() {
    // Checks if continue block has been set
    if(continue_block != NULL) {
        link_blocks(NULL, continue_block, NONE_OC);
        // Do I make a new block?
    } else {
        fprintf(stderr, "ERROR: No continue block set.\n");
    }
}

// Generates IR for return
void gen_return_stmt_IR(astnode *node) {
    emit_quad(RETURN_OC, NULL, get_rvalue(node->ast_return.return_expr,NULL), NULL, NULL);
}

// Generates IR for function call
void gen_fnc_call_IR(astnode *node) {
    // Emit quad for argument number
    emit_quad(ARGBEGIN_OC, NULL, node->ast_fnc_call.num_arguments, NULL, NULL);

    // Creates number astnode to pass to quad
    astnode *arg_number = create_num_one_node();
    // Emits quads for arguments
    astnode_list_entry *curr_arg = node->ast_fnc_call.expr_list_head->ast_node_list_head;
    for(int i = 1; i <= node->ast_fnc_call.num_arguments; i++) { // Should this be right to left?
        arg_number->ast_number.number.i_value = i;
        emit_quad(ARG, NULL, arg_number, curr_arg->node, NULL);
        curr_arg = curr_arg->next;
    }

    // Emits quad for function call
    emit_quad(CALL_OC, NULL, node->ast_fnc_call.function_name, NULL, NULL);
}
