/* Andrew Lorber */
/* Compilers - Parser */
/* Quads File */

#include "quads.h"
#include "parser.tab.h"

// Global Variables
// ----------------

struct basic_block_list *block_list;
struct basic_block *curr_block;
struct basic_block *break_block;
struct basic_block *continue_block;
struct quad_list_entry *curr_quad;

// Functions
// ----------

// Initializes global variables
void init_quad_gen() {
    //  Checks for errors
    if((block_list = malloc(sizeof(block_list))) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for basic block list.\n");
        exit(-1);
    }

    curr_block = NULL;
    break_block = NULL;
    continue_block = NULL;
    curr_quad = NULL;
}

// Creates basic block list entry
struct basic_block_list_entry *create_block_list_entry(basic_block *block) {
    basic_block_list_entry *block_list_entry;

    //  Checks for errors
    if((block_list_entry = malloc(sizeof(basic_block_list_entry))) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for basic block list entry.\n");
        exit(-1);
    }

    block_list_entry->bb = block;
    block_list_entry->next = NULL;

    return block_list_entry;
}

// Creates a new basic block
struct basic_block *create_basic_block(char *block_label) {
    static int num_blocks = 0; // Number of nameless blocks (used for naming)
    basic_block *b_block;
    
    //  Checks for errors
    if((b_block = malloc(sizeof(basic_block))) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for basic block.\n");
        exit(-1);
    }

    // Checks if name needed
    if(block_label == NULL) {
        char *new_name = calloc(256, sizeof(char));
        sprintf(new_name, "Basic_Block_%d", num_blocks++);
        b_block->block_label = new_name;
    } else {
        b_block->block_label = block_label;
    }    

    b_block->quad_list = NULL;
    b_block->next = NULL;
    b_block->branch = NULL;
    b_block->branch_condition = NONE_OC;
    b_block->was_printed = 0;

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
// The op_code will be a conditional op_code - used for assembly generation
// CMP_OC is set as the op_code to denote the end of the if else block chain 
//      and return to the "main" block chain.
// Makes assembly generation easier.
void link_blocks(basic_block *true_branch, basic_block *false_branch, int op_code) {
    curr_block->next = false_branch;
    curr_block->branch = true_branch;
    curr_block->branch_condition = op_code;
}

// Generates the quads for a function and stores blocks in linked list
void generate_function_quads(astnode *node) {
    // Makes sure that node is a function
    if(node->node_type != SYM_ENTRY_TYPE || node->ast_sym_entry.sym_type != FNC_NAME_TYPE) {
        fprintf(stderr, "ERROR: Can only print quads of function.\n");
        return; // Should this kill the program?
    }

    // Sets current basic block
    set_block(create_basic_block(node->ast_sym_entry.symbol));

    // Adds function block to block list
    basic_block_list_entry *block_entry = create_block_list_entry(curr_block);
    if(block_list->head == NULL) {
        block_list->head = block_entry;
    } else {
        block_list->tail->next = block_entry;
    }
    block_list->tail = block_entry;

    // Generates quads for functions
    generate_quads(node->ast_sym_entry.sym_node);
}

// Given AST node, recursively generates quads
void generate_quads(astnode *node) {

    // Checks node type
    switch(node->node_type) {
        // PLUSPLUS & MINUSMINUS
        case UNARY_TYPE:
            if(node->ast_unary_op.op == PLUSPLUS || node->ast_unary_op.op == MINUSMINUS) {
                gen_incr_decr_IR(node);
                return;
            }

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
                gen_do_while_loop_IR(node);
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
            gen_fnc_call_IR(node, NULL);
            return;

        // Compound statement
        case COMPOUND_STMT_TYPE:;
            // Loop through statements
            astnode_list_entry *curr_statement = &(node->ast_compound_stmt.statement_block->ast_node_list_head);
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
// Currently has field for size - Not sure if needed
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
    quad_entry->next = NULL;

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
    if(node == NULL) {
        return NULL;
    }
    
    // Checks type of node
    int op_code;
    switch(node->node_type) {
        // Temp Nodes
        case TEMP_TYPE:
            return node;

        // Variables
        case SYM_ENTRY_TYPE:
            // Array Variable
            if(node->ast_sym_entry.sym_type == VAR_TYPE 
            && node->ast_sym_entry.sym_node->node_type == ARRAY_TYPE) {
                // Checks if target
                if(target == NULL) {
                    target = get_temp_node();
                }

                emit_quad(LEA_OC, -1, node, NULL, target);
                return target;
            }
            // Other Variable
            if(node->ast_sym_entry.sym_type == VAR_TYPE) {
                return node;
            }
            break;

        // Constants
        case NUMBER_TYPE:
        case CHARLIT_TYPE:
        case STRING_TYPE:
        case IDENT_TYPE:
            return node;

        // Ternary Operations
        case TERNARY_TYPE:;
            // Creates blocks
            basic_block *true_block = create_basic_block(NULL);
            basic_block *false_block = create_basic_block(NULL);
            basic_block *next_block = create_basic_block(NULL);

            // Links blocks to current block
            gen_conditional_expr_IR(node->ast_ternary_op.if_expr, true_block, false_block);
            
            // Checks if target
            if(target == NULL) {
                target = get_temp_node();
            }

            // Creates quads for true branch
            set_block(true_block);
            emit_quad(MOV_OC, -1, get_rvalue(node->ast_ternary_op.then_expr, NULL), NULL, target);
            link_blocks(NULL, next_block, NONE_OC);

            // Creates quads for false brach
            set_block(false_block);
            emit_quad(MOV_OC, -1, get_rvalue(node->ast_ternary_op.else_expr, NULL), NULL, target);
            link_blocks(NULL, next_block, NONE_OC);

            set_block(next_block);

            return target;

        // Binary Operations
        case BINARY_TYPE:;
            astnode *left = get_rvalue(node->ast_binary_op.left_expr, NULL);
            astnode *right = get_rvalue(node->ast_binary_op.right_expr, NULL);

            // Gets op code
            op_code = -1;
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

            // Checks if binary expression
            if(op_code > 0) {
                // Checks if pointer arithmetic is needed
                if(op_code == ADD_OC || op_code == SUB_OC) {
                    int left_type, right_type; // Stores type of operands (Uses enum for ast types)

                    // Should an arithmetic operator node be accepted as a number value?

                    // Checks type of left operand
                    if((node->ast_binary_op.left_expr->node_type == SYM_ENTRY_TYPE) && 
                    (node->ast_binary_op.left_expr->ast_sym_entry.sym_node->node_type == POINTER_TYPE ||
                    node->ast_binary_op.left_expr->ast_sym_entry.sym_node->node_type == ARRAY_TYPE)) {
                        left_type = POINTER_TYPE;
                    }
                    else if(node->ast_binary_op.left_expr->node_type == NUMBER_TYPE ||
                    (node->ast_binary_op.left_expr->node_type == SYM_ENTRY_TYPE &&
                    node->ast_binary_op.left_expr->ast_sym_entry.sym_node->node_type == SCALAR_TYPE)) {
                        left_type = NUMBER_TYPE;
                    }
                    else {
                        fprintf(stderr, "ERROR: Unknown type in arithmetic operation.\n");
                    }

                    // Checks type of right operand
                    if(node->ast_binary_op.right_expr->node_type == SYM_ENTRY_TYPE && 
                    (node->ast_binary_op.right_expr->ast_sym_entry.sym_node->node_type == POINTER_TYPE ||
                    node->ast_binary_op.right_expr->ast_sym_entry.sym_node->node_type == ARRAY_TYPE)) {
                        right_type = POINTER_TYPE;
                    }
                    else if(node->ast_binary_op.right_expr->node_type == NUMBER_TYPE ||
                    (node->ast_binary_op.right_expr->node_type == SYM_ENTRY_TYPE &&
                    node->ast_binary_op.right_expr->ast_sym_entry.sym_node->node_type == SCALAR_TYPE)) {
                        right_type = NUMBER_TYPE;
                    }
                    else {
                        fprintf(stderr, "ERROR: Unknown type in arithmetic operation.\n");
                    }

                    // 3 pairs to check:
                    // (1) pointer / Array +- number
                    if(left_type == POINTER_TYPE && right_type == NUMBER_TYPE) {
                        // Determines size of pointee
                        astnode *pointee_size;
                        if(node->ast_binary_op.left_expr->ast_sym_entry.sym_node->node_type == POINTER_TYPE) {
                            pointee_size = get_size_of(node->ast_binary_op.left_expr->ast_sym_entry.sym_node->ast_pointer.pointer_type);
                        } else {
                            pointee_size = get_size_of(node->ast_binary_op.left_expr->ast_sym_entry.sym_node->ast_array.arr_type);
                        }

                        // Evaluates amount to add
                        astnode *temp_node = get_temp_node();
                        emit_quad(MUL_OC, -1, right, pointee_size, temp_node);
                        right = temp_node;
                    }
                    // (2) number +- pointer / Array
                    else if(left_type == NUMBER_TYPE && right_type == POINTER_TYPE) {
                        // Determines size of pointee
                        astnode *pointee_size;
                        if(node->ast_binary_op.right_expr->ast_sym_entry.sym_node->node_type == POINTER_TYPE) {
                            pointee_size = get_size_of(node->ast_binary_op.right_expr->ast_sym_entry.sym_node->ast_pointer.pointer_type);
                        } else {
                            pointee_size = get_size_of(node->ast_binary_op.right_expr->ast_sym_entry.sym_node->ast_array.arr_type);
                        }

                        // Evaluates amount to add
                        astnode *temp_node = get_temp_node();
                        emit_quad(MUL_OC, -1, left, pointee_size, temp_node);
                        left = temp_node;
                    }
                    // (3) pointer / Array +- pointer / Array 
                    else if(left_type == POINTER_TYPE && right_type ==  POINTER_TYPE) {
                        if(op_code == ADD_OC) {
                            fprintf(stderr, "ERROR: Addition operation not allowed on two pointers.\n");
                            return NULL; // Should this kill the program? 
                        }

                        // How do I check they point to the same type? Can I just compare their sizes?
                        astnode *pointer_size = compare_pointers(node->ast_binary_op.left_expr, node->ast_binary_op.right_expr);

                        // If not equal pointers, error
                        if(pointer_size == NULL) {
                            fprintf(stderr, "ERROR: Attempting to subtract incompatible pointer types.\n");
                            return NULL; // Should this kill the program?
                        }

                        // Two step process
                        // 1) Subtraction
                        astnode *temp_node = get_temp_node();
                        emit_quad(SUB_OC, -1, left, right, temp_node);
                        
                        // 2) Division
                        op_code = DIV_OC;
                        left = temp_node;
                        right = pointer_size;
                    }
                }

                // Checks if target
                if(target == NULL) {
                    target = get_temp_node();
                }
                emit_quad(op_code, -1, left, right, target);
                return target;
            }

            // Checks comparison operators
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

            // Checks if comparison expression
            if(op_code > 0) {
                // Creates blocks
                basic_block *true_block = create_basic_block(NULL);
                basic_block *false_block = create_basic_block(NULL);
                basic_block *next_block = create_basic_block(NULL);

                // Links blocks to current block
                gen_conditional_expr_IR(node, true_block, false_block);

                // Checks if target
                if(target == NULL) {
                    target = get_temp_node();
                }

                // Creates quads for true branch (returns 1)
                set_block(true_block);
                astnode *num_one = create_num_one_node();
                emit_quad(MOV_OC, -1, num_one, NULL, target);
                link_blocks(NULL, next_block, NONE_OC);

                // Creates quads for false brach (returns 0)
                set_block(false_block);
                astnode *num_zero = create_num_one_node();
                num_zero->ast_number.number.i_value = 0;
                emit_quad(MOV_OC, -1, num_zero, NULL, target);
                link_blocks(NULL, next_block, NONE_OC);

                set_block(next_block);

                return target;
            }

            // Do I check for Assignment expressions?
            return NULL;

        // Unary Operations
        case UNARY_TYPE:
            // Pointer Deref
            if(node->ast_unary_op.op == '*') {
                astnode *addr = get_rvalue(node->ast_unary_op.expr,NULL);

                // Checks if target
                if(target == NULL) {
                    target = get_temp_node();
                }

                emit_quad(LOAD_OC, -1, addr, NULL, target);
                return target;
            }
            // Address of operator
            if(node->ast_unary_op.op == '&') {
                // Checks expr type
                switch(node->ast_unary_op.expr->node_type) {
                    // Sym table entry
                    case SYM_ENTRY_TYPE:
                        // Checks if target
                        if(target == NULL) {
                            target = get_temp_node();
                        }

                        emit_quad(LEA_OC, -1, node->ast_unary_op.expr, NULL, target);
                        break;

                    // Pointer dereference
                    case UNARY_TYPE:
                        if(node->ast_unary_op.op == '*') {
                            // Checks if target
                            if(target == NULL) {
                                target = get_temp_node();
                            }

                            // Operators cancel each other out
                            get_rvalue(node->ast_unary_op.expr->ast_unary_op.expr, target);
                            break;
                        }
                        // No break on purpose

                    default:;
                        astnode *expr_rvalue = get_rvalue(node->ast_unary_op.expr, NULL);

                        // Checks if target
                        if(target == NULL) {
                            target = get_temp_node();
                        }

                        emit_quad(LEA_OC, -1, expr_rvalue, NULL, target);
                }
                return target;
            }
            // Increment & Decrement
            if(node->ast_unary_op.op == PLUSPLUS || node->ast_unary_op.op == MINUSMINUS) {
                // Gets rvalue of expr
                astnode *expr_rvalue = get_rvalue(node->ast_unary_op.expr, NULL);

                // Checks if target
                if(target == NULL) {
                    target = get_temp_node();
                }

                // Moves expr to target
                emit_quad(MOV_OC, -1, expr_rvalue, NULL, target);

                // Updates variable with incr / decr
                gen_incr_decr_IR(node);

                return target;
            }
            // Sizeof operator
            if(node->ast_unary_op.op == SIZEOF) {
                // Checks if target
                if(target == NULL) {
                    target = get_temp_node();
                }

                // Moves size to target
                astnode *size = get_size_of(node->ast_unary_op.expr);
                emit_quad(MOV_OC, -1, size, NULL, target);
                return target;
            }
            // Other Unary Operators
            op_code = -1;
            switch(node->ast_unary_op.op) {
                case '+':
                    // Not sure what to do here
                   
                   // Checks if target
                    if(target == NULL) {
                        target = get_temp_node();
                    }

                    get_rvalue(node->ast_unary_op.expr, target);
                    return target;

                case '-':
                    // Should this multiply by (-1)?
                    op_code = NEG_OC;
                    break;

                case '!':
                    op_code = NOT_OC;
                    break;

                case '~':
                    op_code = COMPL_OC;
                    break;
            }

            // Checks if target
            if(target == NULL) {
                target = get_temp_node();
            }

            // Checks if expression was found
            if(op_code > 0) {
                emit_quad(op_code, -1, get_rvalue(node->ast_unary_op.expr, NULL), NULL, target);
                return target;
            }
            break;
        
        case FUNCTION_CALL_TYPE:
            // Checks if target
            if(target == NULL) {
                target = get_temp_node();
            }

            gen_fnc_call_IR(node, target);
            return target;
    }

    return NULL;
}

// Gets l value of expression
struct astnode *get_lvalue(struct astnode *node, int *mode) {
    switch(node->node_type) {
        // Scalar Variable
        case SYM_ENTRY_TYPE:
            // Scalar type
            if(node->ast_sym_entry.sym_type == VAR_TYPE
                && node->ast_sym_entry.sym_node->node_type == SCALAR_TYPE) {
                *mode = DIRECT_MODE;
                return node;
            }
            // Pointer type
            if(node->ast_sym_entry.sym_type == VAR_TYPE && 
                node->ast_sym_entry.sym_node->node_type == POINTER_TYPE) {
                *mode = INDIRECT_MODE;
                return node;
                }
            break;

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
            break;
    }

    return NULL;
}

// Creates a temporary node
struct astnode *get_temp_node() {
    static int num_temps = 0; // Number of temp nodes created (Used for naming)

    return create_temp_node(num_temps++);
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
        LONG_SIZE = 4,
        LONGLONG_SIZE = 8,
        POINTER_SIZE = 4,
        LONGDOUBLE_SIZE = 12
    };

    // If variable, get type
    while(node->node_type == SYM_ENTRY_TYPE) {
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

// Compares two pointers and confirms that they are of equal type
// Does this by comparing size (I think that works)
// Will either return NULL (not equal) or the size of the pointers (needed for ptr - ptr)
astnode *compare_pointers(astnode *left_pointer, astnode *right_pointer) {
    if(left_pointer->node_type != right_pointer->node_type) {
        return NULL;
    }

    // If pointer, moves in a level
    if(left_pointer->node_type == POINTER_TYPE) {
        return compare_pointers(left_pointer->ast_pointer.pointer_type, right_pointer->ast_pointer.pointer_type);
    }
    // If array, moves in a level
    if(left_pointer->node_type == ARRAY_TYPE) {
        return compare_pointers(left_pointer->ast_array.arr_type, right_pointer->ast_array.arr_type);
    }
    // If constant, compares sizes
    if((left_pointer->node_type == SYM_ENTRY_TYPE && left_pointer->ast_sym_entry.sym_node->node_type == SCALAR_TYPE) 
    || left_pointer->node_type == NUMBER_TYPE) {
        astnode *pointer_size = get_size_of(left_pointer);
        
        // Compares sizes
        if(pointer_size == get_size_of(right_pointer)) {
            return pointer_size;
        } else {
            return NULL;
        }
    }
}

// Generates IR for ++ & --
void gen_incr_decr_IR(astnode *node) {
    // Turns a++ -> a = a + 1 & a-- -> a = a - 1
    astnode *num_one = create_num_one_node();

    // Creates arithmetic operator node
    astnode *new_binary_node;
    if(node->ast_unary_op.op == PLUSPLUS) {
        new_binary_node = create_binary_node('+', node->ast_unary_op.expr, num_one);
    } else {
        new_binary_node = create_binary_node('-', node->ast_unary_op.expr, num_one);
    }

    // Creates assignment node & generates IR
    gen_assignment_IR(create_binary_node('=', node->ast_unary_op.expr, new_binary_node));
}

// Generates IR for assignments
void gen_assignment_IR(astnode *node) {
    int mode;
    astnode *dest = get_lvalue(node->ast_binary_op.left_expr, &mode);

    if(mode == DIRECT_MODE) {
        astnode *rvalue = get_rvalue(node->ast_binary_op.right_expr, NULL);
        emit_quad(MOV_OC, -1, rvalue, NULL, dest);
    } else {
        astnode *rvalue = get_rvalue(node->ast_binary_op.right_expr,NULL);
        emit_quad(STORE_OC, -1, rvalue, dest, NULL);
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
    basic_block *next_block;
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
    // CMP_OC is set as the op_code to denote the end of the if else blocks 
    //      and return to the "main" block chain.
    //      Makes it easier to order blocks correctly.
    link_blocks(NULL,next_block,CMP_OC);

    // Creates quads for false branch
    if(node->ast_if_else.else_body) {
        set_block(false_block);
        generate_quads(node->ast_if_else.else_body);
        
        // Attaches false block to next block
        link_blocks(NULL,next_block,CMP_OC);
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
            astnode *left_expr = get_rvalue(cond_expr->ast_binary_op.left_expr, NULL);
            astnode *right_expr = get_rvalue(cond_expr->ast_binary_op.right_expr, NULL);

            // Compare quad
            emit_quad(CMP_OC, -1, left_expr, right_expr, NULL);

            // Attaches true and false branches to current block
            link_blocks(true_branch, false_branch, op_code);

            return;
        }
    }
    // If single expression, compares to 0
    num_type num;
    num.i_value = 0;
    num.is_signed = SIGNED_TYPE;
    num.size_specifier = INT_TYPE;
    astnode *num_zero = create_number_node(num);

    // Checks if constant variable
    if(cond_expr->node_type == SYM_ENTRY_TYPE && cond_expr->ast_sym_entry.sym_type == VAR_TYPE
    && cond_expr->ast_sym_entry.sym_node->node_type == SCALAR_TYPE) {
        // Compares with zero
        emit_quad(CMP_OC, -1, cond_expr, num_zero, NULL);
    } else {
        // Otherwise, attempts to determine value
        emit_quad(CMP_OC, -1, get_rvalue(cond_expr,NULL), num_zero, NULL);
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
    link_blocks(NULL, condition_block, NONE_OC);

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
    emit_quad(RETURN_OC, -1, get_rvalue(node->ast_return.return_expr,NULL), NULL, NULL);
    link_blocks(NULL, NULL, RETURN_OC);
}

// Generates IR for function call
void gen_fnc_call_IR(astnode *node, astnode *target) {
    // Creates number node for argument number
    num_type arg_num;
    arg_num.is_signed = SIGNED;
    arg_num.size_specifier = INT_TYPE;
    arg_num.i_value = node->ast_fnc_call.num_arguments;
    astnode *arg_num_node = create_number_node(arg_num);

    // Emits quad for argument number
    emit_quad(ARGBEGIN_OC, -1, arg_num_node, NULL, NULL);

    // Emits quads for arguments
    astnode_list_entry *curr_arg;
    for(int i = node->ast_fnc_call.num_arguments; i > 0; i--) {
        // Reverses order of arguments
        // Not optimal, but works
        curr_arg = &(node->ast_fnc_call.expr_list_head->ast_node_list_head);
        for(int j = i; j > 1; j--) {
            curr_arg = curr_arg->next;
        }
        emit_quad(ARG_OC, -1, curr_arg->node, NULL, NULL);
    }

    // Emits quad for function call
    emit_quad(CALL_OC, -1, node, arg_num_node, target);
}

// Printing Functions
// ------------------

// Prints the quads of the most recent function
// Just so no chance of messing things up from parser
void print_function_quads() {
    fprintf(stdout, "\n");
    print_block(block_list->tail->bb);
    fprintf(stdout, "\n\n");
}

// Prints basic block and follows chain
void print_block(basic_block *block) {
    // Prints name of block
    fprintf(stdout, "\nBlock: %s\n", block->block_label);

    // Checks if previously printed
    if(block->was_printed == 1) {
        fprintf(stdout,"\tAlready printed\n");
        return;
    }

    // Loops through quads, printing each
    quad_list_entry *curr_quad_entry = block->quad_list;
    while(curr_quad_entry != NULL) {
        print_quad(curr_quad_entry->quad);
        curr_quad_entry = curr_quad_entry->next;
    }

    // Sets as printed
    block->was_printed = 1;

    // Checks for branch
    if(block->branch) {
        // Prints comparison opcode
        fprintf(stdout, "\n%s True", op_code_to_string(block->branch_condition));
        
        // Prints true branch
        print_block(block->branch);
    }
    // Prints false / default branch
    if(block->next) {
        if(block->branch) {
            fprintf(stdout, "\n%s False", op_code_to_string(block->branch_condition));
        } else {
            fprintf(stdout, "\nDefault Next");
        }

        print_block(block->next);
    }
}

// Prints quad
void print_quad(quad *q) {
    // Indents
    fprintf(stdout, "\t");

    // Prints target
    if(q->dest != NULL) {
        fprintf(stdout, "%s = ", node_name_to_string(q->dest));
    }

    // Prints op_code
    fprintf(stdout, "%s  ", op_code_to_string(q->op_code));

    // Prints sources
    if(q->src1 != NULL) {
        fprintf(stdout, "%s", node_name_to_string(q->src1));
    }
    if(q->src1 != NULL && q->src2 != NULL) {
        fprintf(stdout, ", ");
    }
    if(q->src2 != NULL) {
        fprintf(stdout, "%s", node_name_to_string(q->src2));
    }

    fprintf(stdout, "\n");
}

// Converts op_code to string
char *op_code_to_string(int op_code) {
    switch(op_code) {
        case NONE_OC:
            return "";

        // Addressing & Assigning
        case LOAD_OC:
            return "LOAD";
        case STORE_OC:
            return "STORE";
        case LEA_OC:
            return "LEA";
        case MOV_OC:
            return "MOV";

        // Arithmetic Operations
        case ADD_OC:
            return "ADD";
        case SUB_OC:
            return "SUB";
        case MUL_OC:
            return "MUL";
        case DIV_OC:
            return "DIV";
        case MOD_OC:
            return "MOD";

        // Bitwise Operators
        case AND_OC:
            return "AND";
        case OR_OC:
            return "OR";
        case XOR_OC:
            return "XOR";
        case SHL_OC:
            return "SHL";
        case SHR_OC:
            return "SHR";

        // Comparison Operators
        case EQEQ_OC:
            return "EQEQ";
        case NEQ_OC:
            return "NEQ";
        case LT_OC:
            return "LT";
        case GT_OC:
            return "GT";
        case LTEQ_OC:
            return "LTEQ";
        case GTEQ_OC:
            return "GTEQ";
        case CMP_OC:
            return "CMP";

        // Function Operators
        case RETURN_OC:
            return "RETURN";
        case ARGBEGIN_OC:
            return "ARGBEGIN";
        case ARG_OC:
            return "ARG";
        case CALL_OC:
            return "CALL";

        default:
            fprintf(stderr, "ERROR: Cannot determine op_code to print,\n");
            return "";
    }
}

// Prints name of node
char *node_name_to_string(astnode *node) {
    char *node_name = calloc(256, sizeof(char));

    switch(node->node_type) {
        case TEMP_TYPE:
            strcpy(node_name, node->ast_temp_node.name);
            break;
        
        case SYM_ENTRY_TYPE:
            strcpy(node_name, node->ast_sym_entry.symbol);
            break;

        case NUMBER_TYPE:
            // Checks if signed
            if(node->ast_number.number.is_signed == SIGNED_TYPE) {
                switch(node->ast_number.number.size_specifier) {
                    case INT_TYPE:
                        sprintf(node_name, "%d", (int) node->ast_number.number.i_value);
                        break;

                    case FLOAT_TYPE:
                        sprintf(node_name, "%f", (float) node->ast_number.number.d_value);
                        break;

                    case DOUBLE_TYPE:
                        sprintf(node_name, "%f", (double) node->ast_number.number.d_value);
                        break;

                    case LONG_TYPE:
                        sprintf(node_name, "%ld", (long) node->ast_number.number.i_value);
                        break;

                    case LONGLONG_TYPE:
                        sprintf(node_name, "%lld", (long long) node->ast_number.number.i_value);
                        break;

                    case LONGDOUBLE_TYPE:
                        sprintf(node_name, "%Lf",  (long double) node->ast_number.number.d_value);
                    
                }
            } else {
                switch(node->ast_number.number.size_specifier) {
                    case INT_TYPE:
                        sprintf(node_name, "%u", (unsigned int) node->ast_number.number.i_value);
                        break;

                    case LONG_TYPE:
                        sprintf(node_name, "%lu", (unsigned long) node->ast_number.number.i_value);
                        break;

                    case LONGLONG_TYPE:
                        sprintf(node_name, "%llu", (unsigned long long) node->ast_number.number.i_value);
                        break;
                }
            }
            break;

        case CHARLIT_TYPE:
            strcpy(node_name, node->ast_charlit.charlit);
            break;

        case STRING_TYPE:
            strcpy(node_name, node->ast_string.string);
            break;

        case IDENT_TYPE:
            strcpy(node_name, node->ast_ident.ident);
            break;
    }

    return node_name;
}
