/* Andrew Lorber */
/* Compilers - Parser */
/* Quads Header File */

#ifndef QUADS_H
#define QUADS_H

#include "astFunctions.h"

// Structs
// -------

// Basic Block
typedef struct basic_block {
    char *block_label;                    // Unique label
    struct quad_list_entry *quad_list;    // Linked list of quads
    struct basic_block *next, *branch;    // Branch = branch on true condition
    int branch_condition;                 // Type of comparator (from op_codes enum below)
    int was_printed;                      // Used when printing blocks, so no endless loops
    int was_translated;                   // Used when writing assembly, so no endless loops
} basic_block;

// Entry in linked list of basic blocks
// Each list entry contains the basic blocks of a function
typedef struct basic_block_list_entry {
    struct basic_block *bb;
    struct basic_block_list_entry *next;
} basic_block_list_entry;

typedef struct basic_block_list {
    struct basic_block_list_entry *head;
    struct basic_block_list_entry *tail;
} basic_block_list;

// Entry in linked list of quads (used in basic block)
typedef struct quad_list_entry {
    struct quad *quad;
    struct quad_list_entry *next;
} quad_list_entry;

enum op_codes {
    NONE_OC = 0,
    // Addressing & Assigning
    LOAD_OC,
    STORE_OC,
    LEA_OC,
    MOV_OC,
    // Arithmetic Operations
    ADD_OC,
    SUB_OC,
    MUL_OC,
    DIV_OC,
    MOD_OC,
    // Bitwise Operators
    AND_OC,
    OR_OC,
    XOR_OC,
    SHL_OC,
    SHR_OC,
    // Comparison Operators
    EQEQ_OC,
    NEQ_OC,
    LT_OC,
    GT_OC,
    LTEQ_OC,
    GTEQ_OC,
    CMP_OC,
    // Unary Operators
    NOT_OC,
    NEG_OC,
    COMPL_OC,
    // Function Operators
    RETURN_OC,
    ARGBEGIN_OC,
    ARG_OC,
    CALL_OC
};

enum op_sizes {
    INT_OS = 1,
    LONG_OS,
    NONE
};

typedef struct quad {
    int op_code;    // Op code from op_codes enum above
    int op_size;    // Type of operands for arithmetic
    struct astnode *dest, *src1, *src2;
} quad;

enum lvalue_mode {
    DIRECT_MODE = 1,
    INDIRECT_MODE
};

// Functions
// ---------

// Initializes global variables
void init_quad_gen();

// Creates basic block list entry
struct basic_block_list_entry *create_block_list_entry(basic_block *block);

// Creates a new basic block
struct basic_block *create_basic_block(char *block_label);

// Sets block as current block and updates global variable
void set_block(basic_block *block);

// Links given block(s) with current basic block
// Branch is set to the true branch, while next is set to the false branch
// The op_code will be a conditional op_code - used for assembly generation
// CMP_OC is set as the op_code to denote the end of the if else block chain 
//      and return to the "main" block chain.
// Makes assembly generation easier.
void link_blocks(basic_block *true_branch, basic_block *false_branch, int op_code);

// Generates the quads for a function and stores blocks in linked list
void generate_function_quads(astnode *node);

// Given AST node, recursively generates quads
void generate_quads(struct astnode *node);

// Creates a new quad and appends it to linked list of quads
void *emit_quad(int op_code, int op_size, struct astnode *src1, struct astnode *src2, struct astnode *dest);

// Gets r value of expression
struct astnode *get_rvalue(struct astnode *node, struct astnode *target);

// Gets l value of expression
struct astnode *get_lvalue(struct astnode *node, int *mode);

// Creates a temporary node
struct astnode *get_temp_node();

// Determines size of value
// No variable lengths, so will return constant value
// Returns astnode with constant values
astnode *get_size_of(astnode *node);

// Compares two pointers and confirms that they are of equal type
// Does this by comparing size (I think that works)
// Will either return NULL (not equal) or the size of the pointers (needed for ptr - ptr)
astnode *compare_pointers(astnode *left_pointer, astnode *right_pointer);

// Generates IR for ++ & --
void gen_incr_decr_IR(astnode *node);

// Generates IR for assignments
void gen_assignment_IR(astnode *node);

// Generates IR for if statements
void gen_if_stmt_IR(astnode *node);

// Generates IR for conditional expression
void gen_conditional_expr_IR(astnode *cond_expr, basic_block *true_branch, basic_block *false_branch);

// Generates IR for while loop
void gen_while_loop_IR(astnode *node);

// Generates IR for do while loop
void gen_do_while_loop_IR(astnode *node);

// Generates IR for for loop
void gen_for_loop_IR(astnode *node);

// Generates IR for break
void gen_break_stmt_IR();

// Generates IR for continue
void gen_continue_stmt_IR();

// Generates IR for return
void gen_return_stmt_IR(astnode *node);

// Generates IR for function call
void gen_fnc_call_IR(astnode *node, astnode *target);

// Printing Functions
// ------------------

// Prints the quads of the most recent function
void print_function_quads();

// Prints basic block
void print_block(basic_block *block);

// Prints quad
void print_quad(quad *q);

// Converts op_code to string
char *op_code_to_string(int op_code);

// Prints name of node
char *node_name_to_string(astnode *node);

#endif // QUADS_H
