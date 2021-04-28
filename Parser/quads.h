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
    char *block_label;                         // Unique label
    struct quad_list_entry *quad_list;  // Linked list of quads
    struct basic_block *next, *branch;         // Branch = branch on true condition
    int branch_condition;                      // Type of comparator (from op_codes enum below)
} basic_block;

// Entry in linked list of basic blocks
// Each list entry contains the basic blocks of a function
typedef struct basic_block_list_entry {
    basic_block *bb;
    basic_block_list_entry *next;
} basic_block_list_entry;

// Entry in linked list of quads (used in basic block)
typedef struct quad_list_entry {
    struct quad *quad;
    struct quad_list_entry *next;
} quad_list_entry;

enum op_codes {
    NONE_OC = 0,
    LOAD_OC,
    STORE_OC,
    LEA_OC,
    MOV_OC,
    ADD_OC,
    SUB_OC,
    MUL_OC,
    DIV_OC,
    MOD_OC,
    AND_OC,
    OR_OC,
    XOR_OC,
    SHL_OC,
    SHR_OC,
    EQEQ_OC,
    NEQ_OC,
    LT_OC,
    GT_OC,
    LTEQ_OC,
    GTEQ_OC,
    CMP_OC
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

// Creates a new basic block
struct basic_block *create_basic_block(char *block_label);

// Sets block as current block and updates global variable
void set_block(basic_block *block);

// Links given block(s) with current basic block
// Branch is set to the true branch, while next is set to the false branch
void link_blocks(basic_block *true_branch, basic_block *false_branch, int op_code);

// Generates the quads for a function and stores blocks in linked list
void generate_function_quads();

// Given AST node, recursively generates quads
void generate_quads(struct astnode *node);

// Creates a new quad and appends it to linked list of quads
void *emit_quad(int op_code, int op_size, struct astnode *src1, struct astnode *src2, struct astnode *dest);

// Gets r value of expression
struct astnode *get_rvalue(struct astnode *node, struct astnode *target);

// Gets l value of expression
struct astnode *get_lvalue(struct astnode *node, int *mode);

// Creates a temporary node
struct astnode *create_temp_node();

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
void gen_break_IR();

// Generates IR for continue
void gen_continue_IR();

// Generates IR for return
void gen_return_IR(astnode *node);
#endif // QUADS_H