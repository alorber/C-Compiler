// Andrew Lorber
// Compilers - Assembly Generator Header File

#ifndef ASSEMBLY_H
#define ASSEMBLY_H

// Generates assembly of file
void gen_assembly();

// Generates assembly for global variables
void gen_global_assembly(FILE *out_file);

// Generates assembly for functions
void gen_function_assembly(FILE *out_file, FILE *string_file, basic_block *function_block);

// Gets total size of local variables of function
int get_local_scope_size(char *fnc_symbol);

// Generates assembly for all quads in a function
// Generates assembly for a basic block and all blocks branching from it
// Returns last basic block printed in chain (used for conditional branching)
// @Param is_top_level is so function label isn't printed twice
basic_block *gen_block_assembly(FILE *out_file, FILE *string_file, basic_block *block, int is_top_level);

// Given a quad, decides the best assembly instruction(s)
void pick_instruction(FILE *out_file, FILE *string_file, quad *curr_quad);

// Prints assembly for block jump
void pick_jump_instruction(FILE *out_file, basic_block *block);

// Gets alignment of variable
int get_alignment_of(astnode *node);

// Given node, returns assembly reference
char *node_to_assembly(astnode *node);

// Creates label for string in rodata section
char *get_string_label();

// Register Functions
// ------------------

// Enum of registers
enum registers {
    NONE_REGISTER = -1,
    EAX_REGISTER = 0,
    EBX_REGISTER,
    ECX_REGISTER,
    EDX_REGISTER,
    EDI_REGISTER,
    ESI_REGISTER,
    NUM_REGISTERS
};

// Global array that stores taken / free status of each register
int *register_status;

// Initializes registers
void init_registers();

// Allocates a register for node
astnode *allocate_register(astnode *node);

// Frees register used by node
void free_register(astnode *node);

#endif // ASSEMBLY_H