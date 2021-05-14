// Andrew Lorber
// Compilers - Assembly Generator Header File

#ifndef ASSEMBLY_H
#define ASSEMBLY_H

// Generates assembly of file
void gen_assembly();

// Generates assembly for global variables
void gen_global_assembly(FILE *out_file);

// Generates assembly for functions
void gen_function_assembly();

// Generates assembly for all quads in a function
// Generates assembly for a basic block and all blocks branching from it
void gen_block_assembly();

// Given a quad, decides the best assembly instruction(s)
void pick_instruction();

// Gets alignment of variable
int get_alignment_of(astnode *node);

#endif // ASSEMBLY_H