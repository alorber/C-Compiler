// Andrew Lorber
// Compilers - Assembly Generator

#include "../Parser/quads.h"
#include "assembly.h"
#include "../Parser/symbol_table.h"


// Generates assembly of file
// Assume input of output file name
void gen_assembly(char *out_file_name) {
    // Create assembly file for output
    FILE *out_file = fopen(out_file_name, 'w+');

    fprintf(out_file, "    .file \"%s\"\n", out_file_name);

    // Declare global variables in comm section
    // Assuming no initialized variables
    gen_global_assembly(out_file);

    // Start text section

    // Loops through functions
        // Generate assembly for function
}

// Generates assembly for global variables
void gen_global_assembly(FILE *out_file) {
    // Loop through global scope

        // Checks if global variable found

            // Calculates the size

            // Calculates the alignment

            // Prints to output
}

// Generates assembly for functions
void gen_function_assembly() {
    // Prints assembly for function variable
    
    // Sets up stack frame

    // Reserves space for local variables

    // Generates assembly for quads in function

    // Generates assembly for return
    // Checks if explicit return, if not then 0

    // Prints size of function

}

// Generates assembly for all quads in a function
// Generates assembly for a basic block and all blocks branching from it
void gen_block_assembly() {

}

// Given a quad, decides the best assembly instruction(s)
void pick_instruction() {

}
