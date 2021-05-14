// Andrew Lorber
// Compilers - Assembly Generator

#include "../Parser/quads.h"
#include "assembly.h"
#include "../Parser/symbol_table.h"


// Generates assembly of file
// Assumes input of output file name
void gen_assembly(char *out_file_name) {
    // Creates assembly file for output
    FILE *out_file = fopen(out_file_name, 'w+');

    fprintf(out_file, "    .file \"%s\"\n", out_file_name);

    // Declares global variables in comm section
    // Assuming no initialized variables
    gen_global_assembly(out_file);

    // Starts text section

    // Loops through functions
        // Generates assembly for function
}

// Generates assembly for global variables
void gen_global_assembly(FILE *out_file) {
    // Gets global scope entries in OTHER_NAMESPACE
    scope_entry *global_scope = get_global_scope();
    astnode *global_sym_entries = get_table_members(global_scope->sym_tables[OTHER_NS]);

    // Loops through global scope
    astnode_list_entry *curr_sym_entry = &(global_sym_entries->ast_node_list_head);
    while(curr_sym_entry != NULL) {
        // Checks if global variable found
        if(curr_sym_entry->node->ast_sym_entry.sym_type == VAR_TYPE) {
            // Calculates the size
            astnode *var_size = get_size_of(curr_sym_entry->node);

            // Calculates the alignment
            int var_alignment = get_alignment_of(curr_sym_entry->node);

            // Prints to output
            fprintf(out_file, "    .comm    %s,%d,%d\n", 
                    curr_sym_entry->node->ast_sym_entry.symbol, var_size->ast_number.number.i_value, var_alignment);
        }
       curr_sym_entry = curr_sym_entry->next;
    }
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

// Gets alignment of variable
int get_alignment_of(astnode *node) {
    int alignment_size;

    enum alignment_sizes {
        CHAR_ALIGN_SIZE = 1,
        SHORT_ALIGN_SIZE = 2,
        INT_ALIGN_SIZE = 4,
        FLOAT_ALIGN_SIZE = 4,
        DOUBLE_ALIGN_SIZE = 4,
        LONG_ALIGN_SIZE = 4,
        LONGLONG_ALIGN_SIZE = 4,
        POINTER_ALIGN_SIZE = 4,
        LONGDOUBLE_ALIGN_SIZE = 4
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
                    alignment_size = INT_ALIGN_SIZE;
                    break;

                case FLOAT_TYPE:
                    alignment_size = FLOAT_ALIGN_SIZE;
                    break;

                case DOUBLE_TYPE:
                    alignment_size = DOUBLE_ALIGN_SIZE;
                    break;

                case LONG_TYPE:
                    alignment_size = LONG_ALIGN_SIZE;
                    break;

                case LONGLONG_TYPE:
                    alignment_size = LONGLONG_ALIGN_SIZE;
                    break;

                case LONGDOUBLE_TYPE:
                    alignment_size = LONGDOUBLE_ALIGN_SIZE;
                    break;
                
                default:
                    fprintf(stderr, "ERROR: Alignment of NUMBER_TYPE unknown.\n");
            }
            break;

        case SCALAR_TYPE:
            // Gets size
            switch(node->ast_scalar.scalar_type) {
                case CHAR_ST:
                    alignment_size = CHAR_ALIGN_SIZE;
                    break;

                case SHORT_ST:
                    alignment_size = SHORT_ALIGN_SIZE;
                    break;

                case INT_ST:
                    alignment_size = INT_ALIGN_SIZE;
                    break;

                case LONG_ST:
                    alignment_size = LONG_ALIGN_SIZE;
                    break;

                case LONG_LONG_ST:
                    alignment_size = LONGLONG_ALIGN_SIZE;
                    break;

                case FLOAT_ST:
                    alignment_size = FLOAT_ALIGN_SIZE;
                    break;

                case DOUBLE_ST:
                    alignment_size = DOUBLE_ALIGN_SIZE;
                    break;

                case LONG_DOUBLE_ST:
                    alignment_size = LONGDOUBLE_ALIGN_SIZE;
                    break;

                case BOOL_ST:
                    alignment_size = CHAR_ALIGN_SIZE;
                    break;

                default:
                    fprintf(stderr, "ERROR: Alignment of SCALAR_TYPE unknown.\n");
            }
            break;

        case POINTER_TYPE:
            alignment_size = POINTER_ALIGN_SIZE;
            break;

        case ARRAY_TYPE:
            alignment_size = node->ast_array.arr_size * get_size_of(node->ast_array.arr_type)->ast_number.number.i_value;
            break;

        default:
            fprintf(stderr, "ERROR: Cannot determine alignment.\n");
            return NULL; // Should this kill the program?
    }

    // Returns alignment
    return alignment_size;
}