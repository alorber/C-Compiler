// Andrew Lorber
// Compilers - Assembly Generator

// All assembly is x86 - 32 Bit

#include "../Parser/quads.h"
#include "assembly.h"
#include "../Parser/symbol_table.h"

extern basic_block_list *block_list;

// Generates assembly of file
// Assumes input of output file name
void gen_assembly(char *out_file_name) {
    // Creates assembly file for output
    FILE *out_file = fopen(out_file_name, 'w+');

    fprintf(out_file, "    .file \"%s\"\n", out_file_name);

    // Declares global variables in comm section
    // Assuming no initialized variables (Not supported)
    gen_global_assembly(out_file);

    // Starts text section
    fprintf(out_file, "    .text\n");

    // Loops through functions
    basic_block_list_entry *curr_function_block = block_list->head;
    while(curr_function_block != NULL) {
        // Generates assembly for function
        gen_function_assembly(out_file, curr_function_block);

        curr_function_block = curr_function_block->next;
    }
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
void gen_function_assembly(FILE *out_file, basic_block *function_block) {
    // Prints assembly for function variable
    fprintf(out_file, "    .globl  %s\n", function_block->block_label);
    fprintf(out_file, "    .type   %s, @function\n", function_block->block_label);
    fprintf(out_file, "%s:\n", function_block->block_label);
    
    // Sets up stack frame
    fprintf(out_file, "    pushl  %%ebp\n");
    fprintf(out_file, "    movl  %%esp, %%ebp\n");

    // Reserves space for local variables
    int local_var_size = get_local_scope_size(function_block->block_label);
    if(local_var_size > 0) {
        fprintf(out_file, "    subl  $%d, %esp\n", local_var_size);
    }

    // Generates assembly for quads in function
    gen_block_assembly(out_file, function_block, 0);

    // Generates assembly for return
    // Checks if explicit return, if not then 0

    // Prints size of function
    fprintf(out_file, "    .size  %s, .-%s\n", function_block->block_label, function_block->block_label);

}

// Gets total size of local variables of function
// Sets offset of each variable within stack
int get_local_scope_size(char *fnc_symbol) {
    // Gets symbol table entry for function
    astnode *fnc_sym_entry;

    // Checks for errors
    if((fnc_sym_entry = search_scope_stack(fnc_symbol, OTHER_NS)) == NULL) {
        fprintf(stderr, "ERROR: Unable to find function in symbol table.\n");
        exit(-1);
    }

    // Gets symbol table for function
    symbol_table **sym_table = fnc_sym_entry->ast_sym_entry.sym_node->ast_compound_stmt.block_scope->sym_tables[OTHER_NS];

    // Gets members of OTHER_NAMESPACE
    astnode *sym_entries = get_table_members(sym_table[OTHER_NS]);

    // Loops through symbol table entries
    astnode_list_entry *curr_sym_entry = &(sym_entries->ast_node_list_head);
    long int total_offset = 0;
    while(curr_sym_entry != NULL) {
        // Checks if variable found
        if(curr_sym_entry->node->ast_sym_entry.sym_type == VAR_TYPE) {
            // Calculates the size
            astnode *var_size = get_size_of(curr_sym_entry->node);

            // Updates Offset
            total_offset += var_size->ast_number.number.i_value;
            curr_sym_entry->node->ast_sym_entry.ident_var.stack_frame_offset = -total_offset;
        }
        curr_sym_entry = curr_sym_entry->next;
    }

    return total_offset;
}

// Generates assembly for all quads in a function
// Generates assembly for a basic block and all blocks branching from it
// Returns last basic block printed in chain (used for conditional branching)
basic_block *gen_block_assembly(FILE *out_file, basic_block *block, int in_branch) {
    // Checks if block was already printed
    if(block == NULL || block->was_translated) {
        return NULL;
    }

    // Picks assembly instruction for quad
    quad_list_entry *curr_quad = block->quad_list;
    while(curr_quad) {
        pick_instruction(curr_quad->quad, out_file);
        curr_quad = curr_quad->next;
    }

    // Marks as translated
    block->was_translated = 1;

    // Checks for branch (Could check int range, but explicit in case enum changes)
    if(block->branch_condition == EQEQ_OC || block->branch_condition == NEQ_OC ||
       block->branch_condition == LT_OC || block->branch_condition == GT_OC ||
       block->branch_condition == LTEQ_OC || block->branch_condition == GT_OC) {

        // Prints assembly for branch jump
        pick_jump_instruction(out_file, block);

        // Prints true branch
        basic_block *true_branch_end = gen_block_assembly(out_file, block->branch, 1);

        // If true branch defaults into the false branch (i.e no else statement)
        //    continues with false branch
        if(true_branch_end != NULL && true_branch_end->branch_condition == NONE /*TODO*/) {
            return gen_block_assembly(out_file, block->next, 0);
        } else {
            // Prints false branch
            basic_block *false_branch_end = gen_block_assembly(out_file, block->next, 1);

            // Checks if true and false branches continue into same block
            // TODO: I don't think this will work!!!
            if(true_branch_end != NULL && false_branch_end != NULL && 
                true_branch_end->next == false_branch_end->next) {
                // Continues with blocks
                return gen_block_assembly(out_file, true_branch_end->next, in_branch);
            }
        }
    }
    // Checks if next block was already translated
    else if(block->next->was_translated) {
        // Prints jump assembly
        pick_jump_instruction(out_file, block);
        return block;
    }
    // Checks if next block
    if(block->next != NULL) {
        return gen_block_assembly(out_file, block->next, in_branch);
    }
    // Need to fix



    return block;
}

// Given a quad, decides the best assembly instruction(s)
void pick_instruction(FILE *out_file, quad *curr_quad) {
    // Checks if destination
    // If yes, reserve register
    if(curr_quad->dest != NULL) {
        allocate_register(curr_quad->dest);
    }

    // Checks if either source is a string
    if(curr_quad->src1->node_type == STRING_TYPE) {
        // Enters rodata section
        fprintf(out_file, "    .rodata\n");

        // Gets label for string
        char *string_label = get_string_label();

        // Prints assembly
        fprintf(out_file, "%s:\n", string_label);
        fprintf(out_file, "    .string  \"%s\"\n", curr_quad->src1->ast_string.string);

        // Changes src -> string memory location
        astnode *temp_reg = allocate_register(NULL);
        fprintf(out_file, ".data\n");
        fprintf(out_file, "    leal  %s, %s\n", string_label, node_to_assembly(temp_reg));
        curr_quad->src1 = temp_reg;
    }
    if(curr_quad->src2->node_type == STRING_TYPE) {
        // Enters rodata section
        fprintf(out_file, "    .rodata\n");

        // Gets label for string
        char *string_label = get_string_label();

        // Prints assembly
        fprintf(out_file, "%s:\n", string_label);
        fprintf(out_file, "    .string  \"%s\"\n", curr_quad->src2->ast_string.string);

        // Changes src -> string memory location
        astnode *temp_reg = allocate_register(NULL);
        fprintf(out_file, ".data\n");
        fprintf(out_file, "    leal  %s, %s\n", string_label, node_to_assembly(temp_reg));
        curr_quad->src2 = temp_reg;
    }

    // Checks op code
    switch(curr_quad->op_code) {
        // Addressing & Assigning
        case LOAD_OC:
            // Prints assembly
            fprintf(out_file, "    movl  (%s), %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(curr_quad->src1));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(curr_quad->dest));

            break;

        case STORE_OC:
            // Prints assembly
            fprintf(out_file, "    movl  %s, (%s)\n", node_to_assembly(curr_quad->src1), node_to_assembly(curr_quad->src2));

            break;

        case LEA_OC:
            // Gets temp register
            astnode *temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    leal  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->src1));
        
            // Frees temp register
            free_register(temp_reg);
            
            break;

        case MOV_OC:
            // Gets temp register
            astnode *temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        // Arithmetic Operations
        case ADD_OC:
            // Gets temp register
            astnode *temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    addl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case SUB_OC:
            // Gets temp register
            astnode *temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    subl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case MUL_OC:
            // Gets temp register
            astnode *temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    imull  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case DIV_OC:
            // Saves %eax
            fprintf(out_file, "    pushl %%eax\n");
            // src1 -> %eax
            fprintf(out_file, "    movl  %s, %%eax\n", node_to_assembly(curr_quad->src1));

            // Saves %edx
            fprintf(out_file, "    pushl %%edx\n");
            // Zeros out %edx
            fprintf(out_file, "    xor %%edx, %%edx\n");

            // Divides
            fprintf(out_file, "    idiv  %s\n", node_to_assembly(curr_quad->src2));
            fprintf(out_file, "    movl  %%eax, %s\n", node_to_assembly(curr_quad->dest));
            
            // Restores registers
            fprintf(out_file, "    popl  %%edx\n");
            fprintf(out_file, "    popl  %%eax\n");

            break;

        case MOD_OC:
            // Saves %eax
            fprintf(out_file, "    pushl %%eax\n");
            // src1 -> %eax
            fprintf(out_file, "    movl  %s, %%eax\n", node_to_assembly(curr_quad->src1));

            // Saves %edx
            fprintf(out_file, "    pushl %%edx\n");
            // Zeros out %edx
            fprintf(out_file, "    xor %%edx, %%edx\n");

            // Divides
            fprintf(out_file, "    idiv  %s\n", node_to_assembly(curr_quad->src2));
            fprintf(out_file, "    movl  %%edx, %s\n", node_to_assembly(curr_quad->dest));
            
            // Restores registers
            fprintf(out_file, "    popl  %%edx\n");
            fprintf(out_file, "    popl  %%eax\n");

            break;


        // Bitwise Operators
        case AND_OC:
            // Gets temp register
            astnode *temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    andl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case OR_OC:
            // Gets temp register
            astnode *temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    orl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case XOR_OC:
            // Gets temp register
            astnode *temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    xorl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case SHL_OC:
            // Gets temp register
            astnode *temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    shl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case SHR_OC:
            // Gets temp register
            astnode *temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    shr  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        // Comparison Operators
        case CMP_OC:
            // Prints assembly
            fprintf(out_file, "    cmpl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(curr_quad->src2));

            break;

        // Unary Operators
        case NOT_OC:
            // Prints assembly
            fprintf(out_file, "    notl  %s, %s\n", node_to_assembly(curr_quad->src1));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(curr_quad->dest));

            break;

        case NEG_OC:
            // Prints assembly
            fprintf(out_file, "    negl  %s, %s\n", node_to_assembly(curr_quad->src1));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(curr_quad->dest));

            break;

        case COMPL_OC:
            // Prints assembly
            fprintf(out_file, "    notl  %s, %s\n", node_to_assembly(curr_quad->src1));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(curr_quad->dest));

            break;

        // Function Operators
        case RETURN_OC:
            // Checks if something to return
            if(curr_quad->src1 != NULL) {
                // Sets return value
                fprintf(out_file, "    movl  %s, %%eax\n", node_to_assembly(curr_quad->src1));
                // Resets stack frame
                fprintf(out_file, "    leave\n");
                // Returns
                fprintf(out_file, "    ret\n");
            }
            
            break;

        case ARGBEGIN_OC:

        case ARG_OC:

        case CALL_OC:


    }
}

// Prints assembly for block jump
// Will either get a conditional jump or a jump to a block already translated
void pick_jump_instruction(FILE *out_file, basic_block *block) {
    // Checks if comparison operator
    // Applies conditional inversion if possible
    switch(block->branch_condition) {
        case EQEQ_OC:
            // Prints assembly
            fprintf(out_file, "    jne  %s\n", node_to_assembly(block->branch));
            break;

        case NEQ_OC:
            // Prints assembly
            fprintf(out_file, "    je  %s\n", node_to_assembly(block->branch));
            break;
            
        case LT_OC:
            // Prints assembly
            fprintf(out_file, "    jge  %s\n", node_to_assembly(block->branch));
            break;

        case GT_OC:
            // Prints assembly
            fprintf(out_file, "    jle  %s\n", node_to_assembly(block->branch));
            break;
            
        case LTEQ_OC:
            // Prints assembly
            fprintf(out_file, "    jg  %s\n", node_to_assembly(block->branch));
            break;
            
        case GTEQ_OC:
            // Prints assembly
            fprintf(out_file, "    jl  %s\n", node_to_assembly(block->branch));
            break;
            
        // Not conditional operator
        default:
            // Prints assembly
            fprintf(out_file, "    jmp  %s\n", node_to_assembly(block->next));
            break;
    }
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

// Given node, returns assembly reference
char *node_to_assembly(astnode *node) {

}

// Creates label for string in rodata section
char *get_string_label() {
    static int num_strings;  // Number of labels created (Used for naming)

    // Generates label
    char *string_label = calloc(256, sizeof(char));
    sprintf(string_label, ".LC%d", num_strings++);

    return string_label;
}

// Register Functions
// -------------------

// Initializes registers
void init_register() {
    // Checks for errors
    if((register_status = malloc(sizeof(int) * NUM_REGISTERS)) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for register array.\n");
        exit(-1);
    }

    // Sets all registers to available
    for(int i = 0; i < NUM_REGISTERS; i++) {
        register_status[i] = 1;
    }
}

// Allocates a register for node
astnode *allocate_register(astnode *node) {
    // If variable is already contained in memory, return it
    if(node != NULL && (node->node_type == TEMP_TYPE || 
       (node->node_type == SYM_ENTRY_TYPE && node->ast_sym_entry.sym_type == VAR_TYPE))) {
        return node;
    }

    // Otherwise, find open register
    for(int i = 0; i < NUM_REGISTERS; i++) {
        // Checks if register is free
        if(register_status[i] == 1) {
            // Set as taken
            register_status[i] = 0;

            // Checks if node exists
            if(node != NULL) {
                // TODO
            }
            // Create new temp node

        }
    }

    // No free registers
    return NULL;
}

// Frees register used by node
void free_register(astnode *node) {
    // Checks if no register was used
    // TODO

    // Frees register
    // TODO
}