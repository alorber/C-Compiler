// Andrew Lorber
// Compilers - Assembly Generator

// All assembly is x86 - 32 Bit

#include "../Parser/quads.h"
#include "assembly.h"
#include "../Parser/symbol_table.h"

extern basic_block_list *block_list;
extern char filename[256];

// Generates assembly of file
// Assumes input of output file name
void gen_assembly(char *out_file_name) {
    // Creates assembly file for output
    FILE *out_file = fopen(out_file_name, "w+");

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
        gen_function_assembly(out_file, curr_function_block->bb);

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
            fprintf(out_file, "    .comm    %s,%lld,%d\n", 
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
        fprintf(out_file, "    subl  $%d, %%esp\n", local_var_size);
    }

    // Generates assembly for quads in function
    basic_block *last_block = gen_block_assembly(out_file, function_block, 1);

    // Generates assembly for return
    // Checks if explicit return, if not then 0
    if(last_block->branch_condition != RETURN_OC) {
        // Sets return value
            fprintf(out_file, "    movl  $0, %%eax\n");
            // Resets stack frame
            fprintf(out_file, "    leave\n");
            // Returns
            fprintf(out_file, "    ret\n");
    }

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
    symbol_table *sym_table = fnc_sym_entry->ast_sym_entry.sym_node->ast_compound_stmt.block_scope->sym_tables[OTHER_NS];

    // Gets members of OTHER_NAMESPACE
    astnode *sym_entries = get_table_members(sym_table);

    // Loops through symbol table entries
    astnode_list_entry *curr_sym_entry = &(sym_entries->ast_node_list_head);
    long int total_offset = 0;
    // while(curr_sym_entry != NULL) {
    //     // Checks if variable found
    //     if(curr_sym_entry->node->ast_sym_entry.sym_type == VAR_TYPE) {
    //         // Calculates the size
    //         astnode *var_size = get_size_of(curr_sym_entry->node);

    //         // Updates Offset
    //         total_offset += var_size->ast_number.number.i_value;
    //         curr_sym_entry->node->ast_sym_entry.ident_var.stack_frame_offset = -total_offset;
    //     }
    //     curr_sym_entry = curr_sym_entry->next;
    // }
    // Yes, this takes twice as long
    // But the one above causes a seg fault and for some reason the one below works
    while(curr_sym_entry != NULL) {
        // Checks if variable found
        if(curr_sym_entry->node->ast_sym_entry.sym_type == VAR_TYPE) {
            // Calculates the size
            astnode *var_size = get_size_of(curr_sym_entry->node);

            // Updates Total Offset
            total_offset += var_size->ast_number.number.i_value;
        }
        curr_sym_entry = curr_sym_entry->next;
    }

    long int offset = total_offset;
    curr_sym_entry = &(sym_entries->ast_node_list_head);
    while(curr_sym_entry != NULL) {
        // Checks if variable found
        if(curr_sym_entry->node->ast_sym_entry.sym_type == VAR_TYPE) {
            // Calculates the size
            astnode *var_size = get_size_of(curr_sym_entry->node);

            // Updates Variable Offset
            curr_sym_entry->node->ast_sym_entry.ident_var.stack_frame_offset = -offset;
            offset -= var_size->ast_number.number.i_value;
        }
        curr_sym_entry = curr_sym_entry->next;
    }

    return total_offset;
}

// Generates assembly for all quads in a function
// Generates assembly for a basic block and all blocks branching from it
// Returns last basic block printed in chain (used for conditional branching)
// @Param is_top_level is so function label isn't printed twice
basic_block *gen_block_assembly(FILE *out_file, basic_block *block, int is_top_level) {
    // Checks if block was already printed
    if(block == NULL || block->was_translated) {
        return NULL;
    }

    // Prints block label (Checks if top level block, so doesn't print fnc label twice)
    if(is_top_level == 0) {
        fprintf(out_file, "%s:\n", block->block_label);
    }

    // Picks assembly instruction for quad
    quad_list_entry *curr_quad = block->quad_list;
    while(curr_quad) {
        pick_instruction(out_file, curr_quad->quad);
        curr_quad = curr_quad->next;
    }

    // Marks as translated
    block->was_translated = 1;

    // Checks for branch (Could check int range, but explicit in case enum value changes)
    if(block->branch_condition == EQEQ_OC || block->branch_condition == NEQ_OC ||
       block->branch_condition == LT_OC || block->branch_condition == GT_OC ||
       block->branch_condition == LTEQ_OC || block->branch_condition == GT_OC) {

        // Prints assembly for branch jump
        pick_jump_instruction(out_file, block);

        // Prints true branch
        basic_block *true_branch_end = gen_block_assembly(out_file, block->branch, 0);

        // If true branch defaults into the false branch (i.e no else statement)
        //    continues with false branch
        if(true_branch_end != NULL && true_branch_end->next == block->next) {
            return gen_block_assembly(out_file, block->next, 0);
        } else {
            // Prints false branch
            basic_block *false_branch_end = gen_block_assembly(out_file, block->next, 0);

            // Continues with blocks
            return gen_block_assembly(out_file, false_branch_end->next, 0);
        }
    }
    // If no next, block return
    else if(block->next == NULL) {
        return block;
    }
    // Checks if next block was already translated
    else if(block->next->was_translated) {
        // Prints jump assembly
        pick_jump_instruction(out_file, block);
        return block;
    }
    // Checks if end of if-else chain and returning to main block chain.
    // The CMP_OC is just an arbitrary value chosen to represent this.
    // Stops here, so blocks can be printed in correct order.
    else if(block->branch_condition == CMP_OC) {
        return block;
    }
    // Otherwise, continue with blocks
    else {
        return gen_block_assembly(out_file, block->next, 0);
    }
}

// Given a quad, decides the best assembly instruction(s)
void pick_instruction(FILE *out_file, quad *curr_quad) {
    // Checks if destination
    // If yes, reserve register
    if(curr_quad->dest != NULL) {
        allocate_register(curr_quad->dest);
    }

    // Checks if either source is a string
    if(curr_quad->src1 != NULL && curr_quad->src1->node_type == STRING_TYPE) {
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
    if(curr_quad->src2 != NULL && curr_quad->src2->node_type == STRING_TYPE) {
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
        // astnode for temporary register
        astnode *temp_reg;

        // Addressing & Assigning
        case LOAD_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);
            astnode *temp_reg2 = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  (%s), %s\n", node_to_assembly(temp_reg), node_to_assembly(temp_reg2));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg2), node_to_assembly(curr_quad->dest));

            free_register(temp_reg2);

            break;

        case STORE_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);
            
            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, (%s)\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            break;

        case LEA_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    leal  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->src1));
        
            // Frees temp register
            free_register(temp_reg);
            
            break;

        case MOV_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);
            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            // Frees temp register
            free_register(temp_reg);
            break;

        // Arithmetic Operations
        case ADD_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    addl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case SUB_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    subl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case MUL_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);

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
        case AND_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    andl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case OR_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    orl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case XOR_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    xorl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case SHL_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);

            // Prints assembly
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(temp_reg));
            fprintf(out_file, "    shl  %s, %s\n", node_to_assembly(curr_quad->src2), node_to_assembly(temp_reg));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(temp_reg), node_to_assembly(curr_quad->dest));
            
            // Frees temp register
            free_register(temp_reg);

            break;

        case SHR_OC:;
            // Gets temp register
            temp_reg = allocate_register(NULL);

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
            fprintf(out_file, "    notl  %s\n", node_to_assembly(curr_quad->src1));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(curr_quad->dest));

            break;

        case NEG_OC:
            // Prints assembly
            fprintf(out_file, "    negl  %s\n", node_to_assembly(curr_quad->src1));
            fprintf(out_file, "    movl  %s, %s\n", node_to_assembly(curr_quad->src1), node_to_assembly(curr_quad->dest));

            break;

        case COMPL_OC:
            // Prints assembly
            fprintf(out_file, "    notl  %s\n", node_to_assembly(curr_quad->src1));
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
            // Do nothing since number is passed with call?
            break;

        case ARG_OC:
            // Prints assembly
            fprintf(out_file, "    pushl  %s\n", node_to_assembly(curr_quad->src1));

            break;

        case CALL_OC:
            // Prints assembly
            fprintf(out_file, "    call  %s\n", curr_quad->src1->ast_fnc_call.function_name->ast_string.string);

            // Resets stack pointer
            if(curr_quad->src2->ast_number.number.i_value > 0) {
                // Default to size of int / pointer
                fprintf(out_file, "    addl  $%lld, %%esp\n", curr_quad->src2->ast_number.number.i_value * 4);
            }

            // Moves to target, if needed
            if(curr_quad->dest != NULL) {
                fprintf(out_file, "    movl  %%eax %s\n", node_to_assembly(curr_quad->dest));
            } 

            break;

    }

    // Frees registers used by sources
    free_register(curr_quad->src1);
    free_register(curr_quad->src2);

}

// Prints assembly for block jump
// Will either get a conditional jump or a jump to a block already translated
void pick_jump_instruction(FILE *out_file, basic_block *block) {
    // Checks if comparison operator
    // Applies conditional inversion if possible
    switch(block->branch_condition) {
        case EQEQ_OC:
            // Prints assembly
            fprintf(out_file, "    jne  %s\n", block->next->block_label);
            break;

        case NEQ_OC:
            // Prints assembly
            fprintf(out_file, "    je  %s\n", block->next->block_label);
            break;
            
        case LT_OC:
            // Prints assembly
            fprintf(out_file, "    jge  %s\n", block->next->block_label);
            break;

        case GT_OC:
            // Prints assembly
            fprintf(out_file, "    jle  %s\n", block->next->block_label);
            break;
            
        case LTEQ_OC:
            // Prints assembly
            fprintf(out_file, "    jg  %s\n", block->next->block_label);
            break;
            
        case GTEQ_OC:
            // Prints assembly
            fprintf(out_file, "    jl  %s\n", block->next->block_label);
            break;
            
        // Not conditional operator
        default:
            // Prints assembly
            fprintf(out_file, "    jmp  %s\n", block->next->block_label);
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
            return 0; // Should this kill the program?
    }

    // Returns alignment
    return alignment_size;
}

// Given node, returns assembly reference
char *node_to_assembly(astnode *node) {
    char *node_name = calloc(256, sizeof(char));

    switch(node->node_type) {
        case TEMP_TYPE:
            // Checks for register
            switch(node->ast_temp_node.curr_register) {
                case NONE_REGISTER:
                    sprintf(node_name, "No register allocated");
                    break;
                case EAX_REGISTER:
                    sprintf(node_name, "%%eax");
                    break;
                case EBX_REGISTER:
                    sprintf(node_name, "%%ebx");
                    break;
                case ECX_REGISTER:
                    sprintf(node_name, "%%ecx");
                    break;
                case EDX_REGISTER:
                    sprintf(node_name, "%%edx");
                    break;
                case EDI_REGISTER:
                    sprintf(node_name, "%%edi");
                    break;
                case ESI_REGISTER:
                    sprintf(node_name, "%%esi");
                    break;
            }
            break;

        case SYM_ENTRY_TYPE:
            // Variable
            if(node->ast_sym_entry.sym_type == VAR_TYPE) {
                // Checks if global
                if(node->ast_sym_entry.ident_var.storage_class == EXTERN_SC) {
                    sprintf(node_name, "%s", node->ast_sym_entry.symbol);
                }
                // Local
                else {
                    sprintf(node_name, "%d(%%ebp)", node->ast_sym_entry.ident_var.stack_frame_offset);
                }
            }
            break;

        case NUMBER_TYPE:
            // Checks if signed
            if(node->ast_number.number.is_signed == SIGNED_TYPE) {
                switch(node->ast_number.number.size_specifier) {
                    case INT_TYPE:
                        sprintf(node_name, "$%d", (int) node->ast_number.number.i_value);
                        break;

                    case FLOAT_TYPE:
                        sprintf(node_name, "$%f", (float) node->ast_number.number.d_value);
                        break;

                    case DOUBLE_TYPE:
                        sprintf(node_name, "$%f", (double) node->ast_number.number.d_value);
                        break;

                    case LONG_TYPE:
                        sprintf(node_name, "$%ld", (long) node->ast_number.number.i_value);
                        break;

                    case LONGLONG_TYPE:
                        sprintf(node_name, "$%lld", (long long) node->ast_number.number.i_value);
                        break;

                    case LONGDOUBLE_TYPE:
                        sprintf(node_name, "$%Lf",  (long double) node->ast_number.number.d_value);
                    
                }
            } else {
                switch(node->ast_number.number.size_specifier) {
                    case INT_TYPE:
                        sprintf(node_name, "$%u", (unsigned int) node->ast_number.number.i_value);
                        break;

                    case LONG_TYPE:
                        sprintf(node_name, "$%lu", (unsigned long) node->ast_number.number.i_value);
                        break;

                    case LONGLONG_TYPE:
                        sprintf(node_name, "$%llu", (unsigned long long) node->ast_number.number.i_value);
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
void init_registers() {
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
    if(node != NULL && 
      ((node->node_type == TEMP_TYPE && node->ast_temp_node.curr_register != NONE_REGISTER) || 
       (node->node_type == SYM_ENTRY_TYPE && node->ast_sym_entry.sym_type == VAR_TYPE))) {
        return node;
    }

    // Otherwise, find open register
    for(int i = 0; i < NUM_REGISTERS; i++) {
        // Checks if register is free
        if(register_status[i] == 1) {
            // Set as taken
            register_status[i] = 0;

            // Checks if node exists (Assuming temp type)
            if(node == NULL) {
                // Creates temp node
                node = get_temp_node();
            } 

            node->ast_temp_node.curr_register = i;
            return node;
        }
    }

    // No free registers
    return NULL;
}

// Frees register used by node
void free_register(astnode *node) {
    if(node == NULL) {
        return;
    }

    // Checks if register was used
    if(node->node_type == TEMP_TYPE && node->ast_temp_node.curr_register != NONE_REGISTER) {
        // Frees register
        register_status[node->ast_temp_node.curr_register] = 1;
        node->ast_temp_node.curr_register = NONE_REGISTER;
    }
}