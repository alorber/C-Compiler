/* Andrew Lorber */
/* Compilers - Parser */
/* AST Functions File */

#include "astFunctions.h"

// -------------
// AST Functions
// -------------

// Allocates memory for new node & checks for errors
astnode* allocate_node_mem() {
    astnode *tmp_node;
    
    // Checks for error
    if((tmp_node = malloc(sizeof(astnode))) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for AST node.\n");
        exit(-1);
    }

    return tmp_node;
}

// Expression Nodes
// ----------------

// The following node functions set the type and members of the created node
//  and then return it.
astnode* create_unary_node(int op, astnode *expr) {
    astnode *unary_node = allocate_node_mem();
    unary_node->node_type = UNARY_TYPE;
    unary_node->ast_unary_op.op = op;
    unary_node->ast_unary_op.expr = expr;

    return unary_node;
}

astnode* create_binary_node(int op, astnode *left, astnode *right) {
    astnode *binary_node = allocate_node_mem();
    binary_node->node_type = BINARY_TYPE;
    binary_node->ast_binary_op.op = op;
    binary_node->ast_binary_op.left_expr = left;
    binary_node->ast_binary_op.right_expr = right;

    return binary_node;
}

// Builds a binary node for an expression with a compound operator (i.e +=)
astnode* simplify_compound_op(int op, astnode *left, astnode *right) {
    // Creates binary node for right side
    astnode *right_node = create_binary_node(op, left, right);
    
    // Creates main binary node
    astnode *binary_node = create_binary_node('=', left, right_node);

    return binary_node;
}

astnode* create_ternary_node(astnode *if_expr, astnode *then_expr, astnode *else_expr) {
    astnode *ternary_node = allocate_node_mem();
    ternary_node->node_type = TERNARY_TYPE;
    ternary_node->ast_ternary_op.if_expr = if_expr;
    ternary_node->ast_ternary_op.then_expr = then_expr;
    ternary_node->ast_ternary_op.else_expr = else_expr;

    return ternary_node;
}

astnode* create_number_node(num_type number) {
    astnode *number_node = allocate_node_mem();
    number_node->node_type = NUMBER_TYPE;
    number_node->ast_number.number = number;

    return number_node;
}

astnode* create_ident_node(char *ident) {
    astnode *ident_node = allocate_node_mem();
    ident_node->node_type = IDENT_TYPE;
    ident_node->ast_ident.ident = strdup(ident);

    return ident_node;
}

astnode* create_string_node(char *string) {
    astnode *string_node = allocate_node_mem();
    string_node->node_type = STRING_TYPE;
    string_node->ast_string.string = strdup(string);

    return string_node;
}

astnode* create_char_node(char *charlit) {
    astnode *char_node = allocate_node_mem();
    char_node->node_type = CHARLIT_TYPE;
    char_node->ast_charlit.charlit = strdup(charlit);

    return char_node;
}

astnode* create_fnc_call_node(astnode *function_name, astnode *expr_list) {
    astnode *fnc_call_node = allocate_node_mem();
    fnc_call_node->node_type = FUNCTION_CALL_TYPE;
    fnc_call_node->ast_fnc_call.function_name = function_name;
    fnc_call_node->ast_fnc_call.expr_list_head = expr_list;
    
    // Calculates number of arguments
    if(expr_list == NULL) {
        fnc_call_node->ast_fnc_call.num_arguments = 0;
    } else {
        int num_arguments = 1; 

        astnode_list_entry *curr_argument = expr_list->ast_node_list_head.next;
        while(curr_argument != NULL) {
            num_arguments++;
            curr_argument = curr_argument->next;
        }
        fnc_call_node->ast_fnc_call.num_arguments = num_arguments;
    }
    
    return fnc_call_node;
}

// Creates an expression list
astnode* init_node_list(astnode* node_list_head) {
    astnode *node_list_node = allocate_node_mem();
    node_list_node->node_type = NODE_LIST_TYPE;
    node_list_node->ast_node_list_head.node = node_list_head;
    node_list_node->ast_node_list_head.next = NULL;

    return node_list_node;
}

// Adds new argument node to expression list
astnode* add_node_to_list(astnode *node_list, astnode *new_argument) {
    // Creates new argument node
    astnode_list_entry *list_node;
    
    // Checks for error
    if((list_node = malloc(sizeof(astnode_list_entry))) == NULL) {
        fprintf(stderr, "ERROR: Unable to allocate memory for AST node.\n");
        exit(-1);
    }

    list_node->node = new_argument;
    list_node->next = NULL;

    // Get last node in list
    astnode_list_entry *curr_node = &(node_list->ast_node_list_head);
    while(curr_node->next != NULL) {
        curr_node = curr_node->next;
    }
    
    // Add new argument to list
    curr_node->next = list_node;

    // Returns head of list
    return node_list;
}

// Merges two node lists into one list
astnode *merge_node_lists(astnode *node_list, astnode *list_to_merge) {
    // Checks that list2 is actually a list
    if(list_to_merge->node_type != NODE_LIST_TYPE) {
        return add_node_to_list(node_list,list_to_merge);
    }

    // Get last node in list
    astnode_list_entry *curr_node = &(node_list->ast_node_list_head);
    while(curr_node->next != NULL) {
        curr_node = curr_node->next;
    }

    curr_node->next = &(list_to_merge->ast_node_list_head);
    
    return node_list;
}

// Creates number node with value of 1
// Helper function for "++" and "--"
astnode* create_num_one_node() {
    astnode *num_one = allocate_node_mem();
    num_one->node_type = NUMBER_TYPE;
    num_one->ast_number.number.is_signed = UNSIGNED_TYPE;
    num_one->ast_number.number.size_specifier = INT_TYPE;
    num_one->ast_number.number.i_value = 1;

    return num_one;
}

// Declaration Nodes
// -----------------

// Creates declaration specifier node
astnode *create_decl_spec_node(astnode* type_spec, int storage_class, int type_qual) {
    astnode *decl_spec = allocate_node_mem();
    decl_spec->node_type = DECL_SPEC_TYPE;
    decl_spec->ast_decl_spec.type_specifier = type_spec;
    decl_spec->ast_decl_spec.storage_class = storage_class;
    decl_spec->ast_decl_spec.type_qual = type_qual;
    decl_spec->ast_decl_spec.is_inline = 0;

    return decl_spec;
}

// Sets decl_spec_node function specifier to 'inline'
void set_decl_spec_node_inline(astnode *decl_spec) {
    decl_spec->ast_decl_spec.is_inline = 1;
}

// Merges declarator specifiers
// Adds updates from "addition" to "decl_spec" and frees "addition"
astnode *merge_decl_spec_nodes(astnode* addition, astnode *decl_spec) {
    // Only one field in addition will be filled, so checks which field
    // Checks type specifier
    if(addition->ast_decl_spec.type_specifier != NULL) {
        // Checks which field is updated
        if(decl_spec->ast_decl_spec.type_specifier == NULL) {
            decl_spec->ast_decl_spec.type_specifier = addition->ast_decl_spec.type_specifier;
        }
        else if(decl_spec->ast_decl_spec.type_specifier->ast_scalar.scalar_type == UNKNOWN_ST
        && addition->ast_decl_spec.type_specifier->ast_scalar.scalar_type != UNKNOWN_ST) {
            decl_spec->ast_decl_spec.type_specifier->ast_scalar.scalar_type = addition->ast_decl_spec.type_specifier->ast_scalar.scalar_type;
        } 
        else if(decl_spec->ast_decl_spec.type_specifier->ast_scalar.is_signed == UNKNOWN_SS
        && addition->ast_decl_spec.type_specifier->ast_scalar.is_signed != UNKNOWN_SS) {
            decl_spec->ast_decl_spec.type_specifier->ast_scalar.is_signed = addition->ast_decl_spec.type_specifier->ast_scalar.is_signed;    
        } 
        else {
            fprintf(stderr,"ERROR: Cannot merge declarator specifiers.\n");
            // ERROR
        }
    }
    // Checks storage class
    else if(addition->ast_decl_spec.storage_class != UNKNOWN_SC) {
        // Checks that decl_spec has no storage class
        if(decl_spec->ast_decl_spec.storage_class != UNKNOWN_SC) {
            // ERROR
        } else {
            decl_spec->ast_decl_spec.storage_class = addition->ast_decl_spec.storage_class;
        }
    }
    // Checks type qualifier
    else if(addition->ast_decl_spec.type_qual != NONE_TQ) {
        // Combines with bitwise OR
        decl_spec->ast_decl_spec.type_qual = decl_spec->ast_decl_spec.type_qual | addition->ast_decl_spec.type_qual;

        // Removes NONE_TQ bit if needed
        decl_spec->ast_decl_spec.type_qual = decl_spec->ast_decl_spec.type_qual & 0b1110;
    }
    // Checks inline
    else if(addition->ast_decl_spec.is_inline == 1) {
        decl_spec->ast_decl_spec.is_inline = 1;
    }

    // Frees addition
    free(addition);

    return decl_spec;
}

// Merges declaration specifiers with declarator list
// Returns list of symbol table entries
astnode *merge_spec_decl_list(astnode *spec, astnode* decl_list) {
    // Gets first declarator in list
    astnode_list_entry *curr_list_node = &(decl_list->ast_node_list_head);
    astnode *curr_sym_entry;  // Current symbol table entry

    // Updates symbol table entry list
    do {
        curr_sym_entry = curr_list_node->node;

        // Fills in necessary default values for the declarator specifier
        fill_defaults(spec);

        // Adds specifiers depending on declarator type
        switch(curr_sym_entry->ast_sym_entry.sym_type) {
            case VAR_TYPE:
                curr_sym_entry->ast_sym_entry.ident_var.storage_class = spec->ast_decl_spec.storage_class;
                curr_sym_entry->ast_sym_entry.ident_var.type_qual = spec->ast_decl_spec.type_qual;

                break;

            case FNC_NAME_TYPE:
                curr_sym_entry->ast_sym_entry.ident_fnc_name.storage_class = spec->ast_decl_spec.storage_class;
                curr_sym_entry->ast_sym_entry.ident_fnc_name.is_inline = spec->ast_decl_spec.is_inline;

                break;

            default:
                fprintf(stderr, "Unknown symbol table entry type %i or %s.\n", curr_sym_entry->ast_sym_entry.sym_type, ident_type_to_string(curr_sym_entry));
                break;
        }

        // Adds type specifier to end of node chain
        if(spec->ast_decl_spec.type_specifier != NULL) {
            build_declarator(spec->ast_decl_spec.type_specifier, curr_sym_entry);
        }

        // Gets next declarator in list
        curr_list_node = curr_list_node->next;

    } while (curr_list_node != NULL);

    // Returns new symbol table entry list
    return decl_list;
}

// Fills declarator specifier with necessary default values
void fill_defaults(astnode *specifier) {
    // Checks if storage class is needed
    if(specifier->ast_decl_spec.storage_class == UNKNOWN_SC) {
        // Checks current scope
        int curr_scope = get_inner_scope()->scope;
        if(curr_scope == FILE_SCOPE) {
            specifier->ast_decl_spec.storage_class = EXTERN_SC;
        } else {
            specifier->ast_decl_spec.storage_class = AUTO_SC;
        }
    }

    // Checks if type specifier is missing
    if(specifier->ast_decl_spec.type_specifier == NULL) {
        specifier->ast_decl_spec.type_specifier = create_scalar_node(INT_ST,SIGNED_SS);
    }

    // Checks if Scalar type (as opposed to struct / union)
    if(specifier->ast_decl_spec.type_specifier->node_type == SCALAR_TYPE) {
        // Checks if Scalar type specifier is missing type
        if(specifier->ast_decl_spec.type_specifier->ast_scalar.scalar_type == UNKNOWN_ST) {
            specifier->ast_decl_spec.type_specifier->ast_scalar.scalar_type = INT_ST;
        }
        // Checks if Scalar type specifier is missing sign
        if(specifier->ast_decl_spec.type_specifier->ast_scalar.is_signed == UNKNOWN_SS) {
            specifier->ast_decl_spec.type_specifier->ast_scalar.is_signed = SIGNED_SS;
        } 
    }
    
}

// Combines pointer into declarator symbol table entry
astnode *build_declarator(astnode *ptr, astnode *declarator) {
    // Checks type of declarator
    astnode *tmp_node;

    // If function -> set ptr as return type
    if(declarator->ast_sym_entry.sym_type == FNC_NAME_TYPE) {
        // Checks if no return type
        if(declarator->ast_sym_entry.ident_fnc_name.return_type == NULL) {
            declarator->ast_sym_entry.ident_fnc_name.return_type = ptr;
            return declarator;
        }

        tmp_node = declarator->ast_sym_entry.ident_fnc_name.return_type;
    } 
    // Else -> set ptr as type
    else {
        if(declarator->ast_sym_entry.sym_node == NULL) {
            declarator->ast_sym_entry.sym_node = ptr;
            return declarator;
        }

        tmp_node = declarator->ast_sym_entry.sym_node;
    }

    // Follows ptr / arr chain
    while(1) {
        if(tmp_node->node_type == POINTER_TYPE) {
            // End of chain
            if(tmp_node->ast_pointer.pointer_type == NULL) {
                tmp_node->ast_pointer.pointer_type = ptr;
                break;
            } 
            // Goes to next in chain
            else {
                tmp_node = tmp_node->ast_pointer.pointer_type;
            }
        } else if(tmp_node->node_type == ARRAY_TYPE) {
            // End of chain
            if(tmp_node->ast_array.arr_type == NULL) {
                tmp_node->ast_array.arr_type = ptr;
                break;
            }
            // Goes to next in chain
            else {
                tmp_node = tmp_node->ast_array.arr_type;
            }
        } else if(tmp_node->node_type == FUNCTION_TYPE) {
            // End of chain
            if(tmp_node->ast_function.return_type == NULL) {
                tmp_node->ast_function.return_type = ptr;
                break;
            } 
            // Goes to next in chain
            else {
                tmp_node = tmp_node->ast_function.return_type;
            }
        } else {
            // ERROR
            break;
        }
    }

    return declarator;
}

// Combines pointer into abstract declarator
astnode *build_abstract_declarator(astnode *ptr, astnode *declarator) {
    astnode *tmp_node;
    // Checks parent type
    if(declarator->node_type == POINTER_TYPE) {
        // Checks for empty chain
        if(declarator->ast_pointer.pointer_type == NULL) {
            declarator->ast_pointer.pointer_type = ptr;
            return declarator;
        } else {
            tmp_node = declarator->ast_pointer.pointer_type;
        }
    } else if(declarator->node_type == ARRAY_TYPE) {
        // Checks for empty chain
        if(declarator->ast_array.arr_type == NULL) {
            declarator->ast_array.arr_type = ptr;
            return declarator;
        } else {
            tmp_node = declarator->ast_array.arr_type;
        }
    } else if(declarator->node_type == FUNCTION_TYPE) {
        // Checks for empty chain
        if(declarator->ast_function.return_type == NULL) {
            declarator->ast_function.return_type = ptr;
            return declarator;
        } else {
            tmp_node = declarator->ast_function.return_type;
        }
    } else {
        // ERROR - Invalid input
    }

    // Follows ptr / arr chain
    while(1) {
        if(tmp_node->node_type == POINTER_TYPE) {
            // End of chain
            if(tmp_node->ast_pointer.pointer_type == NULL) {
                tmp_node->ast_pointer.pointer_type = ptr;
                break;
            } 
            // Goes to next in chain
            else {
                tmp_node = tmp_node->ast_pointer.pointer_type;
            }
        } else if(tmp_node->node_type == ARRAY_TYPE) {
            // End of chain
            if(tmp_node->ast_array.arr_type == NULL) {
                tmp_node->ast_array.arr_type = ptr;
                break;
            }
            // Goes to next in chain
            else {
                tmp_node = tmp_node->ast_array.arr_type;
            }
        } else if(tmp_node->node_type == FUNCTION_TYPE) {
            // End of chain
            if(tmp_node->ast_function.return_type == NULL) {
                tmp_node->ast_function.return_type = ptr;
                break;
            }
            // Goes to next in chain
            else {
                tmp_node = tmp_node->ast_function.return_type;
            }
        } else {
            // ERROR
            break;
        }
    }

    return declarator;
}

astnode *create_type_name_node(astnode *spec_qual_list, astnode *abstr_decl) {
    astnode *type_name_node = allocate_node_mem();
    type_name_node->node_type = TYPE_NAME_TYPE;
    type_name_node->ast_type_name.spec_qual_list = spec_qual_list;
    type_name_node->ast_type_name.abstr_declarator = abstr_decl;

    return type_name_node;
}

// Statement Nodes
// ---------------

astnode *create_label_stmt_node(int label_type, astnode *label, astnode *statement) {
    astnode *label_stmt_node = allocate_node_mem();
    label_stmt_node->node_type = LABEL_STMT_TYPE;
    label_stmt_node->ast_label_stmt.label_type = label_type;
    label_stmt_node->ast_label_stmt.stmt = statement;

    // Checks label type
    if(label_type == GOTO_LABEL) {
        // Checks symbol table
        astnode *label_sym_table_entry = search_scope_stack(label->ast_ident.ident,LABEL_NS);

        // If symbol doesn't exist, adds to symbol table
        if(label_sym_table_entry == NULL) {
            label_sym_table_entry = create_sym_table_entry(label->ast_ident.ident);
            label_sym_table_entry->ast_sym_entry.sym_type = LABEL_TYPE;
            label_sym_table_entry->ast_sym_entry.sym_node = statement;

            add_entry_to_namespace(LABEL_NS,label_sym_table_entry,0);
        }
        // If symbol exists, checks if already declared
        else if(label_sym_table_entry->ast_sym_entry.sym_node != NULL) {
            fprintf(stderr, "ERROR: Label already defined at line %i in %s.\n", label_sym_table_entry->ast_sym_entry.line_num, label_sym_table_entry->ast_sym_entry.filename);
        }
        // Updates filename & line number
        else {
            label_sym_table_entry->ast_sym_entry.line_num = line_number;
            label_sym_table_entry->ast_sym_entry.filename = strdup(filename);
        }

        label_stmt_node->ast_label_stmt.label = label_sym_table_entry;
    } else {
        label_stmt_node->ast_label_stmt.label = label;
    }

    return label_stmt_node;
}

astnode *create_compound_stmt_node(astnode *statement_block, struct scope_entry *block_scope) {
    astnode *compound_stmt_node = allocate_node_mem();
    compound_stmt_node->node_type = COMPOUND_STMT_TYPE;
    compound_stmt_node->ast_compound_stmt.statement_block = statement_block;
    compound_stmt_node->ast_compound_stmt.block_scope = block_scope;

    return compound_stmt_node;
}

astnode *create_if_else_node(astnode *if_condition, astnode *if_body, astnode *else_body) {
    astnode *if_else_node = allocate_node_mem();
    if_else_node->node_type = IF_ELSE_TYPE;
    if_else_node->ast_if_else.if_condition = if_condition;
    if_else_node->ast_if_else.if_body = if_body;
    if_else_node->ast_if_else.else_body = else_body;

    return if_else_node;
}

astnode *create_while_loop_node(int is_do_while, astnode *condition, astnode *body) {
    astnode *while_loop_node = allocate_node_mem();
    while_loop_node->node_type = WHILE_LOOP_TYPE;
    while_loop_node->ast_while_loop.is_do_while = is_do_while;
    while_loop_node->ast_while_loop.condition = condition;
    while_loop_node->ast_while_loop.body = body;

    return while_loop_node;
}

astnode *create_for_loop_node(astnode *initialization, astnode *condition, astnode *update, astnode *body) {
    astnode *for_loop_node = allocate_node_mem();
    for_loop_node->node_type = FOR_LOOP_TYPE;
    for_loop_node->ast_for_loop.initialization = initialization;
    for_loop_node->ast_for_loop.condition = condition;
    for_loop_node->ast_for_loop.update = update;
    for_loop_node->ast_for_loop.body = body;

    return for_loop_node;
}

astnode *create_switch_node(astnode *expr, astnode *label_list) {
    astnode *switch_node = allocate_node_mem();
    switch_node->node_type = SWITCH_TYPE;
    switch_node->ast_switch.expr = expr;
    switch_node->ast_switch.label_list = label_list;

    return switch_node;
}

astnode *create_goto_stmt_node(char *label) {
    astnode *goto_stmt_node = allocate_node_mem();
    goto_stmt_node->node_type = GOTO_STMT_TYPE;
    
    // Checks if label is already in symbol table
    astnode *label_sym_entry = search_scope_stack(label,LABEL_NS);
    // If not, adds to symbol table
    if(label_sym_entry == NULL) {
        label_sym_entry = create_sym_table_entry(strdup(label));
        label_sym_entry->ast_sym_entry.sym_type = LABEL_TYPE;
        add_entry_to_namespace(LABEL_NS,label_sym_entry,0);
    }
    
    goto_stmt_node->ast_goto_stmt.label = label_sym_entry;

    return goto_stmt_node;
}

astnode *create_continue_break_stmt_node(int type) {
    astnode *continue_break_stmt_node = allocate_node_mem();
    continue_break_stmt_node->node_type = CONTINUE_BREAK_STMT_TYPE;
    continue_break_stmt_node->ast_continue_break_stmt.type = type;

    return continue_break_stmt_node;
}

astnode *create_return_node(astnode *return_expr) {
    astnode *return_node = allocate_node_mem();
    return_node->node_type = RETURN_TYPE;
    return_node->ast_return.return_expr = return_expr;
    
    return return_node;
}

// Type Nodes
// ----------

astnode *create_scalar_node(int scalar_type, int is_signed) {
    astnode *scalar_node = allocate_node_mem();
    scalar_node->node_type = SCALAR_TYPE;
    scalar_node->ast_scalar.scalar_type = scalar_type;
    scalar_node->ast_scalar.is_signed = is_signed;

    return scalar_node;
}

astnode *create_pointer_node(astnode *parent_ptr, astnode *type_qual_list) {
    astnode *new_pointer = allocate_node_mem();
    new_pointer->node_type = POINTER_TYPE;

    if(type_qual_list == NULL) {
        new_pointer->ast_pointer.type_qual = NONE_TQ;
    } else {
        new_pointer->ast_pointer.type_qual = type_qual_list->ast_decl_spec.type_qual;
    }

    // Checks if no parent pointer (returns new pointer)
    if(parent_ptr == NULL) {
        return new_pointer;
    }

    // Finds last pointer in chain to add new pointer to
    astnode *tmp_ptr =  parent_ptr;
    for(;tmp_ptr->ast_pointer.pointer_type != NULL; tmp_ptr = tmp_ptr->ast_pointer.pointer_type);
    tmp_ptr->ast_pointer.pointer_type = new_pointer;

    return parent_ptr;
}

astnode *create_array_node(int size, astnode *type) {
    astnode *arr_node = allocate_node_mem();
    arr_node->node_type = ARRAY_TYPE;
    arr_node->ast_array.arr_size = size;
    arr_node->ast_array.arr_type = type;

    return arr_node;
}

astnode *create_function_node(int num_args, astnode *return_type, astnode *arg_types) {
    astnode *fnc_node = allocate_node_mem();
    fnc_node->node_type = FUNCTION_TYPE;
    fnc_node->ast_function.num_args = num_args;
    fnc_node->ast_function.return_type = return_type;
    fnc_node->ast_function.arg_types = arg_types;  // ast node list

    return fnc_node;
}

astnode *add_to_arr_ptr_chain(astnode *parent_node, int type_to_add) {
    // Creates new node (checks type to create)
    astnode *new_node;
    if(type_to_add == FUNCTION_TYPE) {
        // Creates function node
        new_node = create_function_node(-1,NULL,NULL);
    } else if(type_to_add == POINTER_TYPE) {
        // Creates pointer node
        new_node = create_pointer_node(NULL,NULL);
    } else {
        // ERROR - Invalid input
        return NULL;
    }

    astnode *tmp_node;
    // Checks parent type
    if(parent_node->node_type == POINTER_TYPE) {
        // Checks for empty chain
        if(parent_node->ast_pointer.pointer_type == NULL) {
            parent_node->ast_pointer.pointer_type = new_node;
            return parent_node;
        } else {
            tmp_node = parent_node->ast_pointer.pointer_type;
        }
    } else if(parent_node->node_type == ARRAY_TYPE) {
        // Checks for empty chain
        if(parent_node->ast_array.arr_type == NULL) {
            parent_node->ast_array.arr_type = new_node;
            return parent_node;
        } else {
            tmp_node = parent_node->ast_array.arr_type;
        }
    } else {
        // ERROR - Invalid input
    }

    // Follows ptr / arr chain
    while(1) {
        if(tmp_node->node_type == POINTER_TYPE) {
            // End of chain
            if(tmp_node->ast_pointer.pointer_type == NULL) {
                tmp_node->ast_pointer.pointer_type = new_node;
                break;
            } 
            // Goes to next in chain
            else {
                tmp_node = tmp_node->ast_pointer.pointer_type;
            }
        } else if(tmp_node->node_type == ARRAY_TYPE) {
            // End of chain
            if(tmp_node->ast_array.arr_type == NULL) {
                tmp_node->ast_array.arr_type = new_node;
                break;
            }
            // Goes to next in chain
            else {
                tmp_node = tmp_node->ast_array.arr_type;
            }
        } else {
            // ERROR
            break;
        }
    }

    return parent_node;
}

// IR Gen Nodes
// ------------

// Creates a temp node to be used for IR generation
// temp_num is the number temp nodes, to be used for name creation
astnode *create_temp_node(int temp_num) {
    astnode *temp_node = allocate_node_mem();
    temp_node->node_type = TEMP_TYPE;
    temp_node->ast_temp_node.temp_value = NULL;
    
    // Generates a name for the temp node
    char *name = calloc(256, sizeof(char));
    sprintf(name, "Temp_Node_%d", temp_num);
    temp_node->ast_temp_node.name = name;

    return temp_node;
}

// Symbol Table Nodes
// ------------------

// Creates a new symbol table entry, only filling symbol field
astnode *create_sym_table_entry(char *ident) {
    astnode *sym_entry = allocate_node_mem();
    sym_entry->node_type = SYM_ENTRY_TYPE;
    sym_entry->ast_sym_entry.symbol = ident;
    sym_entry->ast_sym_entry.sym_type = VAR_TYPE;  // Defaults to variable type
    sym_entry->ast_sym_entry.sym_node = NULL;
    sym_entry->ast_sym_entry.filename = strdup(filename);
    sym_entry->ast_sym_entry.line_num = line_number;

    return sym_entry;
}

// Updates a symbol table entry by adding an array or function node
// If type_to_add = FUNCTION_TYPE & no symbol table node, then entry is set to FNC_NAME_TYPE,
//   otherwise new node is added to end of sym_node chain
// type_to_add parameter uses same type enum as astnode
// arr_size parameter is only used for array nodes
astnode *create_arr_fnc_sym_entry(astnode *sym_table_entry, int type_to_add, int arr_size) {
    // Checks if entry should be changed to FNC_NAME_TYPE
    if(sym_table_entry->ast_sym_entry.sym_type != FNC_NAME_TYPE && type_to_add == FUNCTION_TYPE && sym_table_entry->ast_sym_entry.sym_node == NULL) {
        sym_table_entry->ast_sym_entry.sym_type = FNC_NAME_TYPE;
        sym_table_entry->ast_sym_entry.ident_fnc_name.return_type = NULL;
        return sym_table_entry;
    }

    // Creates new node (checks type to create) that will be added
    astnode *new_node;
    if(type_to_add == ARRAY_TYPE) {
        // Creates array node
        new_node = create_array_node(arr_size,NULL);
    } else if(type_to_add == FUNCTION_TYPE) {
        // Creates function node
        new_node = create_function_node(-1,NULL,NULL);
    } else {
        // ERROR - Invalid input
        return NULL;
    }

    // Adds to current symbol table entry
    // Checks if needs to follow ptr / array chain
    astnode* tmp_node;
    if(sym_table_entry->ast_sym_entry.sym_type == FNC_NAME_TYPE) {
        // Checks if needs to follow ptr / array chain
        if(sym_table_entry->ast_sym_entry.ident_fnc_name.return_type == NULL) {
            sym_table_entry->ast_sym_entry.ident_fnc_name.return_type = new_node;
            return sym_table_entry;
        } else {
            tmp_node = sym_table_entry->ast_sym_entry.ident_fnc_name.return_type;
        }
    } else { // VAR Type
        if(sym_table_entry->ast_sym_entry.sym_node == NULL) {
            sym_table_entry->ast_sym_entry.sym_node = new_node;
            return sym_table_entry;
        } else {
            tmp_node = sym_table_entry->ast_sym_entry.sym_node;
        }
    }
        
    // Follows ptr / arr chain
    while(1) {
        if(tmp_node->node_type == POINTER_TYPE) {
            // End of chain
            if(tmp_node->ast_pointer.pointer_type == NULL) {
                tmp_node->ast_pointer.pointer_type = new_node;
                break;
            } 
            // Goes to next in chain
            else {
                tmp_node = tmp_node->ast_pointer.pointer_type;
            }
        } else if(tmp_node->node_type == ARRAY_TYPE) {
            // End of chain
            if(tmp_node->ast_array.arr_type == NULL) {
                tmp_node->ast_array.arr_type = new_node;
                break;
            }
            // Goes to next in chain
            else {
                tmp_node = tmp_node->ast_array.arr_type;
            }
        } else {
            // ERROR
            break;
        }
    }
    
    return sym_table_entry;

}

// Creates struct / union symbol table entry or returns previously defined entry
astnode *create_struct_union_sym_entry(int struct_or_union, char *ident, int is_abstract) {
    astnode *struct_union_node = allocate_node_mem();
    struct_union_node->node_type = SYM_ENTRY_TYPE;
    struct_union_node->ast_sym_entry.sym_type = STRUCT_UNION_TAG_TYPE;
    struct_union_node->ast_sym_entry.symbol = ident;
    struct_union_node->ast_sym_entry.sym_node = NULL;
    struct_union_node->ast_sym_entry.ident_struct_union_tag.is_struct = struct_or_union; 
    struct_union_node->ast_sym_entry.ident_struct_union_tag.sym_table = NULL;
    struct_union_node->ast_sym_entry.ident_struct_union_tag.is_defined = 0;

    // If not abstract, checks if already defined
    if(is_abstract == 0) {
        astnode *lookup = search_table(get_inner_scope()->sym_tables[TAG_NS],ident);

        if(lookup) {
            free(struct_union_node);
            struct_union_node = lookup;

            if(struct_union_node->ast_sym_entry.ident_struct_union_tag.is_defined) {
                // Already defined
                return struct_union_node;
            }
        }
        // Else, adds to symbol table
        else {
            add_entry_to_namespace(TAG_NS,struct_union_node,0);
        }
    }

    // Sets it here, in case it needs to update info on previously created struct / union
    struct_union_node->ast_sym_entry.filename = strdup(filename);
    struct_union_node->ast_sym_entry.line_num = line_number;

    return struct_union_node;    
}

// Adds struct / union members to struct / union symbol table
int add_struct_union_members(astnode *struct_union_node, astnode *members) {
    // Checks if previously defined
    if(struct_union_node->ast_sym_entry.symbol != NULL) {
        astnode *lookup = search_table(get_inner_scope()->sym_tables[TAG_NS],struct_union_node->ast_sym_entry.symbol);

        if(lookup && struct_union_node->ast_sym_entry.ident_struct_union_tag.is_defined) {
                // ERROR - Already defined
                fprintf(stderr,"ERROR: Struct / Union %s already defined @ line %i in file %s.\n", struct_union_node->ast_sym_entry.symbol, struct_union_node->ast_sym_entry.line_num, struct_union_node->ast_sym_entry.filename);
                return -1;
        }
    }


    // Creates symbol table
    struct_union_node->ast_sym_entry.ident_struct_union_tag.sym_table = create_table();

    // Loops through members
    int return_val = 1;
    astnode_list_entry *curr_list_node = &(members->ast_node_list_head);
    while(curr_list_node != NULL) {
        // Sets entry type to struct / union member
        curr_list_node->node->ast_sym_entry.sym_type = STRUCT_UNION_MEMBER_TYPE;

        // Adds member to symbol table
        if(add_entry_to_table(struct_union_node->ast_sym_entry.ident_struct_union_tag.sym_table,
        curr_list_node->node,0) == -1) {
            return_val = -1;
        }
        curr_list_node = curr_list_node->next;
    }

    // Sets as complete
    struct_union_node->ast_sym_entry.ident_struct_union_tag.is_defined = 1;

    return return_val;
}

// Printing Functions
// -------------------

// Converts storage class enum to string for printing
char *storage_class_to_string(int storage_class) {
    switch(storage_class) {
        case AUTO_SC:
            return "AUTO";
        case REGISTER_SC:
            return "REGISTER";
        case EXTERN_SC:
            return "EXTERN";
        case STATIC_SC:
            return "STATIC";
        case UNKNOWN_SC:
            return "NOT SPECIFIED";
        default:
            return "INVALID STORAGE CLASS";
    }
}

// Converts type qualifier enum to string for printing
// TODO: Use bitwise and to check each type
char *type_qual_to_string(int type_qual) {
    char *type_qual_string = malloc(25 * sizeof(char));
    
    // Checks if const
    if(type_qual & 0b10) {
        strcat(type_qual_string,"CONST ");
    }
    // Checks if volatile
    if(type_qual & 0b100) {
        strcat(type_qual_string,"VOLATILE ");
    }
    // Checks if restrict
    if(type_qual & 0b1000) {
        strcat(type_qual_string,"RESTRICT ");
    }

    return type_qual_string;
}

char *scalar_to_string(astnode *scalar_node) {
    char *scalar_sign;
    char *scalar_type;

    // Converts sign
    switch(scalar_node->ast_scalar.is_signed) {
        case SIGNED_SS:
            scalar_sign = "SIGNED ";
            break;
        case UNSIGNED_SS:
            scalar_sign = "UNSIGNED ";
            break;
        case UNKNOWN_SS:
            scalar_sign = "";
            break;
    }

    // Converts Type
    switch(scalar_node->ast_scalar.scalar_type) {
        case VOID_ST:
            scalar_type = "VOID";
            break;
        case CHAR_ST:
            scalar_type = "CHAR";
            break;
        case SHORT_ST:
            scalar_type = "SHORT";
            break;
        case INT_ST:
            scalar_type = "INT";
            break;
        case LONG_ST:
            scalar_type = "LONG";
            break;
        case LONG_LONG_ST:
            scalar_type = "LONG LONG";
            break;
        case FLOAT_ST:
            scalar_type = "FLOAT";
            break;
        case DOUBLE_ST:
            scalar_type = "DOUBLE";
            break;
        case LONG_DOUBLE_ST:
            scalar_type = "LONG DOUBLE";
            break;
        case BOOL_ST:
            scalar_type = "BOOL";
            break;
        case UNKNOWN_ST:
            scalar_type = "UNSPECIFIED-TYPE";
            break;
    }

    // Concatenates strings
    char *result = calloc(sizeof(scalar_sign)+sizeof(scalar_type),sizeof(char));
    strcat(result,scalar_sign);
    strcat(result,scalar_type);

    return result; 
}

// Converts IDENT type enum to string for printing
char *ident_type_to_string(astnode *node) {
    int ident_type = node->ast_sym_entry.sym_type;

    switch(ident_type) {
        case VAR_TYPE:
            return "VAR";
        case FNC_NAME_TYPE:
            return "FUNCTION NAME";
        case TYPEDEF_TYPE:
            return "TYPEDEF";
        case ENUM_CONST_TYPE:
            return "ENUM CONSTANT";
        case STRUCT_UNION_TAG_TYPE:
            if(node->ast_sym_entry.ident_struct_union_tag.is_struct == 1) {
                return "STRUCT";
            } else {
                return "UNION";
            }
        case ENUM_TAG_TYPE:
            return "ENUM TAG";
        case LABEL_TYPE:
            return "LABEL";
        case STRUCT_UNION_MEMBER_TYPE:
            return "STRUCT / UNION MEMBER";
        default:
            return "INVALID IDENT TYPE";
    }
}
