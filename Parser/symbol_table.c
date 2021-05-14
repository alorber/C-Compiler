/* Andrew Lorber */
/* Compilers - Parser */
/* Symbol Table File */

#include "symbol_table.h"

// Helper Functions
// -----------------

// Returns hashed index of symbol
int hash_symbol(char *symbol, int table_capacity) {
    // Uses basic hashing
    int hash_value = 0;

    for(int i = 0; i < strlen(symbol); i++) {
        hash_value = 37 * hash_value + symbol[i];
    }

    hash_value %= table_capacity;

    if(hash_value < 0) {
        hash_value += table_capacity;
    }

    return hash_value;
}

// Resizes table and rehashes elements in table to new table
int rehash_table(symbol_table *sym_table) {
    // Updates capacity
    int old_capacity = sym_table->capacity;
    if((sym_table->capacity = get_prime(old_capacity)) == -1) {
        // ERROR - Out of prime numbers
        fprintf(stderr, "ERROR: No more prime values. Cannot expand Symbol Table.\n");
        return -1;
    }

    // Updates table
    astnode **temp_table = sym_table->table;
    if((sym_table->table = (astnode **)calloc(sym_table->capacity,sizeof(astnode))) == NULL) {
        // ERROR
    }

    // Rehashes symbol entries
    for(int i = 0; i < old_capacity; i++) {
        if(temp_table[i]) {
            add_entry_to_table(sym_table, temp_table[i], 0);
        }
    }

    free(temp_table);

    return 1;
}

// Returns next prime in list to use as size
int get_prime(int curr_size) {
    const int prime_list_size = 8;
    const int prime_numbers[] = {61, 127, 257, 521, 1049, 2099, 4201, 8419};

    // Returns next highest prime number
    for(int i = 0; i < prime_list_size; i++) {
        if(prime_numbers[i] > curr_size) {
            return prime_numbers[i];
        }
    }

    // Out of prime numbers
    return -1;
}

// Symbol Table Functions
// -----------------------

// Creates a new symbol table
symbol_table *create_table() {
    // Creates symbol table struct
    symbol_table *sym_table;
    if((sym_table = calloc(1,sizeof(symbol_table))) == NULL) {
        // ERROR
    }

    sym_table->size = 0;
    sym_table->capacity = get_prime(0);

    // Creates symbol table array
    if((sym_table->table = (astnode **)calloc(sym_table->capacity,sizeof(astnode))) == NULL) {
        // ERROR
    }

    return sym_table;
}

// Destroys given symbol table
void destroy_table(symbol_table *sym_table) {
    for(int i = 0; i < sym_table->capacity; i++) {
        if(sym_table->table[i]) {
            free(sym_table->table[i]);
        }
    }
    free(sym_table->table);
    free(sym_table);
}

// Searches symbol table for symbol
astnode *search_table(symbol_table *sym_table, char *symbol) {
    // Gets symbol hash (Using linear probing)
    int hash_value = hash_symbol(symbol, sym_table->capacity);
    while(sym_table->table[hash_value] 
        && strcmp(symbol, sym_table->table[hash_value]->ast_sym_entry.symbol) != 0) {
            hash_value = (hash_value + 1) % sym_table->capacity;
    }

    // Checks if symbol is in table
    if(sym_table->table[hash_value]) {
        return sym_table->table[hash_value];
    } else {
        return NULL;
    }
}

// Adds new symbol entry to the symbol table
// If replace == 1 --> replaces duplicate entry
// Returns 1 on success, -1 on failure
int add_entry_to_table(symbol_table *sym_table, astnode *sym_entry, int replace) {
    // Checks if table needs to be made larger
    if(2 * sym_table->size > sym_table->capacity && rehash_table(sym_table) == -1) {
        // ERROR
    }

    // Gets symbol hash (Using linear probing)
    int hash_value = hash_symbol(sym_entry->ast_sym_entry.symbol, sym_table->capacity);
    while(sym_table->table[hash_value] 
        && strcmp(sym_entry->ast_sym_entry.symbol, sym_table->table[hash_value]->ast_sym_entry.symbol) != 0) {
            hash_value = (hash_value + 1) % sym_table->capacity;
    }

    // Checks if duplicate found
    if(sym_table->table[hash_value]) {
        // Replaces entry
        if(replace) {
            free(sym_table->table[hash_value]);
            sym_table->table[hash_value] = sym_entry;
        } else {
            // ERROR - Duplicate found
            return -1;
        }
    } else {
        sym_table->table[hash_value] = sym_entry;
        sym_table->size++;
    }

    return 1;
}

// Adds new symbol to specified namespace in innermost scope
// If list is given, adds each entry in list
// If replace == 1 --> replaces duplicate entry
// Returns 1 on (all) success, -1 on (any) failure
int add_entry_to_namespace(int name_space, astnode *sym_entry, int replace) {
    symbol_table *sym_table = get_inner_scope()->sym_tables[name_space];

    // Checks if list
    if(sym_entry->node_type == NODE_LIST_TYPE) {
        int return_val = 1;
        // Loops through each entry
        astnode_list_entry *curr_list_node = &(sym_entry->ast_node_list_head);
        while(curr_list_node != NULL) {
            if(add_entry_to_table(sym_table,curr_list_node->node,replace) == -1) {
                return_val = -1;
            }
            curr_list_node = curr_list_node->next;
        }
        return return_val;
    } else {
        return add_entry_to_table(sym_table,sym_entry,replace);
    }
    
}

// Given symbol table, returns astnode list of table members
astnode *get_table_members(symbol_table *sym_table) {
    astnode *node_list = NULL;

    // Loops through table
    for(int i = 0; i < sym_table->capacity; i++) {
        if(sym_table->table[i]) {
            if(node_list == NULL) {
                node_list = init_node_list(sym_table->table[i]);
            } else {
                add_node_to_list(node_list, sym_table->table[i]);
            }
        }
    }

    return node_list;
}

// Scope Stack Functions
// ----------------------

// Scope stack for program
static scope_stack *program_stack;

// Initializes Scope Stack with File Scope
// Run once at start of parser
void init_scope_stack() {
    // Creates stack
    if((program_stack = malloc(sizeof(scope_stack))) == NULL) {
        // ERROR - unable to create new scope stack
        return;
    }

    program_stack->innermost_scope = NULL;
    program_stack->outermost_scope = NULL;

    // Creates file scope
    create_new_scope(FILE_SCOPE);
}

// Creates new scope and adds to stack
void create_new_scope(int scope_type) {
    // Creates new scope
    scope_entry *new_scope;
    if((new_scope = calloc(1,sizeof(scope_entry))) == NULL) {
        // ERROR - unable to create new scope
        return;
    }

    new_scope->scope = scope_type;
    new_scope->scope_start_file = strdup(filename);
    new_scope->scope_start_line = line_number;

    // Creates symbol tables for each namespace
    for(int i = 0; i < 3; i++) {
        new_scope->sym_tables[i] = create_table();
    }

    // Adds to scope stack
    new_scope->scope_up = program_stack->innermost_scope;
    program_stack->innermost_scope = new_scope;
    if(program_stack->outermost_scope == NULL) {
        program_stack->outermost_scope = new_scope;
    }
}

// Deletes innermost scope
void delete_inner_scope() {
    // Checks for empty stack
    if(program_stack->innermost_scope == NULL) {
        // ERROR - Trying to delete empty stack
    }

    // Frees symbol tables of namespaces
    for(int i = 0; i < 3; i++) {
        free(program_stack->innermost_scope->sym_tables[i]);
    }

    // Updates new inner scope and frees old one
    scope_entry *new_inner_scope = program_stack->innermost_scope->scope_up;
    free(program_stack->innermost_scope);
    program_stack->innermost_scope = new_inner_scope;

    // Checks if table now empty
    if(new_inner_scope == NULL) {
        program_stack->outermost_scope = NULL;
    }
}

// Returns innermost /  current scope
scope_entry *get_inner_scope() {
    return program_stack->innermost_scope;
}

// Returns outermost / file scope
scope_entry *get_global_scope() {
    // Should always be true, but checks anyways
    if(program_stack->outermost_scope->scope == FILE_SCOPE) {
        return program_stack->outermost_scope;
    }

    fprintf(stderr, "ERROR: No global scope found.\n");
    return NULL;
}

// Searches entire scope stack for symbol, inner --> outer
astnode *search_scope_stack(char *symbol, int symbol_namespace) {
    astnode *symbol_node;
    for(scope_entry *curr_scope = program_stack->innermost_scope; curr_scope != NULL; curr_scope = curr_scope->scope_up) {
        // Searches given namespace in current scope
        symbol_node = search_table(curr_scope->sym_tables[symbol_namespace], symbol);

        // Checks if symbol was found
        if(symbol_node != NULL) {
            return symbol_node;
        }
    }

    return NULL;
}

// Printing Functions
// -------------------

char *scope_type_to_string(int scope_type) {
    switch(scope_type) {
        case FILE_SCOPE:
            return "FILE";

        case BLOCK_SCOPE:
            return "BLOCK";
    
        case FUNCTION_SCOPE:
            return "FUNCTION";

        case PROTOTYPE_SCOPE:
            return "PROTOTYPE";

        default:
            return "UNKNOWN";
    }
}