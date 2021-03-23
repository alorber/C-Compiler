/* Andrew Lorber */
/* Compilers - Parser */
/* Symbol Table File */

#include "symbol_table.h"

// Helper Functions
// -----------------

// Returns hashed index of symbol
int hashSymbol(char *symbol, int table_capacity) {
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
int rehashTable(symbolTable *sym_table) {
    // Updates capacity
    int old_capacity = sym_table->capacity;
    if((sym_table->capacity = getPrime(old_capacity)) == -1) {
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
            addEntryToTable(sym_table, temp_table[i], 0);
        }
    }

    free(temp_table);

    return 1;
}

// Returns next prime in list to use as size
int getPrime(int curr_size) {
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
symbolTable *createTable() {
    // Creates symbol table struct
    symbolTable *sym_table;
    if((sym_table = calloc(1,sizeof(symbolTable))) == NULL) {
        // ERROR
    }

    sym_table->size = 0;
    sym_table->capacity = getPrime(0);

    // Creates symbol table array
    if((sym_table->table = (astnode **)calloc(sym_table->capacity,sizeof(astnode))) == NULL) {
        // ERROR
    }

    return sym_table;
}

// Destroys given symbol table
void destroyTable(symbolTable *sym_table) {
    for(int i = 0; i < sym_table->capacity; i++) {
        if(sym_table->table[i]) {
            free(sym_table->table[i]);
        }
    }
    free(sym_table->table);
    free(sym_table);
}

// Searches symbol table for symbol
astnode *searchTable(symbolTable *sym_table, char *symbol) {
    // Gets symbol hash (Using linear probing)
    int hash_value = hashSymbol(symbol, sym_table->capacity);
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
int addEntryToTable(symbolTable *sym_table, astnode *sym_entry, int replace) {
    // Checks if table needs to be made larger
    if(2 * sym_table->size > sym_table->capacity && rehashTable(sym_table) == -1) {
        // ERROR
    }

    // Gets symbol hash (Using linear probing)
    int hash_value = hashSymbol(sym_entry->ast_sym_entry.symbol, sym_table->capacity);
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
// If replace == 1 --> replaces duplicate entry
// Returns 1 on success, -1 on failure
int addEntryToNamespace(int name_space, astnode *sym_entry, int replace) {
    symbolTable *sym_table = getInnerScope()->sym_tables[name_space];
    return addEntryToTable(sym_table,sym_entry,replace);
}

// Scope Stack Functions
// ----------------------

// Scope stack for program
static scopeStack *scope_stack;

// Initializes Scope Stack with File Scope
// Run once at start of parser
void initScopeStack() {
    // Creates stack
    if((scope_stack = calloc(1,sizeof(scopeStack))) == NULL) {
        // ERROR - unable to create new scope stack
        return;
    } else {
        // Creates file scope
        createNewScope(FILE_SCOPE);
    }

    scope_stack->innermost_scope = NULL;
    scope_stack->outermost_scope = NULL;
}

// Creates new scope and adds to stack
void createNewScope(int scope_type) {
    // Creates new scope
    scopeEntry *new_scope;
    if((new_scope = calloc(1,sizeof(scopeEntry))) == NULL) {
        // ERROR - unable to create new scope
        return;
    }

    new_scope->scope = scope_type;

    // Creates symbol tables for each namespace
    for(int i = 0; i < 3; i++) {
        new_scope->sym_tables[i] = createTable();
    }

    // Adds to scope stack
    new_scope->scope_up = scope_stack->innermost_scope;
    scope_stack->innermost_scope = new_scope;
    if(scope_stack->outermost_scope == NULL) {
        scope_stack->outermost_scope = new_scope;
    }
}

// Deletes innermost scope
void deleteInnerScope() {
    // Checks for empty stack
    if(scope_stack->innermost_scope == NULL) {
        // ERROR - Trying to delete empty stack
    }

    // Frees symbol tables of namespaces
    for(int i = 0; i < 3; i++) {
        free(scope_stack->innermost_scope->sym_tables[i]);
    }

    // Updates new inner scope and frees old one
    scopeEntry *new_inner_scope = scope_stack->innermost_scope->scope_up;
    free(scope_stack->innermost_scope);
    scope_stack->innermost_scope = new_inner_scope;

    // Checks if table now empty
    if(new_inner_scope == NULL) {
        scope_stack->outermost_scope = NULL;
    }
}

// Returns innermost /  current scope
scopeEntry *getInnerScope() {
    return scope_stack->innermost_scope;
}

// Searches entire scope stack for symbol, inner --> outer
astnode *searchScopeStack(char *symbol, int symbol_namespace) {
    astnode *symbol_node;
    for(scopeEntry *curr_scope = scope_stack->innermost_scope; curr_scope != NULL; curr_scope = curr_scope->scope_up) {
        // Searches given namespace in current scope
        symbol_node = searchTable(curr_scope->sym_tables[symbol_namespace], symbol);

        // Checks if symbol was found
        if(symbol_node != NULL) {
            return symbol_node;
        }
    }

    return NULL;
}

