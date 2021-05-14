/* Andrew Lorber */
/* Compilers - Parser */
/* Symbol Table Header File */

#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include "astFunctions.h"

// Structures
// ----------

// Enum of namespaces
enum name_space {
    LABEL_NS = 0,
    TAG_NS,
    OTHER_NS
};

// Enum of scopes
enum scopes {
    FILE_SCOPE = 1,
    BLOCK_SCOPE,
    FUNCTION_SCOPE,
    PROTOTYPE_SCOPE
};

// Symbol Table Struct
// Implemented as hash table
typedef struct symbol_table {
    int size;   // Current number of elements
    int capacity; 
    struct astnode **table;
} symbol_table;

// Scope entry - element of scopeStack linked list
typedef struct scope_entry {
    int scope;  // Scope of symbol table
    struct symbol_table *sym_tables[3]; // Symbol table for each namespace (in order of enum above)
    struct scope_entry *scope_up; // Next outer scope

    // For debugging
    char *scope_start_file;
    int scope_start_line;
} scope_entry;

// Scopes are stored in a linked-list / stack
typedef struct scope_stack {
    struct scope_entry *innermost_scope;
    struct scope_entry *outermost_scope;
} scope_stack;

// Helper Functions
// -----------------

// Returns hashed index of symbol
int hash_symbol(char *symbol, int table_capacity);

// Resizes table and rehashes elements in table to new table
int rehash_table(symbol_table *sym_table);

// Returns next prime in list to use as size
int get_prime(int curr_size);

// Symbol Table Functions
// -----------------------

// Creates a new symbol table
symbol_table *create_table();

// Destroys given symbol table
void destroy_table(symbol_table *sym_table);

// Searches symbol table for symbol
struct astnode *search_table(symbol_table *sym_table, char *symbol);

// Adds new symbol entry to the symbol table
// If replace == 1 --> replaces duplicate entry
// Returns 1 on success, -1 on failure
int add_entry_to_table(symbol_table *sym_table, struct astnode *sym_entry, int replace);

// Adds new symbol to specified namespace in innermost scope
// If list is given, adds each entry in list
// If replace == 1 --> replaces duplicate entry
// Returns 1 on (all) success, -1 on (any) failure
int add_entry_to_namespace(int name_space, struct astnode *sym_entry, int replace);

// Given symbol table, returns astnode list of table members
struct astnode *get_table_members(symbol_table *sym_table);

// Scope Stack Functions
// ----------------------

// Initializes Scope Stack with File Scope
// Run once at start of parser
void init_scope_stack();

// Creates new scope and adds to stack
void create_new_scope(int scope_type);

// Deletes innermost scope
void delete_inner_scope();

// Returns innermost / current scope
scope_entry *get_inner_scope();

// Returns outermost / file scope
scope_entry *get_global_scope();

// Searches entire scope stack for symbol, inner --> outer
struct astnode *search_scope_stack(char *symbol, int symbol_namespace);

// Printing Functions
// -------------------

char *scope_type_to_string(int scope_type);

#endif // SYMBOLTABLE_H