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
typedef struct symbolTable {
    int size;   // Current number of elements
    int capacity; 
    struct astnode **table;
} symbolTable;

// Scope entry - element of scopeStack linked list
typedef struct scopeEntry {
    int scope;  // Scope of symbol table
    struct symbolTable *sym_tables[3]; // Symbol table for each namespace (in order of enum above)
    struct scopeEntry *scope_up; // Next outer scope
} scopeEntry;

// Scopes are stored in a linked-list / stack
typedef struct scopeStack {
    struct scopeEntry *innermost_scope;
    struct scopeEntry *outermost_scope;
} scopeStack;

// Helper Functions
// -----------------

// Returns hashed index of symbol
int hashSymbol(char *symbol, int table_capacity);

// Resizes table and rehashes elements in table to new table
int rehashTable(symbolTable *sym_table);

// Returns next prime in list to use as size
int getPrime(int curr_size);

// Symbol Table Functions
// -----------------------

// Creates a new symbol table
symbolTable *createTable();

// Destroys given symbol table
void destroyTable(symbolTable *sym_table);

// Searches symbol table for symbol
struct astnode *searchTable(symbolTable *sym_table, char *symbol);

// Adds new symbol entry to the symbol table
// If replace == 1 --> replaces duplicate entry
// Returns 1 on success, -1 on failure
int addEntryToTable(symbolTable *sym_table, struct astnode *sym_entry, int replace);

// Adds new symbol to specified namespace in innermost scope
// If list is given, adds each entry in list
// If replace == 1 --> replaces duplicate entry
// Returns 1 on (all) success, -1 on (any) failure
int addEntryToNamespace(int name_space, struct astnode *sym_entry, int replace);

// Scope Stack Functions
// ----------------------

// Initializes Scope Stack with File Scope
// Run once at start of parser
void initScopeStack();

// Creates new scope and adds to stack
void createNewScope(int scope_type);

// Deletes innermost scope
void deleteInnerScope();

// Returns innermost /  current scope
scopeEntry *getInnerScope();

// Searches entire scope stack for symbol, inner --> outer
struct astnode *searchScopeStack(char *symbol, int symbol_namespace);

#endif // SYMBOLTABLE_H