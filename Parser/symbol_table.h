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
    astnode **table;
} symbolTable;

// Scope entry - element of scopeStack linked list
typedef struct scopeEntry {
    int scope;  // Scope of symbol table
    symbolTable *sym_tables[3]; // Symbol table for each namespace (in order of enum above)
    scopeEntry *scope_up; // Next outer scope
} scopeEntry;

// Scopes are stored in a linked-list / stack
typedef struct scopeStack {
    scopeEntry *innermost_scope;
    scopeEntry *outermost_scope;
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
astnode *searchTable(symbolTable *sym_table, char *symbol);

// Adds new symbol entry to the symbol table
// If replace == 1 --> replaces duplicate entry
// Returns 1 on success, -1 on failure
int addTableEntry(symbolTable *sym_table, astnode *sym_entry, int replace);


#endif // SYMBOLTABLE_H