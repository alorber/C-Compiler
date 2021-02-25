/* Andrew Lorber */
/* Compilers - Parser Header File*/

#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "numType.h"
#include "astFunctions.h"

// Prints number of indents given
// Helper function for print_ast
void print_indents(int num_indents);

// Prints the AST
void print_ast(astnode *node, int num_indent);

#endif // PARSER_H