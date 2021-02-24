/* Andrew Lorber */
/* Compilers - Lexical Analyzer */
/* Helper Functions Header File */

#ifndef LEXERFUNCTIONS_H
#define LEXERFUNCTIONS_H

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

// Creates / begins string
void create_string();

// Adds chars to string
void append_string(char *text);

// Adds escape characters to string
void append_escape(char* text, int base);

// Completes string
char* complete_string(int is_char);

// Prints string
void print_string(char *string);

// Prints error messages
void print_error(char *filename, int line_num, char* text, char* message);

// Prints string representation of keyword
void print_keyword(int keyword);

#endif // LEXERFUNCTIONS_H