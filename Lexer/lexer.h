/* Andrew Lorber */
/* Compilers - Lexical Analyzer Header File */

#ifndef LEXER_H
#define LEXER_H

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

// ENUM for number size-specifiers
// Named with "_TYPE" to not conflict with keywords
enum num_sizes {
    FLOAT_TYPE = 1,
    DOUBLE_TYPE,
    LONGDOUBLE_TYPE,
    INT_TYPE,
    LONG_TYPE,
    LONGLONG_TYPE
};

// ENUM for sign specifiers
enum sign {
    UNSIGNED_TYPE,
    SIGNED_TYPE
};

// Defines struct to store number info
typedef struct {
    int is_signed; /* 0 = unsigned */
    int size_specifier; /* See enum */ 
    /* Variables to store possible number types */
    long double d_value;
    long long int i_value;
} num_type;

// Defines union to store semantic values
typedef union {
    num_type number;
    char c;
    char *string;
    char *ident;
} YYSTYPE;

YYSTYPE yylval;

#endif // LEXER_H