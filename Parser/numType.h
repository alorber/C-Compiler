/* Andrew Lorber */
/* Compilers */
/* Header file for needed data-types from lexer */

#ifndef NUMTYPE_H
#define NUMTYPE_H

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
typedef struct num_type {
    int is_signed; /* 0 = unsigned */
    int size_specifier; /* See enum */ 
    /* Variables to store possible number types */
    long double d_value;
    long long int i_value;
} num_type;

#endif // NUMTYPE_H