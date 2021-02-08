/* Andrew Lorber & Henry Son */
/* Compilers - Lexical Analyzer */
/* Helper Functions */

#include "lexerFunctions.h"
#define STRING_SIZE 4096

// Buffer to hold string being built
char string_buffer[STRING_SIZE];
int buf_pos = 0;

// Creates / begins string
void create_string() {
	// Clears buffer and sets pointer position to beginning
	memset(string_buffer, 0, sizeof(string_buffer));
	buf_pos = 0;
}

// Adds chars to string
void append_string(char *text) {
	string_buffer[buf_pos++] = text[0];
}

// Adds escape characters to string
// base: 0 -> simple escape
//		 8 -> Octal escape
//		 16 -> Hex escape
void append_escape(char* text, int base) {
	// Checks escape type
	if(base == 0) {
		string_buffer[buf_pos++] = '\\';
		string_buffer[buf_pos++] = text[1];
	} else if(base == 8) {
		int oct = (int)strtol(text+1, NULL, 8);
		string_buffer[buf_pos++] = 255 < oct ? 255 : oct;
	} else if(base == 16) {
		int hex = (int)strtol(text+2, NULL, 16);
		string_buffer[buf_pos++] = 255 < hex ? 255 : hex;
	}
	// Should never have incorrect input
}

// Completes String
// If char, only saves first character in buffer
char* complete_string(int is_char) {
	if(is_char) {
		// If escape character, need first two chars
		if(string_buffer[0] == '\\') {
			// Adds null terminator
			string_buffer[2] = '\0';
			// Adds zero after null to mark end of char
			string_buffer[3] = 0;
		} else {
			string_buffer[1] = '\0';
			string_buffer[2] = 0;
		}
	} else {
		string_buffer[buf_pos++] = '\0';
	}

	// Returns pointer to copy of string
	return strdup(string_buffer);
}

// Prints String
void print_string(char *string) {
	// Prints until reaches end of buffer or reaches null before zero'ed out buffer
	for(int i = 0; i < STRING_SIZE && (string[i] != '\0' || string[i+1] != 0); i++) {
		// Only prints printable characters
		if(isprint(string[i])) {
			printf("%c", string[i]);
		} else {
			printf("\\%03o", (unsigned char)string[i]);
		}
	}
}

// Prints error messages
void print_error(char *filename, int line_num, char* text, char* message) {
	fprintf(stderr, "%s: line %d: %s - %s\n", filename, line_num, text, message);
}

// Prints string representation of keyword
void print_keyword(int keyword) {
	// List of keywords as strings
	char* keyword_strings[] = {"IDENT", "CHARLIT", "STRING", "NUMBER", "INDSEL", 
		"PLUSPLUS", "MINUSMINUS", "SHL", "SHR", "LTEQ", "GTEQ", "EQEQ", "NOTEQ",
		"LOGAND", "LOGOR", "ELLIPSIS", "TIMESEQ", "DIVEQ", "MODEQ", "PLUSEQ",
		"MINUSEQ", "SHLEQ", "SHREQ", "ANDEQ", "OREQ", "XOREQ", "AUTO", "BREAK",
		"CASE", "CHAR", "CONST", "CONTINUE", "DEFAULT", "DO", "DOUBLE", "ELSE",
		"ENUM", "EXTERN", "FLOAT", "FOR", "GOTO", "IF", "INLINE", "INT", "LONG",
		"REGISTER", "RESTRICT", "RETURN", "SHORT", "SIGNED", "SIZEOF", "STATIC",
		"STRUCT", "SWITCH", "TYPEDEF", "UNION", "UNSIGNED", "VOID", "VOLATILE",
		"WHILE", "_BOOL", "_COMPLEX", "_IMAGINARY"};
	
	// Subtracts keyword value from value of IDENT to get index in list
	printf("%s\n",keyword_strings[keyword - 257]);
}