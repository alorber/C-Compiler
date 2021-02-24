/* Andrew Lorber */
/* Compilers - Lexical Analyzer */

/* ------------- */
/*  Definitions  */
/* ------------- */

%option noyywrap

%{
#include "tokens-manual.h"
#include "lexer.h"
#include "lexerFunctions.h"

extern YYSTYPE yylval;

char filename[256] = "<stdin>";
int line_number = 1;
%}

/* STATES */
%x FILE_INFO
%x CHARLIT
%x STRING
%x INT_NUM
%x FLOAT_NUM
%x COMMENT

/* NUMERIC CONSTANTS */
DEC     [1-9]+[0-9]*
OCT     0[0-7]*
HEX     0[xX][a-fA-F0-9]+
FLO     ([0-9]*\.[0-9]+)|([0-9]+\.[0-9]*)
EXP     [eE][+-]?{DEC}+
P       [pP][+-]?{DEC}+

%%
 /* ------- */
 /*  RULES  */
 /* ------- */

 /* FILE INFORMATION */
#                                   {BEGIN FILE_INFO;}
<FILE_INFO>[0-9]+                   { /* Get line number in base 10 */
                                     line_number = (int)strtol(yytext, NULL, 10);}

<FILE_INFO>\"(\\.|[^\\\"])*\"       { /* copy file name */
                                    strncpy(filename, yytext + 1, strlen(yytext) - 2);
                                    filename[strlen(yytext) - 2] = '\0';}

<FILE_INFO>\n                       {BEGIN INITIAL;}

<FILE_INFO>.                        {/* Ignores everything not listed */;}

 /* WHITE SPACES */
[\t\r\v\f ]+        {;}
[\n]                {line_number++;}

 /* COMMENTS */
"//".*\n            {line_number++;}
"/*"                {BEGIN COMMENT;}
<COMMENT>"*/"       {BEGIN INITIAL;}
<COMMENT>.+         {/* Ignores comment */;}
<COMMENT>\n         {line_number++;}

 /* NUMBERS */
{DEC}           {yylval.number.i_value = strtoll(yytext, NULL, 10);
                 yylval.number.is_signed = SIGNED_TYPE;
                 yylval.number.size_specifier = INT_TYPE;
                 BEGIN INT_NUM;}
{OCT}           {yylval.number.i_value = strtoll(yytext, NULL, 8);
                 yylval.number.is_signed = SIGNED_TYPE;
                 yylval.number.size_specifier = INT_TYPE;
                 BEGIN INT_NUM;}
{HEX}           {yylval.number.i_value = strtoll(yytext, NULL, 16);
                 yylval.number.is_signed = SIGNED_TYPE;
                 yylval.number.size_specifier = INT_TYPE;
                 BEGIN INT_NUM;}
{FLO}           |
0?{DEC}{EXP}    |
{FLO}{EXP}      |
{HEX}{P}        {yylval.number.d_value = strtod(yytext, NULL);
                 yylval.number.is_signed = SIGNED_TYPE;
                 yylval.number.size_specifier = DOUBLE_TYPE;
                 BEGIN FLOAT_NUM;}

<INT_NUM>[uU]       {yylval.number.is_signed = UNSIGNED_TYPE;}
<INT_NUM>(ll|LL)    {yylval.number.size_specifier = LONGLONG_TYPE;}
<INT_NUM>[lL]       {yylval.number.size_specifier = LONG_TYPE;}
<FLOAT_NUM>[fF]     {yylval.number.size_specifier = FLOAT_TYPE;}
<FLOAT_NUM>[lL]     {yylval.number.size_specifier = LONGDOUBLE_TYPE;}
<INT_NUM,FLOAT_NUM>[\t\r\v\f ]+     {BEGIN INITIAL; 
                                     return NUMBER;}
<INT_NUM,FLOAT_NUM>[\n]             {line_number++; 
                                     BEGIN INITIAL; 
                                     return NUMBER;}
<INT_NUM,FLOAT_NUM>.                {BEGIN INITIAL;
                                     /* Pushes back current character */
                                     yyless(yyleng - 1);
                                     return NUMBER;}

 /* OPERATORS */
[!%&\|\+\-\*\/\^=\<\>\(\)\[\]\{\}\~\.\?,;:]     {return yytext[0];}

"->"            {return INDSEL;}
"++"            {return PLUSPLUS;}
"--"            {return MINUSMINUS;}
"<<"            {return SHL;}
">>"            {return SHR;}
"<="            {return LTEQ;}
">="            {return GTEQ;}
"=="            {return EQEQ;}
"!="            {return NOTEQ;}
"&&"            {return LOGAND;}
"||"            {return LOGOR;}
"..."           {return ELLIPSIS;}
"*="            {return TIMESEQ;}
"/="            {return DIVEQ;}
"%="            {return MODEQ;}
"+="            {return PLUSEQ;}
"-="            {return MINUSEQ;}
"<<="           {return SHLEQ;}
">>="           {return SHREQ;}
"&="            {return ANDEQ;}
"|="            {return OREQ;}
"^="            {return XOREQ;}

 /* KEYWORDS */
auto            {return AUTO;}
break           {return BREAK;}
case            {return CASE;}
char            {return CHAR;}
const           {return CONST;}
continue        {return CONTINUE;}
default         {return DEFAULT;}
do              {return DO;}
double          {return DOUBLE;}
else            {return ELSE;}
enum            {return ENUM;}
extern          {return EXTERN;}
float           {return FLOAT;}
for             {return FOR;}
goto            {return GOTO;}
if              {return IF;}
inline          {return INLINE;}
int             {return INT;}
long            {return LONG;}
register        {return REGISTER;}
restrict        {return RESTRICT;}
return          {return RETURN;}
short           {return SHORT;}
signed          {return SIGNED;}
sizeof          {return SIZEOF;}
static          {return STATIC;}
struct          {return STRUCT;}
switch          {return SWITCH;}
typedef         {return TYPEDEF;}
union           {return UNION;}
unsigned        {return UNSIGNED;}
void            {return VOID;}
volatile        {return VOLATILE;}
while           {return WHILE;}
_Bool           {return _BOOL;}
_Complex        {return _COMPLEX;}
_Imaginary      {return _IMAGINARY;}

 /* IDENTIFIERS */
[a-zA-Z_][a-zA-Z0-9_]*      {yylval.ident = strdup(yytext); 
                             return IDENT;}

 /* CHARACTER LITERALS */
[LuU]?\'                    {BEGIN CHARLIT;
                             create_string();}
<CHARLIT>[^\'\\\n]          {append_string(yytext);}
<CHARLIT>\'                 {BEGIN INITIAL;
                             yylval.string = complete_string(1);
                             return CHARLIT;}

 /* STRINGS */
[LuU]?\"                    {BEGIN STRING;
                             create_string();}
<STRING>[^\"\\\n]           {append_string(yytext);}
<STRING>\"                  {BEGIN INITIAL;
                             yylval.string = complete_string(0);
                             return STRING;}

 /* ESCAPE SEQUENCES (STRING & CHAR) */

 /* Simple */
<CHARLIT,STRING>\\[abfrntv0\'\"\\]      {append_escape(yytext,0);}

 /* Octal */
<CHARLIT,STRING>\\[0-7]{1,3}            {append_escape(yytext,8);}

 /* Hex */
<CHARLIT,STRING>\\x[a-fA-F0-9]+         {append_escape(yytext,16);}

 /* ERRORS */
<CHARLIT,STRING>\n      {print_error(filename,line_number,yytext,"Unterminated string/char.");
                         line_number++;}
<CHARLIT,STRING>.       {print_error(filename,line_number,yytext,"Unknown character.");}
.                       {print_error(filename,line_number,yytext,"Unknown character.");}

%%
/* ----------- */
/*  USER CODE  */
/* ----------- */

int main() {
    int token;

    while(token = yylex()) {
        // Prints file info 
        printf("%s\t%d\t", filename, line_number);

        // Check type of token
        switch(token) {
            case IDENT:
                    printf("IDENT\t%s\n", yylval.ident);
                    break;
            
            case NUMBER:
                    printf("NUMBER\t");
                    
                    // Checks type of number for printing
                    int num_type = yylval.number.size_specifier;
                    // Int
                    if(num_type == INT_TYPE || num_type == LONG_TYPE || num_type == LONGLONG_TYPE){
                        printf("INTEGER\t");

                        // Prints value
                        printf("%lli\t", yylval.number.i_value);

                        // Checks if unsigned
                        if(yylval.number.is_signed == UNSIGNED_TYPE) {
                            printf("UNSIGNED,");
                        }

                        // Prints type
                        switch(num_type) {
                            case INT_TYPE:
                                    printf("INT\n");
                                    break;
                            case LONG_TYPE:
                                    printf("LONG\n");
                                    break;
                            case LONGLONG_TYPE:
                                    printf("LONGLONG\n");
                                    break;
                        }
                    }
                    // Real
                    else {
                        printf("REAL\t");

                        // Prints value
                        printf("%Lf\t", yylval.number.d_value);

                        // Prints type
                        switch(num_type) {
                            case FLOAT_TYPE:
                                    printf("FLOAT\n");
                                    break;
                            case DOUBLE_TYPE:
                                    printf("DOUBLE\n");
                                    break;
                            case LONGDOUBLE_TYPE:
                                    printf("LONGDOUBLE\n");
                                    break;
                        }
                    }

                    break;
            
            case CHARLIT:
                    printf("CHARLIT\t");
                    print_string(yylval.string);
                    printf("\n");
                    break;

            case STRING:
                    printf("STRING\t");
                    print_string(yylval.string);
                    printf("\n");
                    break;

            // KEYWORDS
            default:
                    // Checks if ASCII
                    if(token < 256) {
                        printf("%c\n", token);
                    } else {
                        print_keyword(token);
                    }
        }
    }
}


