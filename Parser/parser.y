/* Andrew Lorber & Henry Son */
/* Compilers - Parser */

/* ------------- */
/*  Definitions  */
/* ------------- */

%{


%}

%union {

}

/* Tokens */
%token IDENT CHARLIT STRING NUMBER INDSEL PLUSPLUS MINUSMINUS SHL SHR
%token LTEQ GTEQ EQEQ NOTEQ LOGAND LOGOR ELLIPSIS TIMESEQ DIVEQ MODEQ
%token PLUSEQ MINUSEQ SHLEQ SHREQ ANDEQ OREQ XOREQ AUTO BREAK CASE
%token CHAR CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN FLOAT
%token FOR GOTO IF INLINE INT LONG REGISTER RESTRICT RETURN SHORT
%token SIGNED SIZEOF STATIC STRUCT SWITCH TYPEDEF UNION UNSIGNED
%token VOID VOLATILE WHILE _BOOL _COMPLEX _IMAGINARY

/* Types */


/* Operator Precedence & Associativity */
/* From cpp website */
%left ','
%right '=' PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ SHLEQ SHREQ ANDEQ XOREQ OREQ
%right '?' ":"
%left LOGOR
%left LOGAND
%left '|'
%left '^'
%left '&'
%left EQEQ NOTEQ
%left '<' '>' LTEQ GTEQ
%left SHL SHR
%left '+' '-'
%left '*' '/' '%'
%right PLUSPLUS MINUSMINUS /* prefix */ '!' '~' /* +a, -a, &a, a* */
%left '(' ')' '[' ']' /* .a, ->a */
%left IF
%left ELSE

%%
/* ------- */
/*  RULES  */
/* ------- */



%%
/* ----------- */
/*  USER CODE  */
/* ----------- */