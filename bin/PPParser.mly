%token <string> TOKEN
%token <string * char> STR // ctx, delim
%token <int> WS
%token LPAREN "("
%token RPAREN ")"
%token COMMA ","
%token NEWLINE
%token EOF

%token Kw_defined // defined(SYMBOL) will be expanded to 1 or 0 respecting to according macro symbol.

/* Following tokens are parser specified.
* Since each preprocessing instructs has its own syntax, 
* there will be multiple independent starting symbols
* for each commands will parameters.
* 
* These tokens will be in blocks followed a C_ prefix token referring to corresponding instructs.
**/

%token C_unix_exec  // #!, ignore whole line
%token B_unix_exec  // its body, incluing Endline

%token C_define  // #define
// todo

%token C_elif  // #elif, no param
%token C_else  // #else, no param

%token C_if  // #if, <if> ::= <if-body> <endline>
%token B_if  // <if-body> ::= <bool-expr>

%token C_ifdef  // #ifdef
%token C_ifndef  // #ifndef

%token C_include  // #include

%token C_line  // #line

%token C_pragma  // #pragma
%token C_undef  // #undef

%token C_warning  // #warning
%token C_error  // #error

%start <unit> preprocess_unit

%%

preprocess_unit:
    | text_line
    | prep_line
    {}

prep_line:
    prep_line_ctx; NEWLINE;
    {}

prep_line_ctx:
    | i_define
    | i_elif
    | i_else
    | i_error
    | i_if
    | i_ifdef
    | i_ifndef
    | i_include
    | i_line
    | i_pragma
    | i_undef
    | i_warning
    {}

i_define:
    | C_define; param = p_define; body = p_body;
    {
        
    }


