
%token DEF_TOKEN

%token ARROW_TOKEN
%token COLON_TOKEN
%token LPAREN_TOKEN  RPAREN_TOKEN
%token COMMA_TOKEN

%token ID_TOKEN
%token TYPE_TOKEN


funcdef
    : DEF_TOKEN identifier parameter_list_declaration COLON_TOKEN codeblock
    | DEF_TOKEN identifier parameter_list_declaration ARROW_TOKEN type_specifier COLON_TOKEN codeblock
    ;

type_specifier
    : TYPE_TOKEN
    ;

identifier
    : ID_TOKEN
    ;

parameter_list_declaration
    : LPAREN_TOKEN RPAREN_TOKEN
    | LPAREN_TOKEN parameter_list RPAREN_TOKEN
    ;

parameter_list
    : parameter
    | parameter_list COMMA_TOKEN parameter
    ;

parameter
    : variable
    | variable COLON_TOKEN type_specifier
    ;

codeblock
    : INDENT statements DEDENT
    ;

statements
    : statements statement
    | statement
    ;

/*
suite_stmt
    : NEWLINE INDENT stmt
    | suite_stmt NEWLINE INDENT stmt
    ;

stmt
    : simple_stmt
    | compound_stmt
    ;

simple_stmt
    : small_stmt NEWLINE
    ;

small_stmt
    : pass_stmt
    ;

compound_stmt
    : funcdef
    ;
*/