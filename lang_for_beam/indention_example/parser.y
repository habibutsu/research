statements
    : statements statement
    | statement
    ;

statement
    : selectionstatement END_STATEMENT
    | iterationstatement END_STATEMENT
    | simplestatement
    ;

simplestatement
    : selectionstatement
    | iterationstatement
    ;

selectionstatement
    : IF L_BRACKET conditionalexpression R_BRACKET THEN codeblock
    | IF L_BRACKET conditionalexpression R_BRACKET THEN codeblock ELSE codeblock
    ;

iterationstatement
    : WHILE L_BRACKET conditionalexpression R_BRACKET codeblock
    ;

codeblock
    : INDENT statements DEDENT
    ;