^\n         {}
\n[ ]*      { 
                if (yyleng == 1)
                {
                    if (indent.size > 0)
                    {
                        pop(&indent);
                        if (peek(&indent) > yyleng && indent.size != 0)
                        yyless(0);
                        return(DEDENT);
                    }
                    return(END_STATEMENT);
                }
                else
                {
                    if (peek(&indent) < yyleng || indent.size == 0)
                    {
                        push(&indent, yyleng);
                        return (INDENT);
                    }
                    else if (peek(&indent) > yyleng && indent.size != 0)
                    {
                        pop(&indent);
                        if (peek(&indent) > yyleng && indent.size != 0)
                        yyless(0);
                        return (DEDENT);
                    }
                    else
                    {
                        return(END_STATEMENT);
                    }
                }
            }

<<EOF>>     {
                if (indent.size > 0)
                {
                    pop(&indent);
                    return(DEDENT);
                }
                yyterminate();
            }