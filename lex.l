%{
    #include <string.h>
    #include "parser.tab.h"
%}
%s IN_COMMENT

%%
"int"                                           { return INT; }
"double"                                        { return DOUBLE; }
"beg"                                           { return BEG; }
"end"                                           { return END; }
"if"                                            { return IF; }
[_a-zA-Z][_a-zA-Z0-9]*                          { yylval.str = strdup(yytext); return ID; }
[ \t\n]*                                        ;
"/*"                                            BEGIN(IN_COMMENT); 
<IN_COMMENT>{
    "*/"                                        BEGIN(0);
    [^*]*                                       ;   
}
-?[0-9]+                                        { yylval.ival = atoi(yytext); return INTEGER_CONSTANT; }
-?[0-9]+"."+[0-9]+                              { char* temp; yylval.dval = strtod(yytext, &temp); return DOUBLE_CONSTANT; }
","                                             { return COMMA; }
";"                                             { return SEMICOLON; }
.                                               { return yytext[0]; }
%%
