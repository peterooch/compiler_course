%{
    int lineno=0;
%}
%x comment
%%

if 		                        {return IF;}
else 	                        {return ELSE;}
while 	                        {return WHILE;}
var 	                        {return VAR;}
func 	                        {return FUNC;}
proc 	                        {return PROC;}
return 	                        {return RETURN;}

bool 	                        {yylval = mkleaf(tBOOL_N); return BOOL;}
char 	                        {yylval = mkleaf(tCHAR_N); return CHAR;}
int 	                        {yylval = mkleaf(tINT_N); return INT;}
real 	                        {yylval = mkleaf(tREAL_N); return REAL;}
string 	                        {yylval = mkleaf(tSTRING_N); return STRING;}
int\*	                        {yylval = mkleaf(tINTPTR_N); return INTPTR;}
char\*	                        {yylval = mkleaf(tCHARPTR_N); return CHARPTR;}
real\*	                        {yylval = mkleaf(tREALPTR_N); return REALPTR;}

"="		                        {yylval = mkleaf(ASS_N); return ASS;}
"&&" 	                        {yylval = mkleaf(LOGICAL_N); return AND;}
"||"	                        {yylval = mkleaf(LOGICAL_N); return OR;}
"=="	                        {yylval = mkleaf(COMP_N); return EQ;}
"!="	                        {yylval = mkleaf(COMP_N); return NE;}
">"		                        {yylval = mkleaf(COMP_N); return GT;}
">=" 	                        {yylval = mkleaf(COMP_N); return GE;}
"<"		                        {yylval = mkleaf(COMP_N); return LT;}
"<="	                        {yylval = mkleaf(COMP_N); return LE;}
"*"		                        {yylval = mkleaf(ARITH_N); return MUL;}
"/"  	                        {yylval = mkleaf(ARITH_N); return DIV;}
"+"		                        {yylval = mkleaf(ARITH_N); return PLUS;}
"-"		                        {yylval = mkleaf(ARITH_N); return MINUS;}
"!"		                        {yylval = mkleaf(NOT_N); return NOT;}
"&"		                        {yylval = mkleaf(ADDRESS_N); return ADDRESSOF;}
"^"		                        {yylval = mkleaf(DEREF_N); return DEREF;}

null 	                        {yylval = mkleaf_str(NULLPTR_N, yytext); return NULLPTR;}
true	                        {yylval = mkleaf_str(BOOLLITERAL_N, yytext); return TRUE;}
false	                        {yylval = mkleaf_str(BOOLLITERAL_N, yytext); return FALSE;}
[a-zA-Z][_a-zA-Z0-9]*	        {yylval = mkleaf_str(IDENTIFIER_N, yytext); return ID;}
\'.\'		                    {yylval = mkleaf_str(CHARLITERAL_N, yytext); return CHARLITERAL;}
0|[1-9][0-9]*	                {yylval = mkleaf_str(INTLITERAL_N, yytext); return DECLITERAL;}
0[Xx][0-9A-F]+		            {yylval = mkleaf_str(INTLITERAL_N, yytext); return HEXLITERAL;}
[0-9]+\.[0-9]*	                {yylval = mkleaf_str(REALLITERAL_N, yytext); return REALLITERAL;} 
[0-9]+\.[0-9]*[\-\+][Ee][0-9]+	{yylval = mkleaf_str(REALLITERAL_N, yytext); return REALLITERAL;} 
\".*\"		                    {yylval = mkleaf_str(STRINGLITERAL_N, yytext); return STRINGLITERAL;}
[ \t]+                          ;
"/%" BEGIN                      comment; 
<comment>[^\n]                  ;
<comment>\n                     yylineno++;
<comment>"%/" BEGIN             0;
\n                              {lineno++;}
.                               {return yytext[0];}
%%
