%{
    int lineno=1;
%}
%%
bool 	{yylval = mkleaf(yytext); return BOOL;}
char 	{yylval = mkleaf(yytext); return CHAR;}
int 	{yylval = mkleaf(yytext); return INT;}
real 	{yylval = mkleaf(yytext); return REAL;}
string 	{yylval = mkleaf(yytext); return STRING;}
if 		{yylval = mkleaf(yytext); return IF;}
else 	{yylval = mkleaf(yytext); return ELSE;}
while 	{yylval = mkleaf(yytext); return WHILE;}
var 	{yylval = mkleaf(yytext); return VAR;}
func 	{yylval = mkleaf(yytext); return FUNC;}
proc 	{yylval = mkleaf(yytext); return PROC;}
return 	{yylval = mkleaf(yytext); return RETURN;}
null 	{yylval = mkleaf(yytext); return NULLPTR;}

int\*	{yylval = mkleaf(yytext); return INTPTR;}
char\*	{yylval = mkleaf(yytext); return CHARPTR;}
real\*	{yylval = mkleaf(yytext); return REALPTR;}

"&&" 	{yylval = mkleaf(yytext); return AND;}
"/"  	{yylval = mkleaf(yytext); return DIV;}
"="		{yylval = mkleaf(yytext); return ASS;}
"=="	{yylval = mkleaf(yytext); return EQ;}
">"		{yylval = mkleaf(yytext); return GT;}
">=" 	{yylval = mkleaf(yytext); return GE;}
"<"		{yylval = mkleaf(yytext); return LT;}
"<="	{yylval = mkleaf(yytext); return LE;}
"-"		{yylval = mkleaf(yytext); return MINUS;}
"!"		{yylval = mkleaf(yytext); return NOT;}
"!="	{yylval = mkleaf(yytext); return NE;}
"||"	{yylval = mkleaf(yytext); return OR;}
"+"		{yylval = mkleaf(yytext); return PLUS;}
"*"		{yylval = mkleaf(yytext); return MUL;}
"&"		{yylval = mkleaf(yytext); return ADDRESSOF;}
"^"		{yylval = mkleaf(yytext); return DEREF;}


true	                        {yylval = mkleaf(yytext); return TRUE;}
false	                        {yylval = mkleaf(yytext); return FALSE;}
[a-zA-Z][_a-zA-Z0-9]*	        {yylval = mkleaf(yytext); return ID;}
\'.\'		                    {yylval = mkleaf(yytext); return CHARLITERAL;}
0|[1-9][0-9]*	                {yylval = mkleaf(yytext); return DECLITERAL;}
0[Xx][0-9A-F]+		            {yylval = mkleaf(yytext); return HEXLITERAL;}
[0-9]+\.[0-9]*	                {yylval = mkleaf(yytext); return REALLITERAL;} 
[0-9]+\.[0-9]*[\-\+][Ee][0-9]+	{yylval = mkleaf(yytext); return REALLITERAL;} 
\".*\"		                    {yylval = mkleaf(yytext); return STRINGLITERAL;}
[ \t]+                          ;
\/%.*%\/                        ;    
\n                              {lineno++;}
.                               {return yytext[0];}
%%
