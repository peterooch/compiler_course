%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

typedef struct node
{
    char* token;
    int nchildren;
    struct node** children;
} node;
struct node* mkleaf(const char* token);
struct node* mknode(const char* token, int count, ...);
void printtree(struct node* tree, int spacing);
#define YYSTYPE struct node*
%}
/* Keywords Lexemes */
%token BOOL
%token CHAR
%token INT
%token REAL
%token STRING
%token IF
%token ELSE
%token WHILE
%token VAR
%token FUNC
%token PROC
%token RETURN
%token NULLPTR

/* Pointers			*/
%token INTPTR
%token CHARPTR
%token REALPTR

/* Operator Lexemes */
%token ASS
%token AND
%token OR
%token EQ
%token NE
%token GT
%token GE
%token LT
%token LE
%token NOT
%token PLUS
%token MINUS
%token MUL
%token DIV
%token ADDRESSOF
%token DEREF

/* Literal Lexemes */
%token TRUE
%token FALSE
%token CHARLITERAL
%token DECLITERAL
%token HEXLITERAL
%token REALLITERAL 
%token STRINGLITERAL

/* ID */
%token ID

%right ASS
%left OR
%left AND
%left EQ NE GT GE LT LE
%left PLUS MINUS
%left MUL DIV
%right NOT
%right ADDRESSOF DEREF

%%
S:          
              PROG                                  {printtree($1, 0);}
            ;
PROG:         
              PROG FUNCDECL                         {$$ = mknode("code", 2, $1, $2);}
            | FUNCDECL
            ;
FUNCDECL:    
              PROCEXPR
            | FUNCEXPR
            ;
PROCEXPR:    
              PROC ID '(' PARAMLIST ')' '{' BLOCK '}'  {$$ = mknode("proc", 3, $2, $4, $7);}
            ;
FUNCEXPR:   
              FUNC ID '(' PARAMLIST ')' RETURN TYPE  '{' BLOCK '}'  {$$ = mknode("func", 4, $2, $7, $4, $9);}
            ;
PARAMLIST:   
              VARLIST ';' PARAMLIST                 {$$ = mknode("paramlist", 2, $1, $3);}
            | VARLIST                               {$$ = mknode("paramlist", 1, $1);}
            | /*EMPTY*/                             {$$ = mkleaf("paramlist none");}
            ;         
BLOCK:      
              DECLARATION INNERBLOCK                {$$ = mknode("block", 2, $1, $2);}
            ;
INNERBLOCK: 
              INNERBLOCK STATEMENT                  {$$ = mknode("innerblock", 2, $1, $2);}
            | INNERBLOCK '{' BLOCK '}'              {$$ = mknode("innerblock", 2, $1, $3);} 
            | /*EMPTY*/                             {$$ = NULL;}
            ;
DECLARATION: 
              DECLARATION VARDECL                   {$$ = mknode("declarations", 2, $1, $2);}
            | DECLARATION FUNCDECL                  {$$ = mknode("declarations", 2, $1, $2);}
            | /*EMPTY*/                             {$$ = NULL;}
            ;
STATEMENT:   
              ASSIGNMENT     
            | FUNCCALL ';'
            | IFELSE
            | RETURNST
            | WHILEEXPR
            ;
RETURNST:   
              RETURN EXPR ';'                       {$$ = mknode("return", 1, $2);}
            ;
ASSIGNMENT:   
              ID ASS EXPR ';'                       {$$ = mknode("=", 2, $1, $3);}
            | ID '[' EXPR ']' ASS EXPR ';'          {$$ = mknode("[]=", 3, $1, $3, $6);}
            | DEREF EXPR ASS EXPR ';'               {$$ = mknode("^=", 2, $2, $4);}
            ;
EXPR:       
             '(' EXPR ')'                           {$$ = $2;}    
            | LITERAL 
            | ID
            | FUNCCALL
            | UNARYEXP
            | POINTEREXPR
            | EXPR BINARYOP EXPR                    {$$ = mknode($2->token, 2, $1, $3);};
            ;
VARDECL:    
              VAR VARLIST ';'                       {$$ = mknode("var", 1, $2);}
            ;
VARLIST:    
              ID ',' VARLIST                        {$$ = mknode("varlist", 2, $1, $3);}
            | ID ':' STRING '[' DECLITERAL ']'      {$$ = mknode("varlist", 2, $1, mknode("string", 1, $5));}
            | ID ':' TYPE                           {$$ = mknode("varlist", 2, $1, $3);} 
            ;
IFELSE:     
              IF '(' EXPR ')' '{' BLOCK '}' ELSE '{' BLOCK '}'   {$$ = mknode("ifelse", 3, $3 ,$6, $10);}  
            | IF '(' EXPR ')' '{' BLOCK '}'         {$$ = mknode("if", 2, $3, $6);}
            | IF '(' EXPR ')' STATEMENT             {$$ = mknode("if", 2, $3, $5);}
            ;
WHILEEXPR:  
              WHILE '(' EXPR ')' STATEMENT          {$$ = mknode("while", 2, $3, $5);}
            | WHILE '(' EXPR ')' '{' BLOCK '}'      {$$ = mknode("while", 2, $3, $6);}
            ;
FUNCCALL:   
              ID '(' ARGS ')'                       {$$ = mknode("call", 2, $1, $3);}
            ;
ARGS:       
              EXPR ',' ARGS                         {$$ = mknode("args", 2, $1, $3);}                            
            | EXPR                                  {$$ = mknode("args", 1, $1);} 
            | /*EMPTY*/                             {$$ = mkleaf("args none");}
            ;
POINTEREXPR:
              DEREF '(' EXPR ')'                    {$$ = mknode("^", 1, $3);}
            | DEREF ID                              {$$ = mknode("^", 1, $2);}
            | ADDRESSOF ID '[' EXPR ']'             {$$ = mknode("&", 2, $2, $4);}
            | ADDRESSOF ID                          {$$ = mknode("&", 1, $2);}
            ;
UNARYEXP:   
              UNARYOPS EXPR                         {$$ = mknode($1->token, 1, $2);}
            | '|' EXPR '|'                          {$$ = mknode("|s|", 1, $2);}
            ;
BINARYOP:   
              ARITHOP
            | LOGICALOP
            ;
ARITHOP:    
              DIV
            | MINUS
            | PLUS
            | MUL
            ;
LOGICALOP:  
              AND
            | OR
            | EQ
            | NE
            | GT
            | GE
            | LT
            | LE
            ;
UNARYOPS:    
              PLUS
            | MINUS
            | NOT
            ;        
TYPE:       
              SCALARTYPE
            | POINTERTYPE
            | STRING
            ;
SCALARTYPE: 
              INT
            | CHAR
            | REAL
            | BOOL
            ;
POINTERTYPE: 
              INTPTR
            | CHARPTR
            | REALPTR
            ;
LITERAL:    
              TRUE
            | FALSE 
            | DECLITERAL
            | HEXLITERAL
            | REALLITERAL 
            | CHARLITERAL
            | STRINGLITERAL
            | NULLPTR
            ;
%%
#include "lex.yy.c"
int main()
{
    return yyparse();
}

int yyerror()
{
    printf("\nParsing error: unexpected token \'%s\' at line %d.\n", yytext, lineno);   
    return 0; 
}

struct node* mkleaf(const char* token)
{
    return mknode(token, 0);
}

struct node* mknode(const char* token, int count, ...)
{
    va_list args;
    struct node* newnode;

    newnode = (struct node*)malloc(sizeof(struct node));
    newnode->token = strdup(token);
    newnode->nchildren = count;
    newnode->children = (struct node**)malloc(sizeof(struct node*) * count);

    va_start(args, count);
    for (int i = 0; i < count; i++)
        newnode->children[i] = va_arg(args, struct node*);
    va_end(args);

    return newnode;
}

#define isleaf(x) (x != NULL && x->nchildren == 0) 
void printtree(struct node* tree, int spacing)
{
    int done = 0;
    char* spaces = (char*)calloc(spacing*2+1, sizeof(char));

    if (!tree)
        return;

    while (done < spacing)
    {
        strcat(spaces, "  ");
        done++;
    }

    if (!isleaf(tree))
        printf("\n%s", spaces);

    printf(" (%s", tree->token);

    for (int i = 0; i < tree->nchildren; i++)
        printtree(tree->children[i], spacing + 1);

    printf(")");
    free(spaces);
}
