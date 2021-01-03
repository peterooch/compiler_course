%{
#include "common.h"
#define YYSTYPE node*
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
              PROG                                  {analyzer($1);}
            ;
PROG:         
              PROG FUNCDECL                         {$$ = mknode(CODE_N, 2, $1, $2);}
            | FUNCDECL                              {$$ = mknode(CODE_N, 1, $1);}
            ;
FUNCDECL:    
              PROCEXPR
            | FUNCEXPR
            ;
PROCEXPR:    
              PROC ID '(' PARAMLIST ')' '{' BLOCK '}'  {$$ = mknode(PROC_N, 3, $2, $4, $7);}
            ;
FUNCEXPR:   
              FUNC ID '(' PARAMLIST ')' RETURN TYPE  '{' BLOCK '}'  {$$ = mknode(FUNC_N, 4, $2, $7, $4, $9);}
            ;
PARAMLIST:   
              VARLIST ';' PARAMLIST                 {$$ = mknode(PARAMLIST_N, 2, $1, $3);}
            | VARLIST                               {$$ = mknode(PARAMLIST_N, 1, $1);}
            | /*EMPTY*/                             {$$ = mkleaf(EMPTY_PARAMLIST);}
            ;         
BLOCK:      
              DECLARATION INNERBLOCK                {$$ = mknode(BLOCK_N, 2, $1, $2);}
            ;
INNERBLOCK: 
              INNERBLOCK STATEMENT                  {$$ = mknode(INNERBLOCK_N, 2, $1, $2);}
            | INNERBLOCK '{' BLOCK '}'              {$$ = mknode(INNERBLOCK_N, 2, $1, $3);} 
            | /*EMPTY*/                             {$$ = NULL;}
            ;
DECLARATION: 
              DECLARATION VARDECL                   {$$ = mknode(DECLARATIONS_N, 2, $1, $2);}
            | DECLARATION FUNCDECL                  {$$ = mknode(DECLARATIONS_N, 2, $1, $2);}
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
              RETURN EXPR ';'                        {$$ = mknode(RETURN_N, 1, $2);}
            ;
ASSIGNMENT:   
              ID ASS EXPR ';'                        {$$ = mknode(ASSIGNMENT_N, 2, $1, $3);}
            | ID '[' EXPR ']' ASS EXPR ';'           {$$ = mknode(ASSIGNMENT_BYINDEX_N, 3, $1, $3, $6);}
            | DEREF EXPR ASS EXPR ';'                {$$ = mknode(ASSIGNMENT_DEREF_N, 2, $2, $4);}
            ;
EXPR:       
             '(' EXPR ')'                            {$$ = $2;}    
            | LITERAL 
            | ID
            | FUNCCALL
            | UNARYEXP
            | POINTEREXPR
            | ARITHEXPR
            | BOOLEXPR
            | ID '[' EXPR ']'                       {$$ = mknode(ARRAY_ACCESS_N, 2, $1, $3);}
            ;
VARDECL:    
              VAR VARLIST ';'                       {$$ = mknode(VAR_N, 1, $2);}
            ;
VARLIST:    
              ID ',' VARLIST                        {$$ = mknode(VARLIST_N, 2, $1, $3);}
            | ID ':' STRING '[' DECLITERAL ']'      {$$ = mknode(VARLIST_N, 3, $1, $5, $3);}
            | ID ':' TYPE                           {$$ = mknode(VARLIST_N, 2, $1, $3);} 
            ;
IFELSE:     
              IF '(' EXPR ')' '{' BLOCK '}' ELSE '{' BLOCK '}'   {$$ = mknode(IFELSE_N, 3, $3 ,$6, $10);}
            | IF '(' EXPR ')' STATEMENT ELSE STATEMENT           {$$ = mknode(IFELSE_N, 3, $3, $5, $7);}
            | IF '(' EXPR ')' '{' BLOCK '}'                      {$$ = mknode(IF_N, 2, $3, $6);}
            | IF '(' EXPR ')' STATEMENT                          {$$ = mknode(IF_N, 2, $3, $5);}
            ;
WHILEEXPR:  
              WHILE '(' EXPR ')' STATEMENT          {$$ = mknode(WHILE_N, 2, $3, $5);}
            | WHILE '(' EXPR ')' '{' BLOCK '}'      {$$ = mknode(WHILE_N, 2, $3, $6);}
            ;
FUNCCALL:   
              ID '(' ARGS ')'                       {$$ = mknode(CALL_N, 2, $1, $3);}
            ;
ARGS:       
              EXPR ',' ARGS                         {$$ = mknode(ARGS_N, 2, $1, $3);}                            
            | EXPR                                  {$$ = mknode(ARGS_N, 1, $1);} 
            | /*EMPTY*/                             {$$ = mkleaf(ARGS_N);}
            ;
POINTEREXPR:
              DEREF '(' ID PLUS EXPR')'             {$$ = mknode(DEREF_N, 1, $3);}
            | DEREF '(' ID MINUS EXPR')'            {$$ = mknode(DEREF_N, 1, $3);}
            | DEREF ID                              {$$ = mknode(DEREF_N, 1, $2);}
            | ADDRESSOF ID '[' EXPR ']'             {$$ = mknode(ADDRESS_N, 2, $2, $4);}
            | ADDRESSOF ID                          {$$ = mknode(ADDRESS_N, 1, $2);}
            ;
UNARYEXP:   
              UNARYOPS
            | '|' EXPR '|'                          {$$ = mknode(STRLEN_N, 1, $2);}
            ;
ARITHEXPR:    
              EXPR DIV EXPR                         {$$ = mknode(ARITH_N, 2, $1, $3);}
            | EXPR MINUS EXPR                       {$$ = mknode(ARITH_N, 2, $1, $3);}
            | EXPR PLUS EXPR                        {$$ = mknode(ARITH_N, 2, $1, $3);}
            | EXPR MUL EXPR                         {$$ = mknode(ARITH_N, 2, $1, $3);}
            ;
BOOLEXPR:  
              EXPR AND EXPR                         {$$ = mknode(LOGICAL_N, 2, $1, $3);}
            | EXPR OR EXPR                          {$$ = mknode(LOGICAL_N, 2, $1, $3);}
            | EXPR EQ EXPR                          {$$ = mknode(COMP_N, 2, $1, $3);}
            | EXPR NE EXPR                          {$$ = mknode(COMP_N, 2, $1, $3);}
            | EXPR GT EXPR                          {$$ = mknode(ARITHCOMP_N, 2, $1, $3);}
            | EXPR GE EXPR                          {$$ = mknode(ARITHCOMP_N, 2, $1, $3);};
            | EXPR LT EXPR                          {$$ = mknode(ARITHCOMP_N, 2, $1, $3);};
            | EXPR LE EXPR                          {$$ = mknode(ARITHCOMP_N, 2, $1, $3);};
            ;
UNARYOPS:    
              PLUS EXPR                             {$$ = mknode(UNARYEXPR_N, 1, $2);}
            | MINUS EXPR                            {$$ = mknode(UNARYEXPR_N, 1, $2);}
            | NOT EXPR                              {$$ = mknode(NOT_N, 1, $2);}
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

/* AST Tree building code */
node* mkleaf_str(NodeType nodetype, const str data)
{
    node* temp = mknode(nodetype, 0);
    temp->data = strdup(data);
    return temp;
}

node* mkleaf(NodeType nodetype)
{
    return mknode(nodetype, 0);
}

node* mknode(NodeType nodetype, int count, ...)
{
    va_list args;
    node* newnode;
    node** children;
    int i, j, copied;

    newnode = (node*)malloc(sizeof(node));
    children = (node**)malloc(sizeof(node*) * count);
    newnode->nodetype = nodetype;
    newnode->nchildren = count;
    newnode->data = NULL;
    newnode->line = lineno; /* For debugging purposes */

    va_start(args, count);
    for (i = 0; i < count; i++)
    {
        children[i] = va_arg(args, node*);
        if (children[i] != NULL && nodetype < EXPR_N && children[i]->nodetype == nodetype)
            newnode->nchildren += children[i]->nchildren;
    }
    va_end(args);

    if (newnode->nchildren == count)
    {
        newnode->children = children;
        return newnode;
    }

    newnode->children = (node**)malloc(sizeof(node*) * newnode->nchildren);

    for (i = 0, copied = 0; i < count; i++)
    {
        if (children[i] == NULL)
            continue;

        if (children[i]->nodetype == nodetype)
        {
            for (j = 0; j < children[i]->nchildren; j++)
                newnode->children[copied++] = children[i]->children[j];
        }
        else
        {
            newnode->children[copied++] = children[i];
        }
    }
    newnode->nchildren = copied;

    free(children);
    return newnode;
}

/* Part 1 Code, not being used */
void printtree(node* tree, int spacing)
{
    int i, done = 0;

    if (tree == NULL)
        return;

    if (tree->nchildren != 0)
    {
        printf("\n");
        while (done++ < spacing)
            printf("  ");
    }

    printf(" (%d", tree->nodetype);

    for (i = 0; i < tree->nchildren; i++)
        printtree(tree->children[i], spacing + 1);

    printf(")");
}
