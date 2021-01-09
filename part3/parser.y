%{
#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

typedef enum _NodeType 
{
    CODE_N,
    PROC_N, FUNC_N,
    PARAMLIST_N, EMPTY_PARAMLIST,
    BLOCK_N, DECLARATIONS_N, INNERBLOCK_N,
    RETURN_N,
    ASSIGNMENT_N,
    ASSIGNMENT_BYINDEX_N,
    ASSIGNMENT_DEREF_N,
    VAR_N, VARLIST_N,
    IF_N, IFELSE_N,
    WHILE_N,
    CALL_N,
    ARGS_N,
    tVOID_N, tINT_N, tCHAR_N, tREAL_N, tBOOL_N, tSTRING_N,
    tINTPTR_N, tCHARPTR_N, tREALPTR_N,
    ASS_N,
    EXPR_N,
    LOGICALAND_N,
    LOGICALOR_N,
    FIRST_BINARY,
    EQ_N = FIRST_BINARY,
    NE_N,
    GT_N,
    GE_N,
    LT_N,
    LE_N,
    MUL_N,
    DIV_N,
    PLUS_N,
    MINUS_N,
    NOT_N,
    ADDRESS_N,
    DEREF_N,
    STRLEN_N,
    IDENTIFIER_N,
    ARITHEXPR_N,
    UNARYPLUS_N,
    UNARYMINUS_N,
    NULLPTR_N,
    BOOLLITERAL_N,
    CHARLITERAL_N,
    INTLITERAL_N,
    REALLITERAL_N,
    STRINGLITERAL_N,
    ARRAY_ACCESS_N
} NodeType;

typedef char* str;
typedef struct _node
{
    NodeType nodetype;
    int nchildren;
    str data;
    int line;
    struct _node** children;
} node;

node* mkleaf(NodeType n);
node* mkleaf_str(NodeType n, const str data);
node* mknode(NodeType n, int count, ...);
void printtree(node* tree, int spacing);

#define PBIT 0x10
typedef enum _Type
{
    tVOID, /* PROCs */
    tINT,
    tCHAR,
    tREAL,
    tBOOL,
    tSTRING,
    tNULLPTR = PBIT,
    tINTPTR,
    tCHARPTR,
    tREALPTR
} Type;

/* Utility macros */
#define ISPTRTYPE(x) (x >= tINT && x <= tREAL)
#define ISPTR(x)     (x & PBIT)
#define TOPTR(x)     (x | PBIT)
#define TOSCALAR(x)  (x & ~PBIT)
#define isnum(x) (x == tINT || x == tREAL)
#define CALLABLE_FLAG (1 << 0)
typedef struct _table_entry
{
    str identifier;
    /* Put here additional entry info members */
    Type type;
    int flags;     Type* parmeters;
    int nparameters;
    /* Tree data */
    struct _table_entry* left;
    struct _table_entry* right;
    int height;
} table_entry;

typedef struct _scopestack
{
    table_entry* symbol_table;
    Type return_type;
    struct _scopestack* next;
} scopestack, *scopestack_ptr;

Type verify_call(node* call_expr, FILE* f, str* t);
Type evaltype(node* expr, FILE* f, str* t);
void analyzer(node* n);
void process_node(node* n, FILE* f);

#define CALLABLE_ID 0
#define FUNC_TYPE 1
#define FUNC_PARAMS 2
#define FUNC_BLOCK 3
#define PROC_PARAMS 1
#define PROC_BLOCK 2
#define IS_FUNC(x) (x->nodetype == FUNC_N)

#endif
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
            | '|' ID '|'                            {$$ = mknode(STRLEN_N, 1, $2);}
            ;
ARITHEXPR:    
              EXPR DIV EXPR                         {$$ = mknode(DIV_N, 2, $1, $3);}
            | EXPR MINUS EXPR                       {$$ = mknode(MINUS_N, 2, $1, $3);}
            | EXPR PLUS EXPR                        {$$ = mknode(PLUS_N, 2, $1, $3);}
            | EXPR MUL EXPR                         {$$ = mknode(MUL_N, 2, $1, $3);}
            ;
BOOLEXPR:  
              EXPR AND EXPR                         {$$ = mknode(LOGICALAND_N, 2, $1, $3);}
            | EXPR OR EXPR                          {$$ = mknode(LOGICALOR_N, 2, $1, $3);}
            | EXPR EQ EXPR                          {$$ = mknode(EQ_N, 2, $1, $3);}
            | EXPR NE EXPR                          {$$ = mknode(NE_N, 2, $1, $3);}
            | EXPR GT EXPR                          {$$ = mknode(GT_N, 2, $1, $3);}
            | EXPR GE EXPR                          {$$ = mknode(GE_N, 2, $1, $3);};
            | EXPR LT EXPR                          {$$ = mknode(LT_N, 2, $1, $3);};
            | EXPR LE EXPR                          {$$ = mknode(LE_N, 2, $1, $3);};
            ;
UNARYOPS:    
              PLUS EXPR                             {$$ = mknode(UNARYPLUS_N, 1, $2);}
            | MINUS EXPR                            {$$ = mknode(UNARYMINUS_N, 1, $2);}
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
    temp->data = (str)strdup(data);
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

/* analyzer.c */

/* Error pretty-printing */
const str threeac_file = "3ac_temp.txt";
static void int_error(int line, const str message)
{
    /* Print message to user, use parser line as fancy "error code" */
    printf("ERROR %d: %s\n", line, message);
    /* There is nothing more to do... close down */
    remove(threeac_file);
    exit(EXIT_FAILURE);
}
static char msg[512];

#define error(fmt, ...) \
        { sprintf(msg, fmt, __VA_ARGS__); \
        int_error(__LINE__, msg); }
#define simple_error(message) \
        int_error(__LINE__, message)

static Type nodetypetotype(NodeType n)
{
    switch (n)
    {
        case tVOID_N:    return tVOID;
        case tBOOL_N:    return tBOOL;
        case tCHAR_N:    return tCHAR;
        case tINT_N:     return tINT;
        case tREAL_N:    return tREAL;
        case tSTRING_N:  return tSTRING;
        case tINTPTR_N:  return tINTPTR;
        case tCHARPTR_N: return tCHARPTR;
        case tREALPTR_N: return tREALPTR;
        default: /* UNREACHABLE */ assert(!n);
    }
}

/* Symbol Table AVL-Tree implementation code */

#define max(a, b) ((a > b) ? a : b)
#define height(x) (x ? x->height : 0)
table_entry* alloc_entry(const node* n, NodeType type)
{
    table_entry* entry = (table_entry*)calloc(1, sizeof(table_entry));
    entry->identifier = (str)strdup(n->data);
    entry->height = 1;
    entry->type = nodetypetotype(type);
    return entry;
}

table_entry* rightRotate(table_entry* y)
{
    table_entry* x = y->left;
    table_entry* t = x->right;

    x->right = y;
    y->left = t;

    y->height = max(height(y->left), height(y->right)) + 1;
    x->height = max(height(x->left), height(x->right)) + 1;

    return x;
}

table_entry* leftRotate(table_entry* x)
{
    table_entry* y = x->right;
    table_entry* t = y->left;

    y->left = x;
    x->right = t;

    x->height = max(height(x->left), height(x->right)) + 1;
    y->height = max(height(y->left), height(y->right)) + 1;

    return y;
}

#define RET(x) { *root = x; return 1; }
/* returns 1 if insertion was successful, returns 0 if item already exists */
int add_entry(table_entry** root, const node* n, NodeType type)
{
    int cmp;
    table_entry* entry;
    str identifier;

    if (!root || !n || !n->data)
        return 0;

    identifier = n->data;
    entry = *root;

    if (!entry)
        RET(alloc_entry(n, type));

    cmp = strcmp(identifier, entry->identifier);

    if (cmp < 0)
    {
        if (!add_entry(&entry->left, n, type))
            return 0;
    }
    else if (cmp > 0)
    {
        if (!add_entry(&entry->right, n, type))
            return 0;
    }
    else
    {
        return 0;
    }

    entry->height = 1 + max(height(entry->left), height(entry->right));

    int balance = height(entry->left) - height(entry->right);

    if (balance > 1)
    {
        cmp = strcmp(identifier, entry->left->identifier);
        if (cmp < 0)
        {
            RET(rightRotate(entry));
        }
        if (cmp > 0)
        {
            entry->left = leftRotate(entry->left);
            RET(rightRotate(entry));
        }
    }
    if (balance < -1)
    {
        cmp = strcmp(identifier, entry->right->identifier);
        if (cmp > 0)
        {
            RET(leftRotate(entry));
        }
        if (cmp < 0)
        {
            entry->right = rightRotate(entry->right);
            RET(leftRotate(entry));
        }
    }
    return 1;
}

/* if insert code is working then lookup code will take O(log(n)) to find the entry
   return a null pointer if was not found */
table_entry* get_entry(table_entry* root, const str identifier)
{
    table_entry* current = root;
    int cmp;

    while (current && (cmp = strcmp(identifier, current->identifier)) != 0)
    {
        current = (cmp < 0) ? current->left : current->right;
    }
    return current;
}

void delete_table(table_entry* root)
{
    if (root)
    {
        delete_table(root->left);
        delete_table(root->right);
        free(root->identifier);
        free(root->parmeters);
        free(root);
    }
}
/* End of Symbol Table AVL-Tree implementation code */

/* Scope stack implementation code */
/* Stack for scopes */
scopestack_ptr stack = NULL;

void scope_push()
{
    scopestack_ptr news = malloc(sizeof(scopestack));
    news->symbol_table = NULL;
    news->next = stack;
    news->return_type = (stack != NULL) ? stack->return_type : tVOID;
    stack = news;
}

void scope_pop()
{
    scopestack_ptr temp;

    if (stack == NULL)
        return;

    temp = stack;
    stack = temp->next;

    delete_table(temp->symbol_table);
    free(temp);
}

int add_symbol(node* n, NodeType type)
{
    return add_entry(&stack->symbol_table, n, type);
}

table_entry* find_symbol(const str identifier)
{
    scopestack_ptr current = stack;

    while (current)
    {
        table_entry* symbol = get_entry(current->symbol_table, identifier);
        if (symbol)
            return symbol;
        current = current->next;
    }
    return NULL;
}
/* End of Scope stack implementation code */
str freshvar()
{
    static int curr = 0;
    str var = calloc(4, sizeof(char));

    sprintf(var, "t%d", curr++);
    return var;
}
str freshlabel()
{
    static int lcurr = 0;
    str var = calloc(4, sizeof(char));

    sprintf(var, "L%d", lcurr++);
    return var;
}
/* AST Semantic analysis code */
Type verify_call(node* call_expr, FILE* f, str* t)
{
    node* call_args;
    table_entry* callable;
    Type* callable_params;
    const str identifier = call_expr->children[0]->data;
    str temp;
    callable = find_symbol(identifier);
    int i;

    if (!callable)
        error("Line %d, Identifier %s does not exists.", call_expr->line, identifier);
    if (!(callable->flags & CALLABLE_FLAG))
        error("Line %d, Identifier %s is not a function/procedure.", call_expr->line, identifier);

    callable_params = callable->parmeters;
    call_args = call_expr->children[1];

    if (call_args->nchildren != callable->nparameters)
    {
        error("Line %d: Call to %s: %s arguments.",
              call_expr->line, identifier, (call_args->nchildren > callable->nparameters) ? "Too much": "Not enough");
    }

    for (i = 0; i < callable->nparameters; i++)
    {
        Type arg_type = evaltype(call_args->children[i], f ,&temp);
        if (callable_params[i] != arg_type && !(isnum(arg_type) && isnum(callable_params[i])))
            error("Line %d: Call to %s, Argument no. %d: Type mismatch.", call_expr->line, identifier, i + 1);
        fprintf(f,"\tPushParam %s\n\t", temp);
    }
    *t = freshvar();
    fprintf(f,"\t%s = LCall %s\n\t",*t,identifier);
    fprintf(f,"\tPopParams %d\n\t",callable->nparameters*8);
    return callable->type;
}

#define LEFT  0
#define RIGHT 1
/* Expression evaluator, returns the type of expression if its a proper expression */
Type evaltype(node* expr, FILE* f, str* t)
{
    static const str ops[] = {"==", "!=", ">", ">=", "<", "<=", "*", "/", "+", "-"};
    NodeType ntype = expr->nodetype;
    str temp;

    switch (ntype)
    {   
        /* Simple values (literals) */
        case NULLPTR_N:
            *t = expr->data;
            return tNULLPTR;
        case BOOLLITERAL_N:
            *t = expr->data;
            return tBOOL;
        case CHARLITERAL_N:
            *t = expr->data;
            return tCHAR;
        case INTLITERAL_N:
            *t = expr->data;
            return tINT;
        case REALLITERAL_N:
            *t = expr->data;
            return tREAL;
        case STRINGLITERAL_N:
            *t = expr->data;        
            return tSTRING;

        case IDENTIFIER_N:
        {
            table_entry* entry = find_symbol(expr->data);
            if (!entry)
                error("Line %d: Identifier %s not found", expr->line, expr->data);

            /* This kind of stuff is being dealt by the CALL_N clauses */
            if (entry->flags & CALLABLE_FLAG) 
                error("Line %d: Identifier %s is function/procedure", expr->line, expr->data);
            *t = expr->data;
            return entry->type;
        }
        /* Binary operators */
        case LOGICALAND_N:
        case LOGICALOR_N:
        {
            Type left, right;
            str tleft, tright;
            str l1 = freshlabel();
            str l2 = freshlabel();
            *t = freshvar();   
            
            if (ntype == LOGICALOR_N)
            {
                left  = evaltype(expr->children[LEFT], f, &tleft);
                fprintf(f, "\tif %s Goto %s\n\t", tleft, l1);
                right = evaltype(expr->children[RIGHT], f, &tright);
                fprintf(f, "\tif %s Goto %s\n\t", tright, l1);
                fprintf(f, "\t%s = false\n\t", *t);
                fprintf(f, "\tGoto %s\n\t", l2);
                fprintf(f, "%s:\t%s = true\n\t", l1, *t);
                fprintf(f, "%s:", l2);
            }
            else
            {
                left  = evaltype(expr->children[LEFT], f, &tleft);
                fprintf(f, "\tif %s == false Goto %s\n\t", tleft, l1);
                right = evaltype(expr->children[RIGHT], f, &tright);
                fprintf(f, "\tif %s == false Goto %s\n\t", tright, l1);
                fprintf(f, "\t%s = true\n\t", *t);
                fprintf(f, "\tGoto %s\n\t", l2);
                fprintf(f, "%s:\t%s = false\n\t", l1, *t);
                fprintf(f, "%s:", l2);
            }
            if (left != tBOOL || right != tBOOL)
                error("Line %d: Logical operation requires 2 boolean operands", expr->line);
            return tBOOL;
        }
        case EQ_N:
        case NE_N:
        case GT_N:
        case GE_N:
        case LT_N:
        case LE_N:
        case MUL_N:
        case DIV_N:
        case PLUS_N:
        case MINUS_N:
        {
            Type left, right;
            str tleft, tright;
            left  = evaltype(expr->children[LEFT], f, &tleft);
            right = evaltype(expr->children[RIGHT], f, &tright);

            if (left == tSTRING || right == tSTRING)
                error("Line %d: Binary operators are not supported with strings", expr->line);

            *t = freshvar();
            fprintf(f, "\t%s = %s %s %s\n\t", *t, tleft, ops[ntype - FIRST_BINARY], tright);

            switch (ntype)
            {
                case EQ_N:
                case NE_N:
                    if ((left != right) &&
                        !(left == tNULLPTR && ISPTR(right)) &&
                        !(right == tNULLPTR && ISPTR(left)))
                    {
                        error("Line %d: Mismatched types in comparison", expr->line);
                    }
                    break;
                case GT_N:
                case GE_N:
                case LT_N:
                case LE_N:
                    if ((left != tREAL && left != tINT) ||
                        (right != tREAL && right != tINT))
                    {
                        error("Line %d: Arithmetic comparison with non int/real operands", expr->line);
                    }
                    break;
                case MUL_N:
                case DIV_N:
                case PLUS_N:
                case MINUS_N:
                    {
                        if ((left != tREAL && left != tINT) ||
                            (right != tREAL && right != tINT))
                        {
                            error("Line %d: Arithmetic operation with non int/real operands", expr->line);
                        }
                        return (left == tINT && right == tINT) ? tINT : tREAL;
                    }
            }
            return tBOOL;
        }
        case STRLEN_N:
        {
            table_entry* entry = find_symbol(expr->children[0]->data);

            if (entry->type != tSTRING)
                error("Line %d: Cannot use | | operation on non-string values", expr->line);

            *t = freshvar();
            fprintf(f, "\t%s = %d\n\t", *t, (entry->flags >> 1));
            return tINT;
        }
        case NOT_N:
        {
            if (evaltype(expr->children[0], f, &temp) != tBOOL)
                error("Line %d: Cannot use ! on a non boolean values", expr->line);
            *t = freshvar();
            fprintf(f,"\t%s = !%s\n\t", *t,temp);
            return tBOOL;
        }
        case UNARYPLUS_N:
        case UNARYMINUS_N:
        {
            Type type = evaltype(expr->children[0], f, &temp);
            if (type != tREAL && type != tINT)
                error("Line %d: Cannont use unary +/- on non int/real operand", expr->line);
            *t = freshvar();
            if (ntype == UNARYMINUS_N)
                fprintf(f, "\t%s = 0 - %s\n\t",*t,temp);
            else
                fprintf(f, "\t%s = 0 + %s\n\t",*t,temp);
            return type;
        }
        case CALL_N:
        {
            Type type = verify_call(expr, f, t);
            if (type == tVOID)
                error("Line %d: Cannot evaluate call of procedure %s.", expr->line, expr->children[0]->data);
            return type;
        }
        case ADDRESS_N:
        {
            Type type = evaltype(expr->children[0], f , &temp);
            str t1;
            *t = freshvar();
            fprintf(f,"\t%s = &%s\n\t",*t,temp);
            if (expr->nchildren == 1)
            {
                if (type != tINT && type != tCHAR && type != tREAL)
                    error("Line %d: operator & must be used on int, char or real.", expr->line);
                return TOPTR(type);
            }
            if (type != tSTRING)
                error("Line %d: operator &[] must be a string.", expr->line);
            if (evaltype(expr->children[1], f , &temp) != tINT)
                error("Line %d: operator &[] must have a int index", expr->line);
            t1 = *t;
            *t = freshvar();
            fprintf(f, "\t%s = %s + %s\n\t",*t,t1,temp);
            return tCHARPTR;
        }
        case DEREF_N:
        {
            Type type = evaltype(expr->children[0], f , &temp);
            if (!ISPTR(type))
                error("Line %d: operator ^ can not be used on non-pointer types", expr->line);
            *t = freshvar();
            fprintf(f, "\t%s = *%s\n\t", *t, temp);
            if (type != tINTPTR && type != tREALPTR && type != tCHARPTR)
                error("Line %d: Pointer error...", expr->line);
            return TOSCALAR(type);
        }
        case ARRAY_ACCESS_N:
        {
            str t0,t1,temp1,temp2;
            if (evaltype(expr->children[0], f , &temp1) != tSTRING)
                error("Line %d: operator [] can be only used on strings.", expr->line);
            if (evaltype(expr->children[1], f , &temp2) != tINT)
                error("Line %d: operator [] can only receive a int as index position.", expr->line);
            t0 = freshvar();
            t1 = freshvar();
            *t = freshvar();
            fprintf(f, "\t%s = &%s\n\t",t0,temp1);
            fprintf(f, "\t%s = %s + %s\n\t",t1,t0,temp2);
            fprintf(f, "\t%s = *%s\n\t",*t,t1);
            return tCHAR;
        }
    }
    /* UNREACHABLE */
    assert(!ntype);
}

int main_defined = 0;

/* Do an overall "preoder" scan to check if there are any semantic errors */
/* AST Processing entry-point */
void analyzer(node* n)
{
    int i;
    char c;
    FILE* file = fopen(threeac_file,"w");

    if (!n ||  n->nodetype != CODE_N)
        simple_error("Bad AST was given to analyzer()");
    
    /* Push global scope */
    scope_push();

    for (i = 0; i < n->nchildren; i++)
        process_node(n->children[i], file);

    if (!main_defined)
        simple_error("Main() procedure was not defined.");

    /* Pop global scope */
    scope_pop();
    fclose(file);
    file = fopen(threeac_file, "r");
    while ((c = fgetc(file)) != EOF)
        printf("%c",c);
    fclose(file);
    remove(threeac_file);
}

#define LAST(n) n->children[n->nchildren-1]
/* Central AST processor, in preorder processes statements and declerations */
void process_node(node* n, FILE* f)
{
    static int no_push = 0;
    int i, j, done;
    table_entry* entry;
    node *plist, *vl;
    Type type;
    str t;
    if (!n)
        return;

    /* Sanity checks*/
    assert(n->nodetype < EXPR_N && n->nodetype != CODE_N);

    switch (n->nodetype)
    {
        /* Define these as "callable" */
        case FUNC_N:
        case PROC_N:
        {
            int fline = n->children[CALLABLE_ID]->line;
            /* Add identifier to current scope */
            if (add_symbol(n->children[CALLABLE_ID], IS_FUNC(n) ? n->children[FUNC_TYPE]->nodetype : tVOID_N) == 0)
                error("Line %d: Identifier %s exists already in current scope", fline, n->children[CALLABLE_ID]->data);
            /* Get symbol table entry to put additional data/"decoration" on the identifier*/
            entry = find_symbol(n->children[CALLABLE_ID]->data);
            if (entry->type == tSTRING)
                error("Line %d: Functions can not return strings.", fline);
            /* Mark identifier as a callable */
            entry->flags |= CALLABLE_FLAG;

            plist = n->children[IS_FUNC(n) ? FUNC_PARAMS : PROC_PARAMS];

            /* Do the Main() checks */
            if (strcmp(entry->identifier, "Main") == 0)
            {   
                if (main_defined)
                    error("Line %d: Main() is already defined", fline);
                if (plist->nodetype != EMPTY_PARAMLIST)
                    error("Line %d: Main() should not have parameters", fline);
                if (n->nodetype != PROC_N)
                    error("Line %d: Main() should be a procedure", fline);
                if (stack->next != NULL)
                    error("Line %d: Main() should be in global scope.", fline);
                main_defined = 1;
            }
            
            /* Create parameter list for identifier */
            for (i = 0, done = 0; i < plist->nchildren; i++)
            {
                vl = plist->children[i];
                entry->nparameters += vl->nchildren - 1;
                entry->parmeters = realloc(entry->parmeters, entry->nparameters * sizeof(Type));
                type = nodetypetotype(LAST(vl)->nodetype);
                for (j = 0; j < vl->nchildren - 1; j++)
                    entry->parmeters[done++] = type;
            }

            /* Push new scope */
            scope_push();
            /* Set scope return type */
            stack->return_type = entry->type;
            no_push = 1; /* avoids things such as proc f(x:int) { var x: bool; ...} */
            
            fprintf(f, "\n%s:\n\t\tBeginFunc\n\t", entry->identifier);

            /* Add parameters to new scope's symtable */
            process_node(n->children[IS_FUNC(n) ? FUNC_PARAMS : PROC_PARAMS], f);
            /* Process function block */
            process_node(n->children[IS_FUNC(n) ? FUNC_BLOCK : PROC_BLOCK], f);
            /* Pop callable scope, Should be popped by ^ BLOCK_N clause */
            
            fprintf(f, "\tEndFunc\n\t");

            if (!IS_FUNC(n))
                break;
            /* Logic behind checking if a FUNC has the required return statement at the end
             *          { BLOCK }              <- Function scope block            
             *              |
             * DECLARATION     INNERBLOCK      <- INNERBLOCK needs to exist (not null)
             *      |              |
             *      x     STMT(1) ... STMT(N)  <- Needs to have at least 1 statement
             *                          |
             *                        RETURN   <- Should be a RETURN_N node, typecheck should be already done
             */
            node* block = n->children[FUNC_BLOCK];
            if (block->children[1] == NULL)
                error("Line %d: Function must have a return statement defined at the end", fline);
            if (block->children[1]->nchildren == 0)
                error("Line %d: Function must have a return statement defined at the end", fline);
            block = block->children[1];
            if (LAST(block)->nodetype != RETURN_N)
                error("Line %d: Function must have a return statement defined at the end", fline);
            break;
        }
        case RETURN_N:
        {
            Type rt, st;
            st = stack->return_type;
            rt = evaltype(n->children[0], f, &t);
            if (stack->return_type == tVOID)
                error("Line %d: Cannot return in a procedure", n->line);

            if (rt != st && !(isnum(rt) && isnum(st)))
                error("Line %d: Return value type does not match function return type", n->line);
            fprintf(f,"\tReturn %s\n\t",t);
            break;
        }
        case BLOCK_N:
        case VAR_N:
        case PARAMLIST_N:
        case EMPTY_PARAMLIST:
        case DECLARATIONS_N:
        case INNERBLOCK_N:
        {
            if (n->nodetype == BLOCK_N)
            {
                if (no_push != 1)
                    scope_push();
                no_push = 0;
            }

            for (i = 0; i < n->nchildren; i++)
                process_node(n->children[i], f);

            if (n->nodetype == BLOCK_N)
                scope_pop();

            break;
        }
        case VARLIST_N:
        {
            int nchildren = n->nchildren;
            int slen;
            NodeType nodetype = LAST(n)->nodetype;
            if (nodetype == tSTRING_N)
            {
                nchildren--;
                slen = atoi(n->children[nchildren - 1]->data);
            }

            for (i = 0; i < nchildren - 1; i++)
            {
                if (add_symbol(n->children[i], nodetype) == 0)
                    error("Line %d: Identifier %s exists already in current scope", n->children[i]->line, n->children[i]->data);

                if (nodetype == tSTRING_N)
                {
                    table_entry* entry = find_symbol(n->children[i]->data);
                    entry->flags |= slen << 1;
                }
            }
            break;
        }
        case IF_N:
        case IFELSE_N:
        {
            str t, l1, l2;
            type = evaltype(n->children[0], f , &t);
            if (type != tBOOL)
                error("Line %d: Conditional expression must be boolean", n->children[0]->line);

            l1 = freshlabel();
            l2 = freshlabel();
            fprintf(f, "\tif %s == false Goto %s\n\t", t, l1);
            process_node(n->children[1], f);
            if (n->nodetype == IFELSE_N)
            {
                fprintf(f, "\tGoto %s\n\t", l2);
            }
            fprintf(f, "%s:", l1);
            if (n->nodetype == IFELSE_N)
            {
                process_node(n->children[2], f);
                fprintf(f, "%s:", l2);
            }            
            break;
        } 
        case WHILE_N:
        {
            str t, l1, l2, l3;
            l1 = freshlabel();
            l2 = freshlabel();
            l3 = freshlabel();
            fprintf(f, "%s:", l1);
            type = evaltype(n->children[0], f , &t);
            if (type != tBOOL)
                error("Line %d: Conditional expression must be boolean", n->children[0]->line);
            fprintf(f, "\tif %s Goto %s\n\t", t, l2);
            fprintf(f, "\tGoto %s\n\t%s:",l3, l2);
            process_node(n->children[1], f);
            fprintf(f, "\tGoto %s\n\t%s:", l1, l3);
            break;
        }
        case ASSIGNMENT_N:
        {
            table_entry* id = find_symbol(n->children[0]->data);
            if (id == NULL)
                error("Line %d: Identifier %s is not defined", n->line, n->children[0]->data);

            if (id->flags & CALLABLE_FLAG)
                error("Line %d: Identifier %s is a procedure/function", n->line, n->children[0]->data);

            type = evaltype(n->children[1], f , &t);

            if (!((isnum(type) && isnum(id->type)) || type == id->type || (ISPTR(id->type) && type == tNULLPTR)))
                error("Line %d: Type mismatch in assignment", n->line);

            fprintf(f, "\t%s = %s\n\t", id->identifier, t);
            break;
        }
        case ASSIGNMENT_BYINDEX_N:
        {
            table_entry* id = find_symbol(n->children[0]->data);
            str t0,t1,temp1,temp2;
            if (id == NULL)
                error("Line %d: Identifier %s does not exists", n->line, n->children[0]->data);
            if (id->flags & CALLABLE_FLAG)
                error("Line %d: Identifier %s is a procedure/function", n->line, n->children[0]->data);
            if (id->type != tSTRING)
                error("Line %d: Identifier %s is not a string", n->line, n->children[0]->data);
            if (evaltype(n->children[1], f , &temp1) != tINT)
                error("Line %d: Index value must be an int", n->line);
            if (evaltype(n->children[2], f , &temp2) != tCHAR)
                error("Line %d: Assignment value must be an char", n->line);
            t0 = freshvar();
            t1 = freshvar();
            fprintf(f, "\t%s = &%s\n\t",t0,id->identifier);
            fprintf(f, "\t%s = %s + %s\n\t",t0,t1,temp1);
            fprintf(f, "\t*%s = %s\n\t",t1,t0);
            break;
        }
        case ASSIGNMENT_DEREF_N:
        {
            Type to, from;
            str temp1, temp2;
            to = evaltype(n->children[0], f , &temp1);
            from = evaltype(n->children[1], f , &temp2);
            if (!ISPTR(to))
                simple_error("Destination is not a pointer");
            to = TOSCALAR(to);
            if (to != from)
                simple_error("Type mismatch");
            if (to != tINT && to != tREAL && to != tCHAR)
                simple_error("Destination type is not pointer to int, real or char");
            fprintf(f, "\t*%s = %s\n\t", temp1, temp2);
            break;
        }
        case CALL_N:
        {
            verify_call(n, f , &t);
            break;
        }
        default:
            /* UNREACHABLE */
            assert(!n->nodetype);
            break;
    }
}
/* End of AST Semantic analysis code */
