#include "common.h"

/* Error pretty-printing */
static inline void int_error(int line, const str message)
{
    /* Print message to user, use parser line as fancy "error code" */
    printf("ERROR %d: %s\n", line, message);
    /* There is nothing more to do... close down */
    exit(EXIT_FAILURE);
}
static char msg[512];

#define error(fmt, ...) \
        { sprintf(msg, fmt, __VA_ARGS__); \
        int_error(__LINE__, msg); }
#define simple_error(message) \
        int_error(__LINE__, message)

static inline Type nodetypetotype(NodeType n)
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
    entry->identifier = strdup(n->data);
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

/* AST Semantic analysis code */
Type verify_call(node* call_expr)
{
    node* call_args;
    table_entry* callable;
    Type* callable_params;
    const str identifier = call_expr->children[0]->data;

    callable = find_symbol(identifier);

    if (!callable)
        error("Line %d, Identifier %s does not exists.", call_expr->line, identifier);
    if (!(callable->flags & CALLABLE_FLAG))
        error("Line %d, Identifier %s is not a function/procedure.", call_expr->line, identifier);

    callable_params = callable->parmeters;
    call_args = call_expr->children[1];

    if (callable->nparameters != call_args->nchildren)
        error("Line %d: Call to %s: Too many/Too few arguments.", call_expr->line, identifier);

    for (int i = 0; i < callable->nparameters; i++)
    {
        if (callable_params[i] != evaltype(call_args->children[i]))
            error("Line %d: Call to %s, Argument no. %d: Type mismatch.", call_expr->line, identifier, i + 1);
    }
    return callable->type;
}

#define LEFT  0
#define RIGHT 1
/* Expression evaluator, returns the type of expression if its a proper expression */
Type evaltype(node* expr)
{
    NodeType ntype = expr->nodetype;

    switch (ntype)
    {   
        /* Simple values (literals) */
        case NULLPTR_N:
            return tNULLPTR;
        case BOOLLITERAL_N:
            return tBOOL;
        case CHARLITERAL_N:
            return tCHAR;
        case INTLITERAL_N:
            return tINT;
        case REALLITERAL_N:
            return tREAL;
        case STRINGLITERAL_N:
            return tSTRING;

        case IDENTIFIER_N:
        {
            table_entry* entry = find_symbol(expr->data);
            if (!entry)
                error("Line %d: Identifier %s not found", expr->line, expr->data);

            /* This kind of stuff is being dealt by the CALL_N clauses */
            if (entry->flags & CALLABLE_FLAG) 
                error("Line %d: Identifier %s is function/procedure", expr->line, expr->data);

            return entry->type;
        }
        /* Binary operators */
        case LOGICAL_N:
        case COMP_N:
        case ARITHCOMP_N:
        case ARITH_N:
        {
            Type left, right;
            left  = evaltype(expr->children[LEFT]);
            right = evaltype(expr->children[RIGHT]);

            if (left == tSTRING || right == tSTRING)
                error("Line %d: Binary operators are not supported with strings", expr->line);

            switch (ntype)
            {
                case LOGICAL_N:
                    if (left != tBOOL || right != tBOOL)
                        error("Line %d: Logical operation requires 2 boolean operands", expr->line);
                    break;
                case COMP_N:
                    if ((left != right) &&
                        !(left == tNULLPTR && ISPTR(right)) &&
                        !(right == tNULLPTR && ISPTR(left)))
                    {
                        error("Line %d: Mismatched types in comparison", expr->line);
                    }
                    break;
                case ARITHCOMP_N:
                    if ((left != tREAL && left != tINT) ||
                        (right != tREAL && right != tINT))
                    {
                        error("Line %d: Arithmetic comparison with non int/real operands", expr->line);
                    }
                    break;
                case ARITH_N:
                    if ((left != tREAL && left != tINT) ||
                        (right != tREAL && right != tINT))
                    {
                        error("Line %d: Arithmetic operation with non int/real operands", expr->line);
                    }
                    return (left == tINT && right == tINT) ? tINT : tREAL;
            }
            return tBOOL;
        }
        case STRLEN_N:
        {
            if (evaltype(expr->children[0]) != tSTRING)
                error("Line %d: Cannot use | | operation on non-string values", expr->line);
            return tINT;
        }
        case NOT_N:
        {
            if (evaltype(expr->children[0]) != tBOOL)
                error("Line %d: Cannot use ! on a non boolean values", expr->line);
            return tBOOL;
        }
        case UNARYEXPR_N:
        {
            Type type = evaltype(expr->children[0]);
            if (type != tREAL && type != tINT)
                error("Line %d: Cannont use unary +/- on non int/real operand", expr->line);
            return type;
        }
        case CALL_N:
        {
            Type type = verify_call(expr);
            if (type == tVOID)
                error("Line %d: Cannot evaluate call of procedure %s.", expr->line, expr->children[0]->data);
            return type;
        }
        case ADDRESS_N:
        {
            Type t = evaltype(expr->children[0]);
            if (expr->nchildren == 1)
            {
                if (t != tINT && t != tCHAR && t != tREAL)
                    error("Line %d: operator & must be used on int, char or real.", expr->line);
                return TOPTR(t);
            }
            if (t != tSTRING)
                error("Line %d: operator &[] must be a string.", expr->line);
            if (evaltype(expr->children[1]) != tINT)
                error("Line %d: operator &[] must have a int index", expr->line);
            return tCHARPTR;
        }
        case DEREF_N:
        {
            Type t = evaltype(expr->children[0]);
            if (!ISPTR(t))
                error("Line %d: operator ^ can not be used on non-pointer types", expr->line);
            if (t != tINTPTR && t != tREALPTR && t != tCHARPTR)
                error("Line %d: Pointer error...", expr->line);
            return TOSCALAR(t);
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
    if (!n ||  n->nodetype != CODE_N)
        simple_error("Bad AST was given to analyzer()");
    
    /* Push global scope */
    scope_push();

    for (int i = 0; i < n->nchildren; i++)
        process_node(n->children[i]);

    if (!main_defined)
        simple_error("Main() procedure was not defined.");

    /* Pop global scope */
    scope_pop();

    printf("AST processing finished without issues.\n");
}

/* Central AST processor, in preorder processes statements and declerations */
void process_node(node* n)
{
    static int no_push = 0;
    int i, j, done;
    table_entry* entry;
    node *plist, *vl;
    Type type;

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
            /* Add identifier to current scope */
            if (add_symbol(n->children[CALLABLE_ID], IS_FUNC(n) ? n->children[FUNC_TYPE]->nodetype : tVOID_N) == 0)
                error("Line %d: Identifier %s exists already in current scope", n->line, n->children[CALLABLE_ID]->data);
            /* Get symbol table entry to put additional data/"decoration" on the identifier*/
            entry = find_symbol(n->children[CALLABLE_ID]->data);
            /* Mark identifier as a callable */
            entry->flags |= CALLABLE_FLAG;

            plist = n->children[IS_FUNC(n) ? FUNC_PARAMS : PROC_PARAMS];

            /* Do the Main() checks */
            if (strcmp(entry->identifier, "Main") == 0)
            {   
                if (main_defined)
                    error("Line %d: Main() is already defined", n->line);
                if (plist->nodetype != EMPTY_PARAMLIST)
                    error("Line %d: Main() should not have parameters", n->line);
                if (n->nodetype != PROC_N)
                    error("Line %d: Main() should be a procedure", n->line);
                if (stack->next != NULL)
                    error("Line %d: Main() should be in global scope.", n->line);
                main_defined = 1;
            }
            
            /* Create parameter list for identifier */
            for (i = 0, done = 0; i < plist->nchildren; i++)
            {
                vl = plist->children[i];
                entry->nparameters += vl->nchildren - 1;
                entry->parmeters = realloc(entry->parmeters, entry->nparameters * sizeof(Type));
                type = nodetypetotype(vl->children[vl->nchildren - 1]->nodetype);
                for (j = 0; j < vl->nchildren - 1; j++)
                    entry->parmeters[done++] = type;
            }

            /* Push new scope */
            scope_push();
            /* Set scope return type */
            stack->return_type = entry->type;
            no_push = 1; /* avoids things such as proc f(x:int) { var x: bool; ...} */
            /* Add parameters to new scope's symtable */
            process_node(n->children[IS_FUNC(n) ? FUNC_PARAMS : PROC_PARAMS]);
            /* Process function block */
            process_node(n->children[IS_FUNC(n) ? FUNC_BLOCK : PROC_BLOCK]);
            /* Pop callable scope, Should be popped by ^ BLOCK_N clause */

            /* Logic behind checking if a FUNC has the required return statement at the end
             *          { BLOCK }              <- Function scope block            
             *              |
             * DECLARATION     INNERBLOCK      <- INNERBLOCK needs to exist (not null)
             *      |              |
             *      x     STMT(1) ... STMT(N)  <- Needs to have at least 1 statement
             *                          |
             *                        RETURN   <- Should be a RETURN_N node, typecheck should be already done
             */
            break;
        }
        case RETURN_N:
        {
            if (stack->return_type == tVOID)
                simple_error("Cannot return in a procedure");

            if (evaltype(n->children[0]) != stack->return_type)
                simple_error("Return value type does not match function return type");
            
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
                process_node(n->children[i]);

            if (n->nodetype == BLOCK_N)
                scope_pop();

            break;
        }
        case VARLIST_N:
        {
            NodeType nodetype = n->children[n->nchildren - 1]->nodetype;
            for (i = 0; i < n->nchildren - 1; i++)
            {
                if (add_symbol(n->children[i], nodetype) == 0)
                    simple_error("ID exists already in scope");
            }
            break;
        }
        case IF_N:
        case IFELSE_N:
        case WHILE_N:
        {
            type = evaltype(n->children[0]);

            if (type != tBOOL)
                simple_error("Conditional expression given is not boolean");

            process_node(n->children[1]);

            if (n->nodetype == IFELSE_N)
                process_node(n->children[2]);

            break;
        }
        case ASSIGNMENT_N:
        {
            table_entry* id = find_symbol(n->children[0]->data);

            if (id == NULL)
                error("Line %d: Identifier %s is not defined", n->line, n->children[0]->data);

            if (id->flags & CALLABLE_FLAG)
                error("Line %d: Identifier %s is a procedure/function", n->line, n->children[0]->data);

            type = evaltype(n->children[1]);

            if (!(type == id->type || (ISPTR(id->type) && type == tNULLPTR)))
                simple_error("type mismatch");

            break;
        }
        case ASSIGNMENT_BYINDEX_N:
        {
            table_entry* id = find_symbol(n->children[0]->data);

            if (id == NULL)
                error("Line %d: identifier %s not exsit", n->line, n->children[0]->data);
            if (id->flags & CALLABLE_FLAG)
                simple_error("Identifier is a procedure/function");
            if (id->type != tSTRING)
                simple_error("identifier must be a String.");
            if (evaltype(n->children[1]) != tINT)
                simple_error("index must be a Int.");
            if (evaltype(n->children[2]) != tCHAR)
                simple_error("value must be a char");
            break;
        }
        case ASSIGNMENT_DEREF_N:
        {
            Type to, from;
            to = evaltype(n->children[0]);
            from = evaltype(n->children[1]);
            if (!ISPTR(to))
                simple_error("Destination is not a pointer");
            to = TOSCALAR(to);
            if (to != from)
                simple_error("Type mismatch");
            if (to != tINT && to != tREAL && to != tCHAR)
                simple_error("Destination type is not pointer to int, real or char");
            break;
        }
        case CALL_N:
        {
            verify_call(n);
            break;
        }
        default:
            /* UNREACHABLE */
            assert(!n->nodetype);
            break;
    }
}
/* End of AST Semantic analysis code */
