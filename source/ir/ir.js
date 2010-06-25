/**
@fileOverview
Intermediate Representation (IR) translation implementation

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// TODO: Explain result of translation in function description comments

/**
Convert an AST code unit into IR functions
*/
function UnitToIR(astProg)
{
    // TODO
    //
    // Top level AST may contain functions and statements...
    // Top level AST is somewhat like a function of its own
    //
    // Parse into a top-level AST function per code unit? 
    // - Only called once
    //   - This ptr is always the global object
    // - Inner functions are nested
    // - Has variable declarations which are global...
    //   - Indicated by symbol resolution of AST
    // - Return value is irrelevant
    //   - Return is actually illegal at global scope
    //



}

/**
Convert an AST function into an IR function
*/
function FuncToIR(astFunc)
{
    // TODO



}

/**
Convert an AST statement into IR code
*/
function StmtToIR(astStmt)
{
    // TODO
    // Take entry and exit basic blocks as input?

}

/**
Convert an AST expression into IR code
*/
function ExprToIR(astExpr)
{
    // TODO
    // Take entry and exit basic blocks as input?

}

