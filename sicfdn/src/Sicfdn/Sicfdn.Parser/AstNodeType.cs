using System;
using System.Collections.Generic;
using System.Text;

namespace Sicfdn.Parser
{
    public enum AstNodeType
    {
        Program,

        FunctionDeclaration,
        FunctionCall,
        AssignmentStatement,
        VarAssignmentStatement,
        PrintStatement,
        IfStatement,
        CompoundStatement,
        ReturnStatement,

        Expression,
        BinaryExpression,
        UnaryExpression,

        Identifier,
        Number,

        ArgumentList,
        StatementList
    }
}
