using System;
using System.Collections.Generic;
using System.Text;

namespace Sicfdn.Parser
{
    public enum TokenType
    {
        Identifier,
        Number,
        OpenParen,
        CloseParen,
        Comma,
        Semicolon,
        OpenBrace,
        CloseBrace,
        RelationalLt,
        RelationalLte,
        RelationalEq,
        RelationalNeq,
        RelationalGte,
        RelationalGt,
        Plus,
        Minus,
        Multiply,
        Divide,
        LogicalAnd,
        LogicalOr,
        LogicalNot,
        Equal // for assignment
    }
}
