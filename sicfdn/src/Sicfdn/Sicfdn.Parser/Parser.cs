using System;
using System.Collections.Generic;
using System.Text;

namespace Sicfdn.Parser
{
    public class Parser
    {
        public static AstNode Parse(IEnumerable<Token> tokens)
        {
            return ParseProgram(new TokenStream(tokens));
        }

        private static AstNode ParseProgram(TokenStream tokenStream)
        {
            List<AstNode> children = new List<AstNode>();
            while (tokenStream.HasMoreTokens)
            {
                children.Add(ParseStatement(tokenStream));
            }
            return new AstNode(AstNodeType.Program, children);
        }

        private static AstNode ParseStatement(TokenStream tokenStream)
        {
            CheckEnd(tokenStream);
            if (tokenStream.Current.Type == TokenType.Identifier)
            {
                switch (tokenStream.Current.Content)
                {
                    case "return":
                        return ParseReturnStatement(tokenStream);
                    case "function":
                        return ParseFunctionDeclaration(tokenStream);
                    case "if":
                        return ParseIfStatement(tokenStream);
                    default:
                        return ParseAssignmentOrFunctionCallStatement(tokenStream);
                }
            }
            else
            {
                throw new ParserException(string.Format(
                    "Expected a statement at {0}.", tokenStream.Current.Start));
            }
        }

        private static AstNode ParseAssignmentOrFunctionCallStatement(TokenStream tokenStream)
        {
            CheckEnd(tokenStream);
            AstNode tokenNode = new AstNode(AstNodeType.Identifier, tokenStream.Current);
            tokenStream.Next();
            CheckEnd(tokenStream);
            if (tokenStream.Current.Type == TokenType.Equal)
            {
                return ParseAssignmentStatement(tokenNode, tokenStream);
            }
            else if (tokenStream.Current.Type == TokenType.OpenParen)
            {
                AstNode result = ParseFunctionCallExpression(tokenNode, tokenStream);
                MatchToken(TokenType.Semicolon, ";", tokenStream);
                return result;
            }
            else
            {
                throw new ParserException(String.Format(
                    "Expected an assignment or function call at {0}.", tokenNode.Token.Start));
            }
        }

        private static AstNode ParseFunctionCallExpression(AstNode function, TokenStream tokenStream)
        {
            MatchToken(TokenType.OpenParen, "(", tokenStream);
            List<AstNode> arguments = ParseExpressionList(tokenStream);
            MatchToken(TokenType.CloseParen, ")", tokenStream);
            arguments.Insert(0, function);
            return new AstNode(AstNodeType.FunctionCall, arguments);
        }

        private static List<AstNode> ParseExpressionList(TokenStream tokenStream)
        {
            List<AstNode> result = new List<AstNode>();
            CheckEnd(tokenStream);
            bool finished = tokenStream.Current.Type == TokenType.CloseParen;
            while (!finished)
            {
                result.Add(ParseExpression(tokenStream));
                CheckEnd(tokenStream);
                if (tokenStream.Current.Type == TokenType.CloseParen)
                {
                    finished = true;
                }
                else
                {
                    MatchToken(TokenType.Comma, ",", tokenStream);
                }
            }
            return result;
        }

        private static AstNode ParseAssignmentStatement(AstNode tok, TokenStream tokenStream)
        {
            MatchToken(TokenType.Equal, "=", tokenStream);
            AstNode value = ParseExpression(tokenStream);
            MatchToken(TokenType.Semicolon, ";", tokenStream);
            return new AstNode(AstNodeType.AssignmentStatement, new AstNode[] { tok, value });
        }

        private static AstNode ParseIfStatement(TokenStream tokenStream)
        {
            MatchToken(TokenType.Identifier, "if", tokenStream);
            MatchToken(TokenType.OpenParen, "(", tokenStream);
            AstNode condition = ParseExpression(tokenStream);
            MatchToken(TokenType.CloseParen, ")", tokenStream);
            AstNode thenClause = ParseStatement(tokenStream);
            if (tokenStream.HasMoreTokens)
            {
                if (tokenStream.Current.Type == TokenType.Identifier
                    && tokenStream.Current.Content == "else")
                {
                    MatchToken(TokenType.Identifier, "else", tokenStream);
                    AstNode elseClause = ParseStatement(tokenStream);
                    return new AstNode(
                        AstNodeType.IfStatement, new AstNode[] { condition, thenClause, elseClause });
                }
            }
            return new AstNode(
                AstNodeType.IfStatement, new AstNode[] { condition, thenClause });
        }

        private static AstNode ParseFunctionDeclaration(TokenStream tokenStream)
        {
            MatchToken(TokenType.Identifier, "function", tokenStream);
            MatchToken(TokenType.OpenParen, "(", tokenStream);
            AstNode arguments = ParseArgumentList(tokenStream);
            MatchToken(TokenType.CloseParen, ")", tokenStream);
            MatchToken(TokenType.OpenBrace, "{", tokenStream);
            AstNode body = ParseStatementList(tokenStream);
            MatchToken(TokenType.CloseBrace, "}", tokenStream);
            return new AstNode(AstNodeType.FunctionDeclaration, new AstNode[] { arguments, body });
        }

        private static AstNode ParseStatementList(TokenStream tokenStream)
        {
            List<AstNode> statements = new List<AstNode>();
            CheckEnd(tokenStream);
            while (tokenStream.Current.Type != TokenType.CloseBrace)
            {
                statements.Add(ParseStatement(tokenStream));
            }
            return new AstNode(AstNodeType.StatementList, statements);
        }

        private static AstNode ParseArgumentList(TokenStream tokenStream)
        {
            CheckEnd(tokenStream);
            List<AstNode> arguments = new List<AstNode>();
            bool finish = tokenStream.Current.Type == TokenType.CloseParen;
            while (!finish)
            {
                if (tokenStream.Current.Type == TokenType.Identifier)
                {
                    arguments.Add(new AstNode(AstNodeType.Identifier, tokenStream.Current));
                }
                else
                {
                    throw new ParserException(string.Format("Expected an identifier at {0}.", tokenStream.Current.Start));
                }
                tokenStream.Next();
                CheckEnd(tokenStream);
                if (tokenStream.Current.Type == TokenType.CloseParen)
                {
                    finish = true;
                }
                else
                {
                    MatchToken(TokenType.Comma, ",", tokenStream);
                }
            }
            return new AstNode(AstNodeType.ArgumentList, arguments);
        }

        private static AstNode ParseReturnStatement(TokenStream tokenStream)
        {
            MatchToken(TokenType.Identifier, "return", tokenStream);
            AstNode value = ParseExpression(tokenStream);
            MatchToken(TokenType.Semicolon, ";", tokenStream);
            return new AstNode(AstNodeType.ReturnStatement, new AstNode[] { value });
        }

        private delegate AstNode ParseFunction(TokenStream tokenStream);

        private static AstNode ParseExpression(TokenStream tokenStream)
        {
            CheckEnd(tokenStream);
            Token tok = tokenStream.Current;
            if (tok.Type == TokenType.Identifier && tok.Content == "function")
            {
                return ParseFunctionDeclaration(tokenStream);
            }
            else
            {
                return LeftAssocCombiner(new string[] { "||" }, ParseExprLogicalAnd, tokenStream);
            }
        }

        private static AstNode ParseExprLogicalAnd(TokenStream tokenStream)
        {
            return LeftAssocCombiner(new string[] { "&&" }, ParseExprRelational, tokenStream);
        }

        private static AstNode ParseExprRelational(TokenStream tokenStream)
        {
            return LeftAssocCombiner(new string[] { "<", "<=", "==", "!=", ">=", ">" }, ParseExprSum, tokenStream);
        }

        private static AstNode ParseExprSum(TokenStream tokenStream)
        {
            return LeftAssocCombiner(new string[] { "+", "-" }, ParseExprMult, tokenStream);
        }

        private static AstNode ParseExprMult(TokenStream tokenStream)
        {
            return LeftAssocCombiner(new string[] { "*", "/" }, ParseExprUnary, tokenStream);
        }

        private static AstNode ParseExprUnary(TokenStream tokenStream)
        {
            CheckEnd(tokenStream);
            AstNode result;
            if (tokenStream.Current.Type == TokenType.OpenParen)
            {
                result = ParseParenExpression(tokenStream);
            }
            else if (tokenStream.Current.Type == TokenType.Minus
                || tokenStream.Current.Type == TokenType.LogicalNot)
            {
                AstNode tokenNode = new AstNode(AstNodeType.Identifier, tokenStream.Current);
                tokenStream.Next();
                AstNode negatedExpression = ParseExprUnary(tokenStream);
                result = new AstNode(AstNodeType.UnaryExpression, new AstNode[] { tokenNode, negatedExpression });
            }
            else if (tokenStream.Current.Type == TokenType.Identifier)
            {
                AstNode tokenNode = new AstNode(AstNodeType.Identifier, tokenStream.Current);
                tokenStream.Next();
                if (tokenStream.HasMoreTokens && tokenStream.Current.Type == TokenType.OpenParen)
                {
                    result = ParseFunctionCallExpression(tokenNode, tokenStream);
                }
                else
                {
                    result = tokenNode;
                }
            }
            else if (tokenStream.Current.Type == TokenType.Number)
            {
                result = new AstNode(AstNodeType.Number, tokenStream.Current);
                tokenStream.Next();
            }
            else
            {
                throw new ParserException(String.Format(
                    "Expecting an unary expression at {0}.",
                    tokenStream.Current.Start));
            }
            while (tokenStream.HasMoreTokens && tokenStream.Current.Type == TokenType.OpenParen)
            {
                result = ParseFunctionCallExpression(result, tokenStream);
            }
            return result;
        }

        private static AstNode ParseParenExpression(TokenStream tokenStream)
        {
            MatchToken(TokenType.OpenParen, "(", tokenStream);
            CheckEnd(tokenStream);
            if (tokenStream.Current.Content == "function")
            {
                AstNode funDef = ParseFunctionDeclaration(tokenStream);
                MatchToken(TokenType.CloseParen, ")", tokenStream);
                return ParseFunctionCallExpression(funDef, tokenStream);
            }
            else
            {
                AstNode result = ParseExpression(tokenStream);
                MatchToken(TokenType.CloseParen, ")", tokenStream);
                return result;
            }
        }

        private static AstNode LeftAssocCombiner(
            string[] operators,
            ParseFunction termParser,
            TokenStream tokenStream)
        {
            AstNode leftTerm = termParser(tokenStream);
            while (tokenStream.HasMoreTokens && OneOf(tokenStream.Current.Content, operators))
            {
                AstNode tokenNode = new AstNode(AstNodeType.Identifier, tokenStream.Current);
                tokenStream.Next();
                AstNode rightTerm = termParser(tokenStream);
                leftTerm = new AstNode(
                    AstNodeType.BinaryExpression,
                    new AstNode[] { tokenNode, leftTerm, rightTerm });
            }
            return leftTerm;
        }

        private static bool OneOf(string op, string[] operators)
        {
            foreach (string acceptedOperator in operators)
            {
                if (op == acceptedOperator)
                {
                    return true;
                }
            }
            return false;
        }


        private static void MatchToken(TokenType tokenType, string content, TokenStream tokenStream)
        {
            CheckEnd(tokenStream);
            if (tokenStream.Current.Type == tokenType && tokenStream.Current.Content == content)
            {
                tokenStream.Next();
            }
            else
            {
                throw new ParserException(String.Format(
                    "Expected '{0}' at position {1}.", content, tokenStream.Current.Start));
            }
        }

        private static void CheckEnd(TokenStream tokenStream)
        {
            if (!tokenStream.HasMoreTokens)
            {
                throw new ParserException("Unexpected end of input.");
            }
        }

    }
}
