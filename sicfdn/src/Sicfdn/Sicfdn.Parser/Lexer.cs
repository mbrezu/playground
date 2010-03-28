using System;
using System.Collections.Generic;
using System.Text;

namespace Sicfdn.Parser
{
    public class Lexer
    {
        public static IEnumerable<Token> Lex(string program)
        {
            int charIndex = 0;
            while (charIndex < program.Length)
            {
                char ch = program[charIndex];
                if (Char.IsWhiteSpace(ch))
                {
                    charIndex++;
                }
                else if (Char.IsDigit(ch) || ch == '-')
                {
                    int start = LexNumber(program, ref charIndex);
                    yield return NewToken(
                        TokenType.Number, program, start, charIndex - 1);
                }
                else if (Char.IsLetter(ch) || ch == '_')
                {
                    int start = LexIdentifier(program, ref charIndex);
                    yield return NewToken(
                        TokenType.Identifier, program, start, charIndex - 1);
                }
                else if (ch == '<')
                {
                    yield return LexMaybeDoubleChar(
                        program, 
                        "<=", 
                        TokenType.RelationalLte, 
                        TokenType.RelationalLt, 
                        ref charIndex);
                }
                else if (ch == '>')
                {
                    yield return LexMaybeDoubleChar(
                        program,
                        ">=",
                        TokenType.RelationalGte,
                        TokenType.RelationalGt,
                        ref charIndex);
                }
                else if (ch == '=')
                {
                    yield return LexMaybeDoubleChar(
                        program,
                        "==",
                        TokenType.RelationalEq,
                        TokenType.Equal,
                        ref charIndex);
                }
                else if (ch == '!')
                {
                    yield return LexMaybeDoubleChar(
                        program, 
                        "!=", 
                        TokenType.RelationalNeq, 
                        TokenType.LogicalNot, 
                        ref charIndex);
                }
                else if (ch == '|')
                {
                    yield return LexDoubleChar(program, "||", TokenType.LogicalOr, ref charIndex);
                }
                else if (ch == '&')
                {
                    yield return LexDoubleChar(program, "&", TokenType.LogicalAnd, ref charIndex);
                }
                else
                {
                    switch (ch)
                    {
                        case '(':
                            charIndex++;
                            yield return NewToken(TokenType.OpenParen, program, charIndex - 1, charIndex - 1);
                            break;
                        case ')':
                            charIndex++;
                            yield return NewToken(TokenType.CloseParen, program, charIndex - 1, charIndex - 1);
                            break;
                        case '{':
                            charIndex++;
                            yield return NewToken(TokenType.OpenBrace, program, charIndex - 1, charIndex - 1);
                            break;
                        case '}':
                            charIndex++;
                            yield return NewToken(TokenType.CloseBrace, program, charIndex - 1, charIndex - 1);
                            break;
                        case '-':
                            charIndex++;
                            yield return NewToken(TokenType.Minus, program, charIndex - 1, charIndex - 1);
                            break;
                        case '+':
                            charIndex++;
                            yield return NewToken(TokenType.Plus, program, charIndex - 1, charIndex - 1);
                            break;
                        case '*':
                            charIndex++;
                            yield return NewToken(TokenType.Multiply, program, charIndex - 1, charIndex - 1);
                            break;
                        case '/':
                            charIndex++;
                            yield return NewToken(TokenType.Divide, program, charIndex - 1, charIndex - 1);
                            break;
                        case ',':
                            charIndex++;
                            yield return NewToken(TokenType.Comma, program, charIndex - 1, charIndex - 1);
                            break;
                        case ';':
                            charIndex++;
                            yield return NewToken(TokenType.Semicolon, program, charIndex - 1, charIndex - 1);
                            break;
                        default:
                            throw new ParserException(String.Format("Unexpected character at position {0}.", charIndex));
                    }
                }
            }
        }

        private static Token LexMaybeDoubleChar(
            string program, 
            string doubleCharToken,
            TokenType tokenTypeDouble,
            TokenType tokenTypeSimple,
            ref int charIndex)
        {
            Token tok;
            if (charIndex + 1 < program.Length && program[charIndex + 1] == doubleCharToken[1])
            {
                charIndex += 2;
                tok = NewToken(tokenTypeDouble, program, charIndex - 2, charIndex - 1);
            }
            else
            {
                charIndex++;
                tok = NewToken(tokenTypeSimple, program, charIndex - 1, charIndex - 1);
            }
            return tok;
        }

        private static Token LexDoubleChar(
            string program, 
            string doubleCharToken, 
            TokenType tokenType, 
            ref int charIndex)
        {
            Token tok;
            if (charIndex + 1 < program.Length && program[charIndex + 1] == doubleCharToken[1])
            {
                charIndex += 2;
                tok = NewToken(tokenType, program, charIndex - 2, charIndex - 1);
            }
            else
            {
                throw new ParserException(String.Format("Unexpected character at position {0}.", charIndex));
            }
            return tok;
        }

        private static Token NewToken(TokenType type, string program, int start, int end)
        {
            return new Token(type, start, end, program.Substring(start, end - start + 1));
        }

        private static int LexIdentifier(string program, ref int charIndex)
        {
            int start = charIndex;
            charIndex++;
            while (charIndex < program.Length
                && (Char.IsLetterOrDigit(program[charIndex]) || program[charIndex] == '_'))
            {
                charIndex++;
            }
            return start;
        }

        private static int LexNumber(string program, ref int charIndex)
        {
            int start = charIndex;
            charIndex++;
            while (charIndex < program.Length && Char.IsDigit(program[charIndex]))
            {
                charIndex++;
            }
            if (charIndex < program.Length && program[charIndex] == '.')
            {
                charIndex++;
                while (charIndex < program.Length && Char.IsDigit(program[charIndex]))
                {
                    charIndex++;
                }
            }
            return start;
        }

        private static int LexNumber(string program, List<Token> result, int charIndex)
        {
            return charIndex;
        }
    }
}
