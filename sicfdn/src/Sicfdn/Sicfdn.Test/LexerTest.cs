using System;
using System.Text;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sicfdn.Parser;
using Sicfdn.Utils;

namespace Sicfdn.Test
{
    /// <summary>
    /// Summary description for UnitTest1
    /// </summary>
    [TestClass]
    public class LexerTest
    {
        private void TestLexer(string[] expected, string strToLex)
        {
            foreach (Pair<string, Token> testPair in Utils.Utils.Zip<string, Token>(expected, Lexer.Lex(strToLex)))
            {
                Assert.AreEqual(testPair.First, testPair.Second.ToString());
            }
        }

        [TestMethod]
        public void TestLexerSimple()
        {
            string[] expectedTokens = 
            {
                "Identifier ('function', between 0 and 7)",
                "Identifier ('test', between 9 and 12)",
                "OpenParen ('(', between 13 and 13)",
                "Identifier ('x', between 14 and 14)",
                "CloseParen (')', between 15 and 15)",
                "OpenBrace ('{', between 17 and 17)",
                "Identifier ('return', between 19 and 24)",
                "Identifier ('x', between 26 and 26)",
                "Plus ('+', between 28 and 28)",
                "Identifier ('x', between 30 and 30)",
                "Semicolon (';', between 31 and 31)",
                "CloseBrace ('}', between 33 and 33)"
            };
            TestLexer(expectedTokens, "function test(x) { return x + x; }");
        }

        [TestMethod]
        public void TestLexerComplex()
        {
            string[] expectedTokens = 
            {
                "Identifier ('function', between 2 and 9)",
                "Identifier ('factorial', between 11 and 19)",
                "OpenParen ('(', between 20 and 20)",
                "Identifier ('x', between 21 and 21)",
                "CloseParen (')', between 22 and 22)",
                "OpenBrace ('{', between 26 and 26)",
                "Identifier ('if', between 32 and 33)",
                "OpenParen ('(', between 35 and 35)",
                "Identifier ('x', between 36 and 36)",
                "RelationalEq ('==', between 38 and 39)",
                "Number ('0', between 41 and 41)",
                "LogicalOr ('||', between 43 and 44)",
                "Identifier ('x', between 46 and 46)",
                "RelationalEq ('==', between 48 and 49)",
                "Number ('1', between 51 and 51)",
                "CloseParen (')', between 52 and 52)",
                "Identifier ('return', between 54 and 59)",
                "Number ('1', between 61 and 61)",
                "Semicolon (';', between 62 and 62)",
                "Identifier ('else', between 68 and 71)",
                "Identifier ('return', between 73 and 78)",
                "Identifier ('x', between 80 and 80)",
                "Multiply ('*', between 82 and 82)",
                "Identifier ('factorial', between 84 and 92)",
                "OpenParen ('(', between 93 and 93)",
                "Identifier ('x', between 94 and 94)",
                "Number ('-', between 96 and 96)",
                "Number ('1', between 98 and 98)",
                "CloseParen (')', between 99 and 99)",
                "Semicolon (';', between 100 and 100)",
                "CloseBrace ('}', between 103 and 103)"
            };
            TestLexer(expectedTokens, @"
function factorial(x) 
{
   if (x == 0 || x == 1) return 1;
   else return x * factorial(x - 1);
}
");
        }
    }
}
