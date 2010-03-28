using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sicfdn.Parser;

namespace Sicfdn.Test
{
    [TestClass]
    public class ParserTest
    {
        [TestMethod]
        public void ParserFactorialTest()
        {
            string expectedOutput = @"
Program: 
    AssignmentStatement: 
        Identifier: factorial
        FunctionDeclaration: 
            ArgumentList: 
                Identifier: x
            StatementList: 
                IfStatement: 
                    BinaryExpression: 
                        Identifier: ||
                        BinaryExpression: 
                            Identifier: ==
                            Identifier: x
                            Number: 0
                        BinaryExpression: 
                            Identifier: ==
                            Identifier: x
                            Number: 1
                    ReturnStatement: 
                        Number: 1
                    ReturnStatement: 
                        BinaryExpression: 
                            Identifier: *
                            Identifier: x
                            FunctionCall: 
                                Identifier: factorial
                                BinaryExpression: 
                                    Identifier: -
                                    Identifier: x
                                    Number: 1
";
            string factorialCode = @"
factorial = function(x) 
{
   if (x == 0 || x == 1) return 1;
   else return x * factorial(x - 1);
};
";
            AstNode parsed = Parser.Parser.Parse(Parser.Lexer.Lex(factorialCode));
            Assert.AreEqual(expectedOutput.Trim(), parsed.ToString().Trim());
        }
    }
}
