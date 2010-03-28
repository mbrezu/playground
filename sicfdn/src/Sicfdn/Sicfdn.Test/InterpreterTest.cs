using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sicfdn.Parser;

namespace Sicfdn.Test
{
    [TestClass]
    public class InterpreterTest
    {
        [TestMethod]
        public void TestInterpreterBasic()
        {
            string str = @"
x = 1;
y = 2;
if (x < y) r1 = 1;
else r2 = 2;
b = function(x) { return function(y) { return x + y; }; };
x = 2;
r3 = b(x)(y);
";
            AstNode parsed = Parser.Parser.Parse(Sicfdn.Parser.Lexer.Lex(str));
            Sicfdn.Interpreter.Environment env = new Sicfdn.Interpreter.Environment(null);
            Interpreter.Interpreter.Interpret(parsed, env);

            Assert.AreEqual(true, env.LookUp("r1").First);
            Assert.AreEqual(1, env.LookUp("r1").Second);

            Assert.AreEqual(false, env.LookUp("r2").First);

            Assert.AreEqual(true, env.LookUp("r3").First);
            Assert.AreEqual(4, env.LookUp("r3").Second);
        }

        [TestMethod]
        public void TestInterpreterFactorial()
        {
            string str = @"
fact = function(n) {
    if (n == 1 || n == 0) return 1;
    else return n * fact(n - 1);
};
r1 = fact(10);
";
            AstNode parsed = Parser.Parser.Parse(Sicfdn.Parser.Lexer.Lex(str));
            Sicfdn.Interpreter.Environment env = new Sicfdn.Interpreter.Environment(null);
            Interpreter.Interpreter.Interpret(parsed, env);

            Assert.AreEqual(true, env.LookUp("r1").First);
            Assert.AreEqual(3628800, env.LookUp("r1").Second);
        }

        [TestMethod]
        public void TestInterpreterFibonacci()
        {
            string str = @"
fib = function(n) {
    if (n == 0 || n == 1) return 1;
    else return fib(n - 1) + fib(n - 2);
};
r1 = fib(10);
";
            AstNode parsed = Parser.Parser.Parse(Sicfdn.Parser.Lexer.Lex(str));
            Sicfdn.Interpreter.Environment env = new Sicfdn.Interpreter.Environment(null);
            Interpreter.Interpreter.Interpret(parsed, env);

            Assert.AreEqual(true, env.LookUp("r1").First);
            Assert.AreEqual(89, env.LookUp("r1").Second);
        }

        [TestMethod]
        public void TestInterpreterConsCarCdr()
        {
            string str = @"
cons = function(a, b) {
    return function(n) {
        if (n == 1) return a;
        else return b;
    };
};
car = function(pair) {
    return pair(1);
};
cdr = function(pair) {
    return pair(2);
};
pair = cons(10, 20);
r1 = car(pair);
r2 = cdr(pair);
";
            AstNode parsed = Parser.Parser.Parse(Sicfdn.Parser.Lexer.Lex(str));
            Sicfdn.Interpreter.Environment env = new Sicfdn.Interpreter.Environment(null);
            Interpreter.Interpreter.Interpret(parsed, env);

            Assert.AreEqual(true, env.LookUp("r1").First);
            Assert.AreEqual(10, env.LookUp("r1").Second);

            Assert.AreEqual(true, env.LookUp("r2").First);
            Assert.AreEqual(20, env.LookUp("r2").Second);
        }

        [TestMethod]
        public void TestInterpreterListLength()
        {
            string str = @"
cons = function(a, b) {
    return function(n) {
        if (n == 1) return a;
        else return b;
    };
};
car = function(pair) {
    return pair(1);
};
cdr = function(pair) {
    return pair(2);
};
length = function(list) {
    if (list == null) return 0;
    else return 1 + length(cdr(list));
};
r1 = length(cons(1, cons(2, null)));
r2 = length(null);
";
            AstNode parsed = Parser.Parser.Parse(Sicfdn.Parser.Lexer.Lex(str));
            Sicfdn.Interpreter.Environment env = new Sicfdn.Interpreter.Environment(null);
            Interpreter.Interpreter.Interpret(parsed, env);

            Assert.AreEqual(true, env.LookUp("r1").First);
            Assert.AreEqual(2, env.LookUp("r1").Second);

            Assert.AreEqual(true, env.LookUp("r2").First);
            Assert.AreEqual(0, env.LookUp("r2").Second);
        }
        [TestMethod]
        public void TestInterpreterGcd()
        {
            string str = @"
gcd = function(a, b) {
    if (b == 0) return a;
    else return gcd(b, a - (a / b) * b);
};
r1 = gcd(35, 14);
r2 = gcd(7, 5);
r3 = gcd(100, 24);
";
            AstNode parsed = Parser.Parser.Parse(Sicfdn.Parser.Lexer.Lex(str));
            Sicfdn.Interpreter.Environment env = new Sicfdn.Interpreter.Environment(null);
            Interpreter.Interpreter.Interpret(parsed, env);

            Assert.AreEqual(true, env.LookUp("r1").First);
            Assert.AreEqual(7, env.LookUp("r1").Second);

            Assert.AreEqual(true, env.LookUp("r2").First);
            Assert.AreEqual(1, env.LookUp("r2").Second);

            Assert.AreEqual(true, env.LookUp("r3").First);
            Assert.AreEqual(4, env.LookUp("r3").Second);

        }
    }
}
