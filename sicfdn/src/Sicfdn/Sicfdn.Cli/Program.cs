using System;
using System.Collections.Generic;
using System.Text;
using Sicfdn.Parser;

namespace Sicfdn.Cli
{
    class Program
    {
        static void Main(string[] args)
        {
            //TestParserHelper();
            TestInterpreterHelper();
        }

        private static void TestInterpreterHelper()
        {
            string str = @"
gcd = function(a, b) {
    if (b == 0) return a;
    else return gcd(b, a - (a / b) * b);
};
print(gcd(102, 24));
";
            AstNode parsed = Parser.Parser.Parse(Sicfdn.Parser.Lexer.Lex(str));
            Interpreter.Interpreter.Interpret(parsed, new Sicfdn.Interpreter.Environment(null));
        }

        private static void TestParserHelper()
        {
            string str = @"
factorial = function(x) 
{
   if (x == 0 || x == 1) return 1;
   else return x * factorial(x - 1);
};
";
            //            string str = @"
            //factorial = function(x) 
            //{
            //   if (x) return 1;
            //   else return x;
            //};
            //";
            List<Token> tokens = new List<Token>();
            foreach (Token tok in Sicfdn.Parser.Lexer.Lex(str))
            {
                Console.WriteLine(tok);
            }

            AstNode parsed = Parser.Parser.Parse(Sicfdn.Parser.Lexer.Lex(str));
            Console.WriteLine(parsed);
        }
    }
}
