using System;
using System.Collections.Generic;
using System.Text;
using Sicfdn.Parser;
using Sicfdn.Utils;

namespace Sicfdn.Interpreter
{
    public class Interpreter
    {
        public static void Interpret(AstNode program, Environment env)
        {
            if (program.Type != AstNodeType.Program)
            {
                throw new InterpreterException("Expected a program.");
            }
            InterpretStatementList(program.Children, env);
        }

        // Returns a pair of <continue, result-of-statement>.
        // Following statements are executed only while continue is true.
        internal static Pair<bool, object> InterpretStatementList(
            List<AstNode> statements, Environment env)
        {
            foreach (AstNode statement in statements)
            {
                Pair<bool, object> result = InterpretStatement(statement, env);
                if (!result.First)
                {
                    return result;
                }
            }
            return new Pair<bool, object>(true, null);
        }

        private static Pair<bool, object> InterpretStatement(AstNode statement, Environment env)
        {
            switch (statement.Type)
            {
                case AstNodeType.FunctionCall:
                    return new Pair<bool, object>(
                        true,
                        EvaluateFunctionCall(statement, env));
                case AstNodeType.AssignmentStatement:
                    env.AssignLocal(
                        statement.Children[0].Token.Content,
                        EvaluateExpression(statement.Children[1], env));
                    return new Pair<bool, object>(true, null);
                case AstNodeType.IfStatement:
                    return InterpretIfStatement(statement, env);
                case AstNodeType.CompoundStatement:
                    return InterpretStatementList(statement.Children, env);
                case AstNodeType.ReturnStatement:
                    return new Pair<bool, object>(
                        false, 
                        EvaluateExpression(statement.Children[0], env));
                default:
                    throw new InterpreterException(String.Format(
                        "Invalid statement {0}.", statement.Type));
            }
        }

        private static Pair<bool, object> InterpretIfStatement(AstNode statement, Environment env)
        {
            if (TruthFunction(EvaluateExpression(statement.Children[0], env)))
            {
                return InterpretStatement(statement.Children[1], env);
            }
            else if (statement.Children.Count == 3)
            {
                return InterpretStatement(statement.Children[2], env);
            }
            return new Pair<bool, object>(true, null);
        }

        private static object EvaluateFunctionCall(AstNode functionCallNode, Environment env)
        {
            AstNode functionNode = functionCallNode.Children[0];

            if (functionNode.Type == AstNodeType.Identifier)
            {
                string functionName = functionNode.Token.Content;
                if (functionName == "print")
                {
                    // execute our only built-in :-)
                    object valueToPrint = EvaluateExpression(functionCallNode.Children[1], env);
                    if (valueToPrint != null)
                    {
                        Console.WriteLine(valueToPrint.ToString());
                    }
                    else
                    {
                        Console.WriteLine("<null>");
                    }
                    return null;
                }
            }
            FunctionValue function = EvaluateExpression(functionNode, env) as FunctionValue;
            if (function == null)
            {
                throw new InterpreterException("Object is not a function.");
            }

            List<object> argumentValues = new List<object>();
            for (int i = 1; i < functionCallNode.Children.Count; i++) 
            {
                AstNode argExpression = functionCallNode.Children[i];
                argumentValues.Add(EvaluateExpression(argExpression, env));
            }

            return function.Call(argumentValues);
        }

        private static bool TruthFunction(object objectToEvaluate)
        {
            return objectToEvaluate.ToString() != "0";
        }

        private static object EvaluateExpression(AstNode astNode, Environment env)
        {
            switch (astNode.Type)
            {
                case AstNodeType.BinaryExpression:
                    return EvaluateBinaryExpression(astNode, env);
                case AstNodeType.UnaryExpression:
                    return EvaluateUnaryExpression(astNode, env);
                case AstNodeType.FunctionCall:
                    return EvaluateFunctionCall(astNode, env);
                case AstNodeType.Number:
                    return Int32.Parse(astNode.Token.Content);
                case AstNodeType.FunctionDeclaration:
                    return EvaluateFunctionDeclaration(astNode, env);
                case AstNodeType.Identifier:
                    return EvaluateIdentifier(astNode, env);
                default:
                    throw new InterpreterException(String.Format(
                        "Unknown kind of Expression '{0}'.", astNode.Type));
            }
        }

        private static object EvaluateIdentifier(AstNode astNode, Environment env)
        {
            string varName = astNode.Token.Content;
            if (varName == "null")
            {
                return null;
            }
            Pair<bool, object> result = env.LookUp(varName);
            if (result.First)
            {
                return result.Second;
            }
            else
            {
                throw new InterpreterException(String.Format(
                    "Cannot find variable '{0}'.", varName));
            }
        }

        private static object EvaluateFunctionDeclaration(AstNode astNode, Environment env)
        {
            List<string> argList = new List<string>();
            foreach (AstNode argNode in astNode.Children[0].Children)
            {
                argList.Add(argNode.Token.Content);
            }
            return new FunctionValue(astNode.Children[1].Children, env, argList);
        }

        private static object EvaluateUnaryExpression(AstNode astNode, Environment env)
        {
            string unaryOp = astNode.Children[0].Token.Content;
            switch (unaryOp)
            {
                case "-":
                    return -EvaluateInt(EvaluateExpression(astNode.Children[1], env));
                case "!":
                    return !TruthFunction(EvaluateExpression(astNode.Children[1], env)) ? 1 : 0;
            }
            throw new InterpreterException(String.Format(
                "Unknown unary operator '{0}'.", unaryOp));
        }

        private static object EvaluateBinaryExpression(AstNode astNode, Environment env)
        {
            string binaryOp = astNode.Children[0].Token.Content;
            object rawOperand1 = EvaluateExpression(astNode.Children[1], env);
            object rawOperand2 = EvaluateExpression(astNode.Children[2], env);
            int operand1 = EvaluateInt(rawOperand1);
            int operand2 = EvaluateInt(rawOperand2);
            switch (binaryOp)
            {
                case "+":
                    return operand1 + operand2;
                case "-":
                    return operand1 - operand2;
                case "*":
                    return operand1 * operand2;
                case "/":
                    return operand1 / operand2;
                case "<":
                    return (operand1 < operand2) ? 1 : 0;
                case "<=":
                    return (operand1 <= operand2) ? 1 : 0;
                case "==":
                    return (object.Equals(rawOperand1, rawOperand2)) ? 1 : 0;
                case "!=":
                    return !(object.Equals(rawOperand1, rawOperand2)) ? 1 : 0;
                case ">=":
                    return (operand1 >= operand2) ? 1 : 0;
                case ">":
                    return (operand1 > operand2) ? 1 : 0;
                case "||":
                    return (TruthFunction(rawOperand1) || TruthFunction(rawOperand2)) ? 1 : 0;
                case "&&":
                    return (TruthFunction(rawOperand1) && TruthFunction(rawOperand2)) ? 1 : 0;
            }
            throw new InterpreterException(String.Format(
                "Unknown binary operator '{0}'.", binaryOp));
        }

        private static int EvaluateInt(object obj)
        {
            if (obj is Int32)
            {
                return (Int32)obj;
            }
            else
            {
                return 0;
            }
        }
    }
}
