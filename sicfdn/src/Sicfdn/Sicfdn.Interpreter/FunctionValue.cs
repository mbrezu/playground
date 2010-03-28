using System;
using System.Collections.Generic;
using System.Text;
using Sicfdn.Parser;
using Sicfdn.Utils;

namespace Sicfdn.Interpreter
{
    public class FunctionValue
    {
        private List<AstNode> body;
        private List<AstNode> Body
        {
            get { return body; }
        }

        private Environment creationEnvironment;
        public Environment CreationEnvironment
        {
            get { return creationEnvironment; }
        }

        private List<string> argumentNames;
        public List<string> ArgumentNames
        {
            get { return argumentNames; }
        }

        public FunctionValue(
            List<AstNode> body, 
            Environment creationEnvironment, 
            List<string> argumentNames)
        {
            this.body = body;
            this.creationEnvironment = creationEnvironment;
            this.argumentNames = argumentNames;
        }

        public object Call(List<object> argumentValues)
        {
            Environment env = this.creationEnvironment.Extend();
            foreach (Pair<string, object> binding 
                in Utils.Utils.Zip(this.argumentNames, argumentValues))
            {
                env.AssignLocal(binding.First, binding.Second);
            }
            Pair<bool, object> result = Interpreter.InterpretStatementList(this.body, env);
            return result.Second;
        }
    }
}
