using System;
using System.Collections.Generic;
using System.Text;

namespace Sicfdn.Interpreter
{
    public class InterpreterException : Exception
    {
        public InterpreterException(string message)
            : base(message)
        {
        }
    }
}
