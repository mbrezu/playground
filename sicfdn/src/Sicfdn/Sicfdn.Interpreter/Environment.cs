using System;
using System.Collections.Generic;
using System.Text;
using Sicfdn.Utils;

namespace Sicfdn.Interpreter
{
    public class Environment
    {
        private Dictionary<string, object> frame;
        private Environment parent;

        public Environment(Environment parent)
        {
            this.parent = parent;
            this.frame = new Dictionary<string, object>();
        }

        public Pair<bool, object> LookUp(string name)
        {
            Environment fr = GetContainingFrame(name);
            if (fr != null)
            {
                return new Pair<bool,object>(true, fr.frame[name]);
            }
            else
            {
                return new Pair<bool, object>(false, null);
            }
        }

        private Environment GetContainingFrame(string name)
        {
            if (this.frame.ContainsKey(name))
            {
                return this;
            }
            else if (this.parent != null)
            {
                return this.parent.GetContainingFrame(name);
            }
            else
            {
                return null;
            }
        }

        public void Assign(string name, object value)
        {
            Environment fr = GetContainingFrame(name);
            if (fr == null)
            {
                fr = this;
            }
            fr.frame[name] = value;
        }

        public void AssignLocal(string name, object value)
        {
            this.frame[name] = value;
        }

        public Environment Extend()
        {
            return new Environment(this);
        }
    }
}
