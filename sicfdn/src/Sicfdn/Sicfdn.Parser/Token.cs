using System;
using System.Collections.Generic;
using System.Text;

namespace Sicfdn.Parser
{
    public class Token
    {
        private TokenType type;
        public TokenType Type
        {
            get { return type; }
        }

        private int start;
        public int Start
        {
            get { return start; }
        }

        private int stop;
        public int Stop
        {
            get { return stop; }
        }

        private string content;
        public string Content
        {
            get { return content; }
        }

        public Token(TokenType type, int start, int stop, string content)
        {
            this.type = type;
            this.start = start;
            this.stop = stop;
            this.content = content;
        }

        public override string ToString()
        {
            return String.Format(
                "{0} ('{1}', between {2} and {3})",
                this.type,
                this.content,
                this.start,
                this.stop);
        }

        public override bool Equals(object obj)
        {
            return this.ToString() == obj.ToString();
        }

        public override int GetHashCode()
        {
            return this.ToString().GetHashCode();
        }
    }
}
