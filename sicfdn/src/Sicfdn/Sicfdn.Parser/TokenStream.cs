using System;
using System.Collections.Generic;
using System.Text;

namespace Sicfdn.Parser
{
    internal class TokenStream
    {
        private IEnumerator<Token> enumerator;
        private bool hasMoreTokens = true;
        private Token current = null;

        public bool HasMoreTokens
        {
            get
            {
                return current != null;
            }
        }

        public TokenStream(IEnumerable<Token> enumerable)
        {
            this.enumerator = enumerable.GetEnumerator();
            Next();
            if (hasMoreTokens)
            {
                current = this.enumerator.Current;
            }
        }

        public Token Current
        {
            get
            {
                return current;
            }
        }

        public void Next()
        {
            if (hasMoreTokens)
            {
                hasMoreTokens = this.enumerator.MoveNext();
                if (hasMoreTokens)
                {
                    current = this.enumerator.Current;
                }
                else
                {
                    current = null;
                }
            }
            else
            {
                throw new InvalidOperationException("Read past end of stream.");
            }
        }


    }
}
