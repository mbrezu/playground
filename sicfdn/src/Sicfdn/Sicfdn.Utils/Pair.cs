using System;
using System.Collections.Generic;
using System.Text;

namespace Sicfdn.Utils
{
    public class Pair<T, U>
    {
        private T first;
        public T First
        {
            get { return first; }
        }

        private U second;
        public U Second
        {
            get { return second; }
        }

        public Pair(T first, U second)
        {
            this.first = first;
            this.second = second;
        }
    }
}
