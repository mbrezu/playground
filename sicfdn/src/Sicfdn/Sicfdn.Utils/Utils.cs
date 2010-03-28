using System;
using System.Collections.Generic;
using System.Text;

namespace Sicfdn.Utils
{
    public class Utils
    {
        public static IEnumerable<Pair<T, U>> Zip<T, U>(IEnumerable<T> coll1, IEnumerable<U> coll2)
        {
            IEnumerator<T> en1 = coll1.GetEnumerator();
            IEnumerator<U> en2 = coll2.GetEnumerator();
            while (en1.MoveNext() && en2.MoveNext())
            {
                yield return new Pair<T, U>(en1.Current, en2.Current);
            }
        }
    }
}
