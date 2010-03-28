using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections;

namespace LowLevelApocryphal
{
    public static class ListExtensions
    {
        // This algorithm is just a first approximation. Should implement a decent
        // shuffling from Knuth.
        public static void Shuffle(this ArrayList toShuffle)
        {
            Random r = new Random();
            for (int i = 0; i < toShuffle.Count - 1; i++)
            {
                int indexToExchange = r.Next(i + 1, toShuffle.Count);
                object aux = toShuffle[i];
                toShuffle[i] = toShuffle[indexToExchange];
                toShuffle[indexToExchange] = aux;
            }
        }
    }
}
