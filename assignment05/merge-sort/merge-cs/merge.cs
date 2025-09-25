using System;
using System.Collections.Generic;
using System.Linq;

static class MergeSortFive1
{
    static int[] Merge(int[] xs, int[] ys)
    {
        int[] tmp = new int[xs.Length + ys.Length];
        int x = 0, y = 0, i = 0;
        while (y < ys.Length && x < xs.Length) tmp[i++] = xs[x] < ys[y] ? xs[x++] : ys[y++];
        while (x < xs.Length) tmp[i++] = xs[x++];
        while (y < ys.Length) tmp[i++] = ys[y++];
        return tmp;
    }
    static void Main(string[] _)
    {
        static void Test(int[] xs, int[] ys)
        {
            foreach (var i in Merge(xs, ys))
            {
                Console.Write(i + " ");
            }
            Console.WriteLine();
        }
        int[] i1 = [3, 4, 12];
        int[] i2 = [2, 3, 5, 7];
        int[] i3 = [0, 1, 8, 9];
        int[] i4 = [-1, -8, 0, 69];

        Test(i1, i2);
        Test(i2, i3);
        Test(i3, i4);
    }
}