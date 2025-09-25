using System;
using System.Linq;


static T[] Merge<T>(T[] xs, T[] ys) where T : IComparable<T>
{
    T[] arr = new T[xs.Length + ys.Length];
    // Variables for tracking indexes 
    int x = 0, y = 0, i = 0;
    while (y < ys.Length && x < xs.Length)
        // Since both arrays extends IComparable, 
        // we use the .CompareTo method rather than equality.
        arr[i++] = xs[x].CompareTo(ys[y]) > 0 ? xs[x++] : ys[y++];
    // Append the rest of the non-empty array into the final arr
    while (x < xs.Length) arr[i++] = xs[x++];
    while (y < ys.Length) arr[i++] = ys[y++];
    return arr;
}

static void Test<T>(T[] xs, T[] ys) where T : IComparable<T> =>
    Merge(xs, ys).ToList().ForEach(k => Console.Write(k)); Console.WriteLine();

// Tests
int[] i1 = [3, 4, 12];
int[] i2 = [2, 3, 5, 7];
bool[] b1 = [false, false, true];
bool[] b2 = [false, true, true];
char[] c1 = ['a', 'b', 'c', 'q', 'z'];

Test(i1, i2);
Test(i2, i1);
Test(b1, b2);
Test(b2, b1);
Test(c1, c1);
