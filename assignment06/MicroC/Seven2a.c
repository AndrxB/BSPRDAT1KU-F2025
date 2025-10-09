/* Write a micro-C program containing a function void arrsum(int n,
int arr[], int *sump) that computes and returns the sum of the first
n elements of the given array arr. The result must be returned through the
sump pointer. The program’s main function must create an array holding the
four numbers 7, 13, 9, 8, call function arrsum on that array, and print the
result using micro-C’s non-standard print statement.
Remember that MicroC is very limited compared to actual C: You cannot

use initializers in variable declarations like “int i=0;” but must use a dec-
laration followed by a statement, as in “int i; i=0;” instead; there is no

for-loop (unless you implement one, see Exercise 7.3); and so on.
Also remember to initialize all variables and array elements; this doesn’t
happen automatically in micro-C or C. */


void main(int m) {
    int i; 
    
    int arr[4];
    arr[0] = 7;
    arr[1] = 13;
    arr[2] = 9;
    arr[3] = 8;
    
    int *sum;
    *sum = 0;
    arrsum(4, arr, sum);
    print *sum;
    
}

void arrsum(int n,int arr[], int *sump){
    
    int count;
    count = 0;
    while (count < n) {
        *sump = *sump + arr[count];
        count = count + 1;
    }
    
}
