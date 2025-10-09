/* 
Write a micro-C program containing a function void squares(int n,
int arr[]) that, given n and an array arr of length n or more fills
arr[i] with i*i for i = 0,...,n − 1.
Your main function should allocate an array holding up to 20 integers, call
function squares to fill the array with n square numbers (where n ≤ 20 is
given as a parameter to the main function), then call function arrsum above
to compute the sum of the n squares, and print the sum.
*/


void main(int m) {
    int i; 
    
    int arr[20];

    int *sum;
    *sum = 0;

    
    squares(20, arr);
    
    arrsum(20, arr, sum);
    print *sum;
    
}

void squares(int n,int arr[]){

    int count;
    count = 0;
    while (count < n) {
        *(arr + count) = count * count;
        count = count + 1;
    }
    
}

void arrsum(int n,int arr[], int *sump){

    int count;
    count = 0;
    while (count < n) {
        *sump = *sump + arr[count];
        count = count + 1;
    }
    
}
