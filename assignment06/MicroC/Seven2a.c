
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
