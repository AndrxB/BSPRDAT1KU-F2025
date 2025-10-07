
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
