
void main(int m) {
    
    int arr[7];
    

    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 1;
    arr[3] = 1;
    arr[4] = 1;
    arr[5] = 2;
    arr[6] = 0;

    int freq[4];

    freq[0] = 10;
    
    
    histogram(7, arr, 3, freq);


    print freq[0];
    print freq[1];
    print freq[2];
    print freq[3];
}

void histogram(int n,int ns[], int max, int freq[]){
    
    int count;
    int freqvalue;
    int numbertoFreq;
    numbertoFreq = 0;
    while(numbertoFreq <= max ){
    
        count = 0;
        freqvalue = 0;
        while (count < n) {
            if (numbertoFreq == ns[count]) {
                 freqvalue = freqvalue + 1;
          
            }
            freq[numbertoFreq] = freqvalue;
            count = count + 1;
        }
        numbertoFreq = numbertoFreq + 1;
    }
    
}

