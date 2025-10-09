/*
Write a micro-C program containing a function void histogram(int
n, int ns[], int max, int freq[]) which fills array freq the

frequencies of the numbers in array ns. More precisely, when the function re-
turns, element freq[c] must equal the number of times that value c appears

among the first n elements of arr, for 0<=c<=max. You can assume that all
numbers in ns are between 0 and max, inclusive.
For example, if your main function creates an array arr holding the seven
numbers 1 2 1 1 1 2 0 and calls histogram(7, arr, 3, freq), then
afterwards freq[0] is 1, freq[1] is 4, freq[2] is 2, and freq[3] is 0.
Of course, freq must be an array with at least four elements. What happens if
it is not? The array freq should be declared and allocated in the main func-
tion, and passed to histogram function. It does not work correctly (in micro-C
or C) to stack-allocate the array in histogram and somehow return it to the
main function. Your main function should print the contents of array freq
after the call.
*/


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

