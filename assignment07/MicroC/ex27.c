void main() {
    int month;
    int days;
    month = 2;
    switch (month) { 
        case 1: { days = 31; } 
        case 2: { days = 28; } 
        case 3: { days = 31; }
    }
    print days;
    println;
}
