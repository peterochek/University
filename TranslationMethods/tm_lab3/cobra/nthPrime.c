#include <stdio.h>

double mod(double a, double q) {
    double x;
    x = a;
    double zero;
    zero = 0;
    while (((x > zero) || (x == zero))) {
        x = x - q;
    }
    return x + q;
}

double isPrime(double n) {
    double zero;
    zero = 0;
    double one;
    one = 1;
    double two;
    two = 2;
    double three;
    three = 3;
    if ((n <= one)) {
        return 0;
    }
    if ((n <= three)) {
        return 1;
    }
    double modN2;
    modN2 = mod(n, two);
    double modN3;
    modN3 = mod(n, three);
    if (((modN2 == zero) || (modN3 == zero))) {
        return 0;
    }
    double i;
    i = 5;
    double iSquared;
    iSquared = i * i;
    while ((iSquared <= n)) {
        double iPlus2;
        iPlus2 = i + 2;
        double modNi;
        modNi = mod(n, i);
        double modNiPlus2;
        modNiPlus2 = mod(n, iPlus2);
        if (((modNi == zero) || (modNiPlus2 == zero))) {
            return 0;
        }
        i = i + 6;
        iSquared = i * i;
    }
    return 1;
}

double findNthPrime(double n) {
    double one;
    one = 1;
    double count;
    count = 0;
    double candidate;
    candidate = 2;
    while ((count < n)) {
        double isPrimeCandidate;
        isPrimeCandidate = isPrime(candidate);
        if ((isPrimeCandidate == one)) {
            count = count + 1;
        }
        if ((count < n)) {
            candidate = candidate + 1;
        }
    }
    return candidate;
}


int main() {
    double n;
    scanf("%lf", &n);
    double nthPrime;
    nthPrime = findNthPrime(n);
    printf("%lf\n", nthPrime);
    return 0;
}

