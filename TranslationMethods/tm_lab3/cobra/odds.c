#include <stdio.h>

double _cbr0(double x, double y) {
    return x + y;
}

double square(double a) {
    return a * a;
}

double cubesWhile(double a) {
    double odd;
    odd = 1;
    double sum;
    sum = 0;
    while ((odd < a)) {
        sum = sum + odd;
        odd = odd + 2;
    }
    return sum;
}


int main() {
    double odd[5] = { 1, 3, 5, 7, 9 };
    int _cbr1 = sizeof(odd) / sizeof(odd[0]);
    double sumOdd = 0;
    for (int _cbr2 = 0; _cbr2 < _cbr1; _cbr2++) {
        sumOdd = _cbr0(sumOdd, odd[_cbr2]);
    }
    double cnt;
    cnt = 10 / 2;
    double smartSumOdd;
    smartSumOdd = square(cnt);
    double last;
    last = cnt * 2;
    double whileSumOdd;
    whileSumOdd = cubesWhile(last);
    printf("%lf\n", sumOdd);
    printf("%lf\n", smartSumOdd);
    printf("%lf\n", whileSumOdd);
    int _cbr4 = sizeof(odd) / sizeof(odd[0]);
	for (int _cbr3 = 0; _cbr3 < _cbr4; _cbr3++) {
		printf("%lf\n", odd[_cbr3]);
	}

    return 0;
}

