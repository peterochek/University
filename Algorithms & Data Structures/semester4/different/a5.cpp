#include <iostream>
#include <vector>
#include <complex>

const int DIGITS_LIMIT = 2'000'000;
const long double PI = acos(-1.0);

using namespace std;
using cd = complex<long double>;

void fft(vector<cd> &vec, int n, bool invert = false) {
    if (n == 1) return;

    vector<cd> first(n / 2), second(n / 2);
    long double alpha = 2 * PI / n * (invert ? -1 : 1);
    cd root(1, 0), shift(cos(alpha), sin(alpha));

    for (int i = 0; i < n / 2; i++) {
        first[i] = vec[2 * i];
        second[i] = vec[2 * i + 1];
    }

    fft(first, n >> 1, invert);
    fft(second, n >> 1, invert);

    for (int i = 0; i < n / 2; i++) {
        vec[i] = first[i] + root * second[i];
        vec[i + n / 2] = first[i] - root * second[i];
        if (invert) {
            vec[i] /= 2;
            vec[i + n / 2] /= 2;
        }
        root *= shift;
    }
}

int main() {
    string first, second;
    cin >> first >> second;
    if (first == "0" || second == "0") {
        cout << "0";
        return 0;
    }

    if ((first.at(0) == '-' || second.at(0) == '-') && (first.at(0) != '-' || second.at(0) != '-')) {
        cout << "-";
    }
    if (first.at(0) == '-') {
        first = first.substr(1);
    }
    if (second.at(0) == '-') {
        second = second.substr(1);
    }

    size_t digits_first = first.size(), digits_second = second.size();

    vector<cd> s1_repr(DIGITS_LIMIT), s2_repr(DIGITS_LIMIT), output(DIGITS_LIMIT);
    vector<int> result(DIGITS_LIMIT);

    for (size_t i = 0; i < digits_first; i++)
        s1_repr[digits_first - 1 - i] = first[i] - '0';

    for (size_t i = 0; i < digits_second; i++)
        s2_repr[digits_second - 1 - i] = second[i] - '0';

    int n = 1;
    while (n < digits_first + digits_second) n <<= 1;

    fft(s1_repr, n);
    fft(s2_repr, n);

    for (int i = 0; i < n; i++)
        output[i] = s1_repr[i] * s2_repr[i];

    fft(output, n, true);

    for (int i = 0; i < n; i++)
        result[i] = (int) (output[i].real() + 0.1);

    for (int i = 0; i < n; i++) {
        if (result[i] > 9) {
            result[i + 1] += result[i] / 10;
            result[i] %= 10;
        }
    }

    int i = n - 1;
    while (i >= 0 && result[i] == 0) i--;

    for (; i >= 0; i--)
        cout << result[i];
}
