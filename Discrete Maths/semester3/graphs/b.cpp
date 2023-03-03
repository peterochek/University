#include <bits/stdc++.h>

using namespace std;

int main() {
    int n;
    string buffer;
    cin >> n;

    auto matrix = vector<vector<bool>>(n, vector<bool>(n, false));
    for (size_t i = 1; i < n; ++i) {
        cin >> buffer;
        for (size_t j = 0; j < buffer.size(); ++j) {
            if (buffer[j] == '1') {
                matrix[i][j] = true;
                matrix[j][i] = true;
            }
        }
    }

    deque<int> q(n);
    for (int i = 0; i < n; ++i) {
        q[i] = i;
    }

    for (int k = 0; k < n * (n - 1); ++k) {
        if (!matrix[q[0]][q[1]]) {
            int i = 2;
            while (true) {
                int inner = -1;

                if (matrix[q[0]][q[i]] && matrix[q[1]][q[i + 1]]) {
                    inner = i;
                } else if (i == n - 1) {
                    inner = 2;
                    while (true) {
                        if (matrix[q[0]][q[inner]]) {
                            break;
                        }

                        inner++;
                    }
                }

                if (inner != -1) {
                    reverse(q.begin() + 1, q.begin() + inner + 1);
                    break;
                }

                i++;
            }
        }

        q.push_back(q.front());
        q.pop_front();
    }

    for (int v: q) {
        cout << v + 1 << ' ';
    }
}