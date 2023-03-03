#include <bits/stdc++.h>

using namespace std;

int main() {
    string s;
    cin >> s;

    s.push_back(0);
    int n = (int) s.size(),
            cnt = 0,
            cls = 0;
    vector<int> c(n), p(n);

    map<int, vector<int> > t;
    for (int i = 0; i < n; i++) {
        t[s[i]].push_back(i);
    }

    for (auto &x: t) {
        for (int u: x.second) {
            c[u] = cls, p[cnt++] = u;
        }
        cls += 1;
    }

    for (int l = 1; cls < n; l++) {
        vector<vector<int> > a(cls);
        vector<int> _c(n);
        int d = (1 << l) / 2;
        int _cls = cnt = 0;

        for (int i = 0; i < n; i++) {
            int k = (p[i] - d + n) % n;
            a[c[k]].push_back(k);
        }

        for (int i = 0; i < cls; i++) {
            for (size_t j = 0; j < a[i].size(); j++) {
                if (j == 0 || c[(a[i][j] + d) % n] != c[(a[i][j - 1] + d) % n]) {
                    _cls += 1;
                }
                _c[a[i][j]] = _cls - 1;
                p[cnt++] = a[i][j];
            }
        }

        c = _c;
        cls = _cls;
    }

    for (int i = 1; i < n; i++) {
        cout << p[i] + 1 << " ";
    }

    int current_lcp = 0;
    vector<int> lcp(n);
    for (int i = 0; i < n; i++) {
        if (c[i] == n - 1) {
            continue;
        }
        int nxt = p[c[i] + 1];
        while (max(i, nxt) + current_lcp < n && s[i + current_lcp] == s[nxt + current_lcp]) {
            current_lcp += 1;
        }
        lcp[c[i]] = current_lcp;
        current_lcp = max(0, current_lcp - 1);
    }

    cout << endl;

    for (int i = 1; i < n - 1; i++) {
        cout << lcp[i] << " ";
    }
}