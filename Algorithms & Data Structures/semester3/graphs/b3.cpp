#include <bits/stdc++.h>

using namespace std;

long long T = 0;
vector<vector<long long>> g;
vector<bool> u, r;
vector<long long> tin, up;

void link(long long q, long long w) {
    g[q].push_back(w);
    g[w].push_back(q);
}

void dfs(long long v, long long p) {
    u[v] = true;
    tin[v] = T;
    up[v] = T;
    T += 1;
    long long c = 0;

    for (long long to: g[v]) {
        if (to == p) {
            continue;
        }

        if (u[to]) {
            up[v] = min(up[v], tin[to]);
        } else {
            dfs(to, v);

            c += 1;
            up[v] = min(up[v], up[to]);

            if (up[to] >= tin[v] && p != -1) {
                r[v] = true;
            }
        }
    }
    if (p == -1 && c > 1) {
        r[v] = true;
    }
}

int main() {
    long long n, m, v;
    cin >> n >> m, v = n;
    g.resize(n + m), u.resize(n + m), tin.resize(n + m), up.resize(n + m, 1e9), r.resize(n + m);
    for (long long i = 0, q, w, e; i < m; ++i, v++) {
        cin >> q >> w >> e;

        link(q - 1, v);
        link(w - 1, v);
        link(e - 1, v);
    }

    dfs(0, -1);

    vector<long long> a;
    for (long long i = n; i < n + m; ++i) {
        if (r[i]) {
            a.push_back(i - n);
        }
    }

    cout << (long long) a.size() << '\n';
    for (auto el: a) {
        cout << el + 1 << ' ';
    }
}