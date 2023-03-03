#include <bits/stdc++.h>

using namespace std;

const long long maxn = 2e5;
vector<pair<long long, long long>> g[maxn];
long long d[maxn];
bool u[maxn];
vector<long long> top;

void dfs(long long v) {
    u[v] = 1;
    for (auto [to, _]: g[v]) {
        if (!u[to]) {
            dfs(to);
        }
    }
    top.push_back(v);
}

signed main() {
    long long n, m;
    long long start, t;
    cin >> n >> m >> start >> t;
    for (long long i = 0, q, w, e; i < m; ++i) {
        cin >> q >> w >> e;
        g[q - 1].emplace_back(w - 1, e);
    }

    for (long long i = 0; i < n; ++i) {
        if (!u[i]) {
            dfs(i);
        }
    }

    reverse(top.begin(),top.end());

    fill(d, d + n, 1e18);
    d[start - 1] = 0;

    long long ind = find(top.begin(),top.end(), start - 1) - top.begin();

    for (long long i = ind; i < n; ++i) {
        long long v = top[i];
        for (auto [to, dist]: g[v]) {
            if (d[to] > d[v] + dist) {
                d[to] = d[v] + dist;
            }
        }
    }
    if (d[t - 1] >= 1e9) {
        cout << "Unreachable";
    }
    else {
        cout << d[t - 1];
    }
}