#include <bits/stdc++.h>

using namespace std;

int main() {
    int n;
    cin >> n;
    vector<set<int>> matrix(n + 1);
    set<int> leafs;  //logarithmic complexity

    int u, v;
    for (int i = 0; i < n - 1; i++) {
        cin >> u >> v;
        matrix[u].insert(v);
        matrix[v].insert(u);
    }

    for (int i = 0; i < n; i++) {
        if (matrix[i].size() == 1) {
            leafs.insert(i);
        }
    }

    multiset<int> code;

    for (int idx = 0; idx < n - 2; idx++) {
        int min_leaf = *leafs.begin();

        int parent = *matrix[].begin();

        matrix[parent].erase(*leafs.begin());
        matrix[*leafs.begin()].erase(parent);

        code.insert(parent);

        if (matrix[parent].size() == 1) {
            leafs.insert(parent);
        }

        leafs.erase(*leafs.begin());
    }

    for (auto i: code) {
        cout << i << " ";
    }
}