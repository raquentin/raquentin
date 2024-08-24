#include <iostream>
#include <vector>

using namespace std;

void solve() {
    int n, temp;
    cin >> n;

    vector<int> v(n + 1);
    for (int i = 0; i < n; i++) {
        cin >> temp;
        v[temp]++;
    }

    int m = 0;
    for (int i = 1; i <= n; i++) {
        m = max(v[i], m);
    }

    cout << n - m << '\n';
}

int main() {
  int t;
  cin >> t;

  while (t--) {
    solve();
  }

  return 0;
}
