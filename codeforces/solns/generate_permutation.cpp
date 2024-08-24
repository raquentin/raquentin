#include <iostream>
#include <vector>

using namespace std;

void solve() {
  int n;
  cin >> n;

  if (n % 2 == 0) {
    cout << -1 << '\n';
    return;
  }

  for (int i = 1; i <= n; i++) {
    if (i <= n / 2) {
      cout << i;
    } else {
      cout << n - (i - n / 2 - 1);
    }

    if (i == n) {
      cout << '\n';
    } else {
      cout << ' ';
    }
  }
}

int main() {
  int t;
  cin >> t;

  while (t--) {
    solve();
  }

  return 0;
}
