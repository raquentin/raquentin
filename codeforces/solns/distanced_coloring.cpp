#include <iostream>
 
using namespace std;
 
int main() {
  ios::sync_with_stdio(false);
  cin.tie(0);
  cout.tie(0);

  int t;
  cin >> t;
 
  while (t--) {
    int n, m, k;
    cin >> n >> m >> k;
 
    if (m >= k && n >= k) {
      cout << k * k << '\n';
    } else if (m < k && n < k) {
      cout << m * n << '\n';
    } else if (m >= k && n < k) {
      cout << k * n << '\n';
    } else if (m < k && n >= k) {
      cout << k * m << '\n';
    }
  }
}
