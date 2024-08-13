#include <iostream>
#include <vector>
using namespace std;

string solve(vector<int> &a, vector<int> &b) {
  int aleft, bleft = 0;
  int aright, bright = a.size() - 1;
  // alice prefs a num if it's

  // game loop
  while (aleft != aright) {
    // alice's turn
    // strategy: force bob to choose a num that I haven't
    // I want to choose a num if the num inside of it is not on bob's ends
    // otherwise, idgaf
    // when i choose, a[choice] *= -1
    if (a[aleft])

    // bob's turn
    if (a[b[bleft]] < 0) {
      bleft += 1;
    } else if (a[b[bright]] < 0) {
      bright += 1;
    } else {
      // bob loses if he's forced to choose a num that alice hasn't
      return "Alice";
    }
  }

  if (a[aleft] == b[bleft]) {
    return "Bob";
  } else {
    return "Alice";
  }
}

int main() {
  int t;
  cin >> t;

  while (t--) {
    int n;
    cin >> n;

    vector<int> a(n), b(n);

    for (int i = 0; i < n; i++) {
      cin >> a[i];
    }

    for (int i = 0; i < n; i++) {
      cin >> b[i];
    }

    cout << solve(a, b) << '\n';
  }

  return 0;
}
