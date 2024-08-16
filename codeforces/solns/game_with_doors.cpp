#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

int main() {
    int t;
    cin >> t;

    while (t--) {
        int l, r, L, R;
        cin >> l >> r >> L >> R;

        if (l >= R || L >= r) {
            cout << 1;
            break;
        }

        cout << min(r, R) - max(l, L) + (r != R) + (l != L);
    }

    return 0;
}
