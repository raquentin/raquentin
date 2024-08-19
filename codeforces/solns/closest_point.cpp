#include <iostream>
#include <vector>

using namespace std;

int main() {
    int t;
    cin >> t;

    while (t--) {
        int n;
        cin >> n;

        vector<int> v;
        int temp;
        for (int i = 0; i < n; i++) {
            cin >> temp;
            v.push_back(temp);
        }

        if (n == 2 && v[1] - v[0] > 1) {
            cout << "YES\n";
        } else {
            cout << "NO\n";
        }
    }

    return 0;
}
