#include <iostream>
#include <vector>

using namespace std;

int main() {
    int t;
    cin >> t;

    while (t--) {
        int n;
        cin >> n;

        if (n > 2) {
            cout << "NO";
        } else {
            cout << "YES";
        }
    }

    return 0;
}
