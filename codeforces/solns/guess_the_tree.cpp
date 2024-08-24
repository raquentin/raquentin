#include <iostream>
#include <vector>

using namespace std;

typedef struct {
  int a;
  int b;
} edge;

void find(int, int, vector<edge> &, vector<int> &);

void solve() {
  int n;
  cin >> n;

  vector<int> vis(n + 1);
  vis[1] = 1;
  vector<edge> edges;

  for (int i = 2; i <= n; i++) {
    find(1, i, edges, vis);
  }

  cout << "! ";
  for (int i = 0; i < edges.size(); i++) {
    cout << edges[i].a << " " << edges[i].b;
    if (i != edges.size() - 1) {
      cout << " ";
    } else {
      cout << endl;
    }
  }
}

void find(int a, int b, vector<edge> &edges, vector<int> &vis) {
  if (a == b) {
    vis[a] = 1;
    return;
  }

  if (vis[a] && vis[b]) {
    return;
  }

  cout << "? " << a << " " << b << endl;
  int mid;
  cin >> mid;

  if (mid == a) {
    edges.push_back(edge{a, b});
    vis[b] = 1;
    return;
  }

  if (!vis[mid])
    find(a, mid, edges, vis);
  find(mid, b, edges, vis);

  vis[b] = 1;
}

int main() {
  int t;
  cin >> t;

  while (t--) {
    solve();
  }

  return 0;
}
