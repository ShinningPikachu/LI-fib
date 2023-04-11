#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

vector<pair<int, int>> literalsList;

struct rule {
    bool operator() (const pair<int, int> a, const pair<int, int> b) {
        return (a.second > b.second);
    }
};

int main() {
    int num;
    cin >> num;
    literalsList.resize(num);
    
    for (int i = 0; i < 9; i++) {
        literalsList[i].first = i + 1;
        literalsList[i].second = 0;
    }

    for (int i = 0; i < 9; i++) {
        cout << literalsList[i].first << " " << literalsList[i].second << endl;
    }
    
    int k;
    for (int i = 0; i < 10; i++) {
        for (int j = 0; j < 3; j++) {
            cin >> k;
            literalsList[k-1].second++;
        }
    }

    for (int i = 0; i < 9; i++) {
        cout << literalsList[i].first << " " << literalsList[i].second << endl;
    }
    sort(literalsList.begin(), literalsList.end(), rule());
        for (int i = 0; i < 9; i++) {
        cout << literalsList[i].first << " " << literalsList[i].second << endl;
    }
}
