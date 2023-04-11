#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
#include <map>

using namespace std;

vector<pair<int, int>> conflictListPositive;
vector<pair<int, int>> conflictListNegative;

vector<int>conflictListPositivePos;
vector<int>conflictListNegativePos;

void changeOrderP(int v, int rep){
    //v-1 = clau
    bool clauFounded = false;
    bool smallFounded = false;
    
    int clau;
    int small;
    for (int i = 1; i < conflictListPositivePos.size() and (not smallFounded or not clauFounded); i++){
        if(conflictListPositive[conflictListPositivePos[i]].second <= rep and not smallFounded){
            smallFounded = true;
            small = i;
        }
        if(conflictListPositive[conflictListPositivePos[i]].first == v and not clauFounded){
            clauFounded = true;
            clau = i;
        }
    }
    if(smallFounded and clauFounded) {
        while(small != clau){
            swap(conflictListPositivePos[clau], conflictListPositivePos[clau-1]);
            clau--;
        }
        
    }
}

void changeOrderN(){
    
}


int main(){
    int numVars;
    cin >> numVars;
    conflictListNegative.resize(numVars);
    conflictListPositive.resize(numVars);
    
    conflictListNegativePos.resize(numVars+1);
    conflictListPositivePos.resize(numVars+1);

    for (uint i = 0; i < numVars; i++) {
        
        conflictListPositive[i].first = i+1;
        conflictListPositive[i].second = 0;
        conflictListNegative[i].first = i+1;
        conflictListNegative[i].second = 0;
        
        conflictListNegativePos[i] = i-1;
        conflictListPositivePos[i] = i-1;
    }
    conflictListNegativePos[numVars] = numVars-1;
    conflictListPositivePos[numVars] = numVars-1;
    
    for (uint i = 0; i < numVars; i++) {

        cout << conflictListPositive[i].first << " " << conflictListPositive[i].second << " " << conflictListNegative[i].first << " " << conflictListNegative[i].second<< endl;
        cout << conflictListNegativePos[i] << " " << conflictListPositivePos[i] << endl;
    }
    cout << conflictListNegativePos[numVars] << " " << conflictListPositivePos[numVars] << endl;
    
    conflictListPositive[2].second++;
    changeOrderP(3, conflictListPositive[2].second);
    
    
    conflictListPositive[1].second++;
    conflictListPositive[1].second++;
    conflictListPositive[1].second++;
    changeOrderP(2, conflictListPositive[1].second);
    
    
    conflictListPositive[3].second++;
    conflictListPositive[3].second++;
    conflictListPositive[3].second++;
    conflictListPositive[3].second++;
    conflictListPositive[3].second++;
    changeOrderP(4, conflictListPositive[3].second);
    
    cout << endl << endl << endl << endl;
    
    for (uint i = 0; i < numVars; i++) {

        cout << conflictListPositive[i].first << " " << conflictListPositive[i].second << " " << conflictListNegative[i].first << " " << conflictListNegative[i].second<< endl;
    }
    
    for (int i = 0; i < numVars+1; i++){
        cout << conflictListNegativePos[i] << endl;
    }
    
    for (int i = 0; i < numVars+1; i++){
        cout << conflictListPositivePos[i] << endl;
    }
            
    
    
    
    
    
}
