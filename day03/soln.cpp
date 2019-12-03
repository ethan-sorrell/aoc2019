#include<vector>
#include<fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <sstream>
#include <set>

using namespace std;

int main()
{
    unordered_map<char, vector<int>> direction;
    direction['U'] = {0, 1};
    direction['D'] = {0, -1};
    direction['R'] = {1, 0};
    direction['L'] = {-1, 0};

    vector<int> lastP = {0, 0};
    set<vector<int>> points;

    string token;
    fstream fs("input.txt");
    string wire1, wire2;
    getline(fs, wire1);
    getline(fs, wire2);
    // Read and parse points for first wire
    istringstream iss (wire1);
    while (getline(iss, token, ',')) {
        int magnitude = stoi(token.substr(1));
        vector<int> dir = direction[token[0]];
        for (int i = 0; i < magnitude; i++) {
            lastP = {lastP[0] + dir[0],
                     lastP[1] + dir[1]};
            points.insert(lastP);
        }
    }

    // Read and parse points for second wire
    // Checking for intersections
    lastP = {0, 0};
    int min_distance = -1;
    istringstream iss2 (wire2);
    while (getline(iss2, token, ',')) {
        int magnitude = stoi(token.substr(1));
        vector<int> dir = direction[token[0]];
        for (int i = 0; i < magnitude; i++) {
            lastP = {lastP[0] + dir[0],
                     lastP[1] + dir[1]};
            if (points.count(lastP)) {
                if (min_distance == -1) {
                    min_distance = abs(lastP[0]) + abs(lastP[1]);
                } else {
                    min_distance = (min_distance > (abs(lastP[0]) + abs(lastP[1]))) ?
                        (abs(lastP[0]) + abs(lastP[1])) : min_distance;
                }
            }
        }
    }
    cout << min_distance << endl;
    return 0;
}
