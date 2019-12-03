#include<vector>
#include<fstream>
#include<iostream>
#include<string>
#include<unordered_map>

using namespace std;

struct LineSegment
{
    vector<int> from;
    vector<int> to;
};

int main()
{
    unordered_map<char, vector<int>> direction;
    direction['U'] = {0, 1};
    direction['D'] = {0, -1};
    direction['R'] = {1, 0};
    direction['L'] = {-1, 0};

    vector<LineSegment> segments;
    vector<int> startP = {0, 0};

    // Read and parse line segments for first wire
    string token;
    fstream fs("input.txt");
    bool done = false;
    while (!done && getline(fs, token, ',')) {
        if (token.back() == '\n')
        {
            done = true;
            token.pop_back();
        }
        int magnitude = stoi(token.substr(1));
        LineSegment l;

        l.from = startP;
        startP[0] += direction[token[0]][0] * magnitude;
        startP[1] += direction[token[0]][1] * magnitude;
        l.to = startP;
        segments.push_back(l);
    }
    // Read and parse line segments for second wire
    // Checking for intersection

    return 0;
}
