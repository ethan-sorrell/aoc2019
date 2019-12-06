#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;


int main()
{
    ifstream fs("input.txt");
    vector<string> record;
    string token;
    // Read file
    while (getline(fs, token, ',')) {
        record.push_back(token);
    }
    record.back().pop_back();

    // "Restore program"
    record[1] = "12";
    record[2] = "2";

    // Execute program
    int pc = 0;
    while (record[pc] != "99") {
        if (record[pc] == "1") {
            int pos0 = stoi(record[pc+1]);
            int pos1 = stoi(record[pc+2]);
            int pos3 = stoi(record[pc+3]);
            record[pos3] = to_string(stoi(record[pos0]) + stoi(record[pos1]));
            pc += 4;
        } else if (record[pc] == "2") {
            int pos0 = stoi(record[pc+1]);
            int pos1 = stoi(record[pc+2]);
            int pos3 = stoi(record[pc+3]);
            record[pos3] = to_string(stoi(record[pos0]) * stoi(record[pos1]));
            pc += 4;
        } else {
            cout << "Error: incorrect opcode [" << record[pc] << "]" << endl;
            return 1;
        }
    }

    cout << record[0] << endl;
    return 0;
}
