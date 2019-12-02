#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

int execProgram(vector<string> record, int noun, int verb)
{
    record[1] = to_string(noun);
    record[2] = to_string(verb);
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
            exit(1);
        }
    }
    return stoi(record[0]);
}

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

    for (int i = 0; i <= 99; ++i)
    {
        for (int j = 0; j <= 99; ++j)
        {
            if (execProgram(record, i, j) == 19690720)
            {
                cout << "noun: " << i << endl;
                cout << "verb: " << j << endl;
                cout << "ans: " << 100 * i + j << endl;
                return 0;
            }
        }
    }
    return 0;
}
