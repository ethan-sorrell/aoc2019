#include <iostream>
#include <fstream>
#include <string>


int main()
{
    std::string line;
    std::ifstream fs("input.txt");
    int acc = 0;
    while (std::getline(fs, line)) {
        int mass = std::stoi(line);
        // fuel_required = floor(mass / 3) - 2
        int fuel_required = (mass / 3) - 2;
        acc += fuel_required;
    }
    std::cout << "we require " << acc << " fuel" << std::endl;
    return 0;
}
