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
        int fuel_required = (mass / 3) - 2;

        while (fuel_required > 0) {
            acc += fuel_required;
            fuel_required = (fuel_required / 3) - 2;
        }
    }
    std::cout << "we require " << acc << " fuel" << std::endl;
    return 0;
}
