#include <iostream>
#include <vector>

#include "matrix-game.h"

int main(int argc, char* argv[]) {
  int test_cases;
  std::cin >> test_cases;
  for (int i = 0; i < test_cases; i++) {
    int rows, cols;
    std::cin >> rows >> cols;
    std::vector<std::vector<bool>> matrix;
    for (int j = 0; j < rows; j++) {
      matrix.emplace_back();
      for (int k = 0; k < cols; k++) {
	int cell;
	std::cin >> cell;
	// we are leaning on implicit conversion here: 0->false, 1->true
	matrix[j].push_back(cell);
      }
    }
    std::cout << (firstPlayerWins(matrix)?"Ashish":"Vivek") << "\n";
  }
  return 0;
}
