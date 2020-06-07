#include <iostream>
#include <vector>

#include <algorithm>

bool firstPlayerWins(const std::vector<std::vector<bool>>& claim_matrix) {
  // TODO: what happens if the number of rows or cols is zero?
  // prohibited: 1 is the minimum
  
  std::vector<bool> rows_claimed(claim_matrix.size(), false);
  std::vector<bool> cols_claimed(claim_matrix[0].size(), false);
  int open_rows = claim_matrix.size();
  int open_cols = claim_matrix[0].size();
  
  for (int i = 0; i < claim_matrix.size(); i++) {
    for (int j = 0; j < claim_matrix[i].size(); j++) {
      if (claim_matrix[i][j] && !rows_claimed[i]) {
	rows_claimed[i] = true;
	open_rows--;
      }
      if (claim_matrix[i][j] && !cols_claimed[j]) {
	cols_claimed[j] = true;
	open_cols--;
      }
    }
  }
  return std::min(open_rows, open_cols) % 2 == 1;
}

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
