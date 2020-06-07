#ifndef MATRIX_GAME_H
#define MATRIX_GAME_H

#include <vector>

// returns true if the first player wins the game starting with this claim matrix
bool firstPlayerWins(const std::vector<std::vector<bool>>& claim_matrix);

#endif // MATRIX_GAME_H
