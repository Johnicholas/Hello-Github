#include <algorithm>
#include <string>
#include <vector>
#include "people.h"

using namespace std;

class Problem {
private:
  vector<vector<string>>& f;
public:
  Problem(vector<vector<string>>& f) : f(f) {}

  // returns true if f[i] is a subset of f[j]
  bool isSubsetOf(int i, int j) {
    return all_of(f[i].begin(), f[i].end(), [&](const string& companyName1) {
	return any_of(f[j].begin(), f[j].end(), [&](const string& companyName2) {
	    return companyName1 == companyName2;
	  });
      });
  }
};



vector<int> Solution::peopleIndexes(vector<vector<string>>& f) {
  Problem p(f);
  
  vector<int> a;
  for (int i = 0; i < f.size(); i++) {
    // found j such that f[i] is a subset of f[j]
    bool found = false;
    for (int j = 0; j < f.size() && !found; j++) {
      if (i == j) {
	continue;
      }
      if (p.isSubsetOf(i, j)) {
	found = true;
      }
    }
    if (!found) {
      a.push_back(i);
    }
  }
  return a;
}

