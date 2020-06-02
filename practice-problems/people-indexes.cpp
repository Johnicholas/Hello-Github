#include <algorithm>
#include <string>
#include <vector>
#include "people-indexes.h"

using namespace std;

class Problem {
private:
  vector<vector<string>> favorites;
public:
  // add a new person, with an initially empty set of favorite companies
  // mutator
  void addPerson() {
    favorites.emplace_back();
  }

  // add a new favorite company, to the most recently-added person
  // mutator
  void addCompany(string company) {
    favorites.back().push_back(company);
  }

  // returns true if favorites[i] is a subset of favorites[j]
  bool isSubsetOf(int i, int j) {
    return all_of(favorites[i].begin(), favorites[i].end(), [&](const string& companyName1) {
	return any_of(favorites[j].begin(), favorites[j].end(), [&](const string& companyName2) {
	    return companyName1 == companyName2;
	  });
      });
  }
};



vector<int> Solution::peopleIndexes(vector<vector<string>>& favorites) {
  Problem p;
  for (const auto& person : favorites) {
    p.addPerson();
    for (const auto& company : person) {
      p.addCompany(company);
    }
  }
      
  vector<int> a;
  for (int i = 0; i < favorites.size(); i++) {
    bool found = false;
    for (int j = 0; j < favorites.size() && !found; j++) {
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

