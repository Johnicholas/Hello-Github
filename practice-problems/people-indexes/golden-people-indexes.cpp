#include "golden-people-indexes.h"

#include <algorithm>

using namespace std;

void GoldenProblem::addPerson() {
  favorites.emplace_back();
}

void GoldenProblem::addCompany(string company) {
  favorites.back().push_back(company);
}

bool GoldenProblem::isSubsetOf(int i, int j) const {
  return all_of(favorites[i].begin(), favorites[i].end(), [&](const string& companyName1) {
      return any_of(favorites[j].begin(), favorites[j].end(), [&](const string& companyName2) {
	  return companyName1 == companyName2;
	});
    });
}

bool GoldenProblem::aSubsetOfAnyOther(int i) const {
  bool found = false;
  for (int j = 0; j < favorites.size(); j++) {
    if (i == j) {
      continue;
    }
    if (isSubsetOf(i, j)) {
      found = true;
    }
  }
  return found;
}


vector<int> GoldenPeopleIndexes(vector<vector<string>> favorites) {
  GoldenProblem p;
  for (const auto& person : favorites) {
    p.addPerson();
    for (const auto& company : person) {
      p.addCompany(company);
    }
  }
      
  vector<int> a;
  for (int i = 0; i < favorites.size(); i++) {
    if (!p.aSubsetOfAnyOther(i)) {
      a.push_back(i);
    }
  }
  return a;
}


