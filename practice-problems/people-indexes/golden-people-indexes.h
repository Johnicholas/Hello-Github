#ifndef GOLDEN_PEOPLE_INDEXES_H
#define GOLDEN_PEOPLE_INDEXES_H

#include <string>
#include <vector>

// A version of the problem which is simple and direct, but possibly inefficient.
class GoldenProblem {
 public:
  // add a new person, with an initially empty set of favorite companies
  void addPerson();

  // add a new favorite company, to the most recently-added person
  void addCompany(std::string company);

  // returns true if favorites[i] is a subset of favorites[j]
  bool isSubsetOf(int i, int j) const;

  // returns true if favorites[i] is a subset of any other j
  bool aSubsetOfAnyOther(int i) const;

 private:
  std::vector<std::vector<std::string>> favorites;

};

std::vector<int> GoldenPeopleIndexes(std::vector<std::vector<std::string>> favorites);

#endif // GOLDEN_PEOPLE_INDEXES_H
