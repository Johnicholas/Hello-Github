// Johnicholas Hines
// I don't know when I started this file, but 
// I believe it was months before Febuary 16 2007 (now)

#ifndef CFG_H
#define CFG_H

#include <string>
#include <map>
#include <vector>
#include <utility> // for pair
#include <iostream>
#include <cassert>
#include "language.h"

using namespace std;

class cfg {
 private:
  vector<string> symbol_cursor_to_name;
  map<string, unsigned int> name_to_symbol_cursor;
  vector<vector<unsigned int> > symbol_cursor_to_productions;
  vector<vector<unsigned int> > production_cursor_to_symbols;
 public:
  cfg() {
    symbol_cursor_to_name= vector<string>();
    name_to_symbol_cursor= map<string, unsigned int>();
    symbol_cursor_to_productions= vector<vector<unsigned int> >();
    production_cursor_to_symbols= vector<vector<unsigned int> >();
  };
  void add_symbol(string x) {
    if( name_to_symbol_cursor.find(x)==name_to_symbol_cursor.end() ) {
      name_to_symbol_cursor.insert(pair<string, unsigned int>(x, this->number_of_symbols()));
      symbol_cursor_to_name.push_back(x);
      symbol_cursor_to_productions.push_back(vector<unsigned int>());
    }
  };
  void add_production(unsigned int symbol_cursor, vector<unsigned int> x) {
    assert(symbol_cursor < this->number_of_symbols());
    for( unsigned int i= 0; i<x.size(); i++ ) {
      assert(x[i] < this->number_of_symbols());
    }
    symbol_cursor_to_productions[symbol_cursor].push_back(this->number_of_productions());
    production_cursor_to_symbols.push_back(x);
  };
  unsigned int number_of_symbols() {
    return symbol_cursor_to_name.size();
  }
  unsigned int number_of_productions() {
    return production_cursor_to_symbols.size();
  }
  string name(unsigned int symbol_cursor) {
    assert(symbol_cursor < this->number_of_symbols());
    return symbol_cursor_to_name[symbol_cursor];
  }
  unsigned int symbol_cursor(string name) {
    assert(name_to_symbol_cursor.find(name)!=name_to_symbol_cursor.end());
    return name_to_symbol_cursor.find(name)->second;
  }
  vector<unsigned int> productions(unsigned int symbol_cursor) {
    assert(symbol_cursor < this->number_of_symbols());
    return symbol_cursor_to_productions[symbol_cursor];
  };
  vector<unsigned int> sequence(unsigned int production_cursor) {
    assert(production_cursor < production_cursor_to_symbols.size() );
    return production_cursor_to_symbols[production_cursor];
  };
};

string print_cfg(cfg x) {
  ostringstream out;
  bool first_i= true;
  for( unsigned int i= 0; i<x.number_of_symbols(); i++ ) {
    vector<unsigned int> productions= x.productions(i);
    for( unsigned int j= 0; j<productions.size(); j++ ) {
      if( first_i ) {
	first_i= false;
      } else {
	out << " , ";
      }
      out << x.name(i) << " ::=";
      vector<unsigned int> sequence= x.sequence(productions[j]);
      for( unsigned int k= 0; k<sequence.size(); k++ ) {
	out << " "
	    << x.name(sequence[k]);
      }
    }
  }
  return out.str();
}

vector<language> to_language_vector(cfg x) {
  vector<language> accumulator; 
  vector<unsigned int> symbol_cursor_to_number;
  int next_free= 0;
  try {
    for( unsigned int i= 0; i < x.number_of_symbols(); i++ ) {
      vector<unsigned int> productions= x.productions(i);
      symbol_cursor_to_number.push_back(next_free);
      if( productions.size() == 0 ) {
	// the SINGLETON
	next_free++;
      } else if( productions.size() == 1 ) {
	vector<unsigned int> sequence= x.sequence(productions[0]);
	if( sequence.size() == 0 ) {
	  throw string("I can't handle epsilon productions yet, sorry\n");
	} else if( sequence.size() == 1 ) {
	  // the INDIRECTION node
	  next_free++;
	} else {
	  // the AND nodes
	  next_free += sequence.size() - 1;
	}
      } else {
	// the OR nodes
	next_free += productions.size() - 1;
	for( unsigned int j= 0; j<productions.size(); j++ ) {
	  vector<unsigned int> sequence= x.sequence(productions[j]);
	  if( sequence.size() == 0 ) {
	    throw string("I can't handle epsilon productions yet, sorry\n");
	  } else if( sequence.size() == 1 ) {
	    // the INDIRECTION node
	    next_free++;
	  } else {
	    // the AND nodes
	    next_free += sequence.size() - 1;
	  }
	}
      }
    }
    // at this point, we should have a map from symbol_cursors to numbers
    assert( symbol_cursor_to_number.size() == x.number_of_symbols() );
    accumulator.resize(next_free);
    next_free= 0;
    for( unsigned int i= 0; i<x.number_of_symbols(); i++ ) {
      vector<unsigned int> productions= x.productions(i);
      if( productions.size() == 0 ) {
	// top languagetype is SINGLETON
	accumulator[next_free].initialize_as_singleton(x.name(i));
	next_free++;
      } else if( productions.size() == 1 ) {
	// top languagetype might be AND
	vector<unsigned int> sequence= x.sequence(productions[0]);
	if( sequence.size() == 0 ) {
	  assert( false );
	} else if( sequence.size() == 1 ) {
	  accumulator[next_free].initialize_as_language(&(accumulator[symbol_cursor_to_number[sequence[0]]])); 
	  next_free++;
	} else {
	  // top language type will be AND
	  for( unsigned int j= 0; j < sequence.size() - 2; j++ ) {
	    accumulator[next_free].initialize_as_and(&(accumulator[symbol_cursor_to_number[sequence[j]]]), 
						     &(accumulator[next_free+1]));
	    next_free++;
	  }
	  accumulator[next_free].initialize_as_and(&(accumulator[symbol_cursor_to_number[sequence[sequence.size()-2]]]),
						   &(accumulator[symbol_cursor_to_number[sequence[sequence.size()-1]]]));
	  next_free++;
	}
      } else {
	// top languagetype will be OR
	for( unsigned int j= 0; j<productions.size(); j++ ) {
	  vector<unsigned int> sequence= x.sequence(productions[j]);
	  if( sequence.size() == 0 ) {
	    assert(false);
	  } else if( sequence.size() == 1 ) {
	    // OR node for this production
	    // don't do an OR node on the last
	    if( j+1 < productions.size() ) {
	      accumulator[next_free].initialize_as_or(&(accumulator[next_free+1]),
						      &(accumulator[next_free+2]));
	      next_free++;
	    }
	    accumulator[next_free].initialize_as_language(&(accumulator[symbol_cursor_to_number[sequence[0]]]));
	    next_free++;
	  } else {
	    if( j+1 < productions.size() ) {
	      // don't do an OR node on the last
	      accumulator[next_free].initialize_as_or(&(accumulator[next_free+1]),
						      &(accumulator[next_free+sequence.size()]));
	      next_free++;
	    }
	    for( unsigned int k= 0; k < sequence.size() - 2; k++ ) {
	      accumulator[next_free].initialize_as_and(&(accumulator[symbol_cursor_to_number[sequence[k]]]),
						       &(accumulator[next_free+1]));
	      next_free++;
	    }
	    accumulator[next_free].initialize_as_and(&(accumulator[symbol_cursor_to_number[sequence[sequence.size()-2]]]),
						     &(accumulator[symbol_cursor_to_number[sequence[sequence.size()-1]]]));
	    next_free++;
	  }
	}
      }
    }    
  } catch( string s ) {
    cerr << s;
    assert( false );
  }
  return accumulator;
}

cfg parse_cfg(ifstream& in) {
  int current_lhs= -1;
  vector<unsigned int> current_rhs;
  cfg answer;
  
  while( true ) {
    string token;
    in >> token;
    if( !in.good() ) {
      break;
    }
    answer.add_symbol(token);
    current_lhs= answer.symbol_cursor(token);
    current_rhs= vector<unsigned int>();
    in >> token;
    if( !in.good() ) {
      cerr << "parse error, expected a token. Current LHS is " << answer.name(current_lhs) << endl;
      exit( 1 );
    }
    if( token != "::=" ) {
      cerr << "parse error, expected '::='. Current LHS is " << answer.name(current_lhs) << endl;
      exit( 1 );
    }
    while( true ) {
      in >> token;
      if( !in.good() || token == "." ) {
	break;
      }
      answer.add_symbol(token);
      current_rhs.push_back(answer.symbol_cursor(token));
    } // end while true
    answer.add_production(current_lhs, current_rhs);
  } // end while true
  return answer;
}

#endif // CFG_H
