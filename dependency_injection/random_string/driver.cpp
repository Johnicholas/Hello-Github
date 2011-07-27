// Johnicholas Hines Jan 23 2007
// This is the entry point and options handling component.

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <cassert>
#include <cstdlib>
#include <getopt.h>

#include "language.h"
#include "cfg.h"

using namespace std;

#define PROGRAM_NAME "random_string"
#define PROGRAM_DESCRIPTION "Reads a grammar file and uses it to translate nonnegative integers on stdin to strings (in the language defined by that grammar) on stdout."
#define VERSION_STRING "0.1"
#define COPYRIGHT "Copyright 2007 IDEXX Laboratories, Inc."

// display usage info and exit
void usage() {
  cout << "Usage: " << PROGRAM_NAME << " [args]" << endl;
  cout << "  -g --grammar      Context free grammar file" << endl;
  cout << "  -v --version      Display full version information and exit" << endl;
  exit( 1 );
}

string int_to_string(int x) {
  stringstream ss;
  ss << x;
  string s;
  ss >> s;
  return s;
}

int main(int argc, char* argv[]) {
  const char * grammar_filename = 0;

  // parse the command line
  while ( true ) {
    static struct option opts[] = {
      // name, has_arg, flag* val
      {"grammar", 1, NULL, 'g'},
      {"version", 0, NULL, 'v'},
      {NULL, 0, NULL, 0} // Last option is always NULL
    };

    int c= getopt_long( argc, argv, "g:v", opts, NULL);
    if ( c == -1 ) {
      break;
    }
    switch ( c ) {
    case 'g': // grammar file
      grammar_filename= optarg;
      break;
    case 'v': // version
      cout << PROGRAM_DESCRIPTION << " V" << VERSION_STRING << endl;
      cout << COPYRIGHT << endl;
      exit( 0 );
      break;
    default:
      cerr << "Unknown arg: " << (char)c << endl;
    case '?': // missing argument
      usage();
      break;
    }
  } // end while true

  if( !grammar_filename ) {
    cerr << "Missing grammar filename" << endl;
    usage();
  }


  ifstream grammar_file;
  grammar_file.open( grammar_filename );
  if ( !grammar_file.is_open() ) {
    perror( grammar_filename );
    exit( 1 );
  }

  cfg current_language= parse_cfg( grammar_file );
  // cerr << print_cfg( current_language ) << endl; // DEBUG
  vector<language> nodes= to_language_vector( current_language );
  assert( nodes.size() > 0 );

  string unrank_target;
  while( cin >> unrank_target ) {
    num tmp;
    tmp.fromstring(unrank_target);
    nodes[0].unrank(cout, tmp);
    cout << endl;
  }

  return 0;
}
