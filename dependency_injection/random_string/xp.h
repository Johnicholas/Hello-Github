// Johnicholas Hines January 27 2007
// I did not write this;
// it is taken from the book C Interfaces and Implementations
#ifndef XP_INCLUDED
#define XP_INCLUDED
#include <vector>
#include <utility>
using namespace std;
typedef vector<unsigned char> T;
extern void debugprint(T x);
extern T AP_add( T x, T y );
extern int XP_add(int n, T::iterator z, T::const_iterator x, T::const_iterator y, int carry);
extern T AP_sub( T x, T y );
extern int XP_sub(int n, T::iterator z, T::const_iterator x, T::const_iterator y, int borrow);
T AP_mul( T x, T y );
extern int XP_mul(T::iterator z, int n, T::const_iterator x, int m, T::const_iterator y);
extern pair<T, T> AP_div( T x, T y );
extern int XP_div(int n, T::iterator q, T::const_iterator x, int m, T::const_iterator y, T::iterator r);
extern void AP_sum(T& z, int carry);
extern int XP_sum(int n, T::iterator z, T::const_iterator x, int y);
extern int XP_diff(int n, T::iterator z, T::const_iterator x, int y);
extern void AP_product( T& z, int y );
extern int XP_product(int n, T::iterator z, T::const_iterator x, int y);
extern T AP_quotient( int& remainder, T x, int y );
extern int XP_quotient(int n, T::iterator z, T::const_iterator x, int y);
extern int XP_neg(int n, T::iterator z, T::const_iterator x, int carry);
extern int AP_cmp( T x, T y );
extern int XP_cmp(int n, T::const_iterator x, T::const_iterator y);
extern void XP_lshift(int n, T::iterator z, int m, T::const_iterator x, int s, int fill);
extern void XP_rshift(int n, T::iterator z, int m, T::const_iterator x, int s, int fill);
extern int AP_length( T x );
extern int XP_length (int n, T::const_iterator x);
extern unsigned long XP_fromint(int n, T::iterator z, unsigned long u);
extern unsigned long XP_toint  (int n, T::const_iterator x);
extern T AP_fromstr(const char *str, int base);
extern int XP_fromstr(int n, T::iterator z, const char *str, int base, char **end);
extern string AP_tostr(T x, int base);
extern void XP_tostr(char *str, int size, int base, int n, T::iterator x);
#endif
