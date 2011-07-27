#include <ctype.h>
#include <string.h>
#include <cassert>
#include "xp.h"
#define BASE (1<<8)
#include <iostream>
using namespace std;
void debugprint(T x) {
  for( unsigned int i= 0; i < x.size(); i++ ) {
    cout << (( x[i] == '\0' )?".":"#");
  }
  cout << endl;
}
static char map[] = {
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
  36, 36, 36, 36, 36, 36, 36,
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
  23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
  36, 36, 36, 36, 36, 36,
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
  23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35
};
T AP_fromint(unsigned long u) {
  T z;
  while( u != 0 ) {
    z.push_back( u % BASE );
    u /= BASE;
  }
  return z;
}
unsigned long XP_fromint(int n, T::iterator z, unsigned long u) {
  int i = 0;
  do {
    z[i++] = u % BASE;
  } while ((u /= BASE) > 0 && i < n);
  for ( ; i < n; i++) {
    z[i] = 0;
  }
  return u;
}
unsigned long XP_toint(int n, T::const_iterator x) {
  unsigned long u = 0;
  int i = (int)sizeof u;
  if (i > n) {
    i = n;
  }
  while (--i >= 0) {
    u = BASE*u + x[i];
  }
  return u;
}
int AP_length( T x ) {
  for( int i= x.size()-1; i >= 0; i-- ) {
    if( x[i] != '\0' ) {
      return i+1;
    }
  }
  return 0;
}
int XP_length(int n, T::const_iterator x) {
  while (n > 1 && x[n-1] == 0) {
    n--;
  }
  return n;
}
T AP_add(T x, T y) {
  T z(max(x.size(), y.size()), '\0');
  int carry= 0;
  int n= min(x.size(), y.size());
  for( int i= 0; i < n; i++ ) {
    carry += x[i] + y[i];
    z[i] = carry % BASE;
    carry /= BASE;
  }
  T* longer;
  longer= (x.size()>y.size())?&x:&y;
  for( unsigned int i= n; i < z.size(); i++ ) {
    carry += (*longer)[i];
    z[i] = carry % BASE;
    carry /= BASE;
  }
  while( carry != 0 ) {
    z.push_back( carry % BASE );
    carry /= BASE;
  }
  return z;
}
int XP_add(int n, T::iterator z, T::const_iterator x, T::const_iterator y, int carry) {
  int i;
  for (i = 0; i < n; i++) {
    carry += x[i] + y[i];
    z[i] = carry%BASE;
    carry /= BASE;
  }
  return carry;
}
T AP_sub( T x, T y ) {
  // cerr << "starting ap_sub" << endl;
  T z;
  if( AP_cmp(x, y) < 0 ) {
    // cerr << "from ap_sub returning zero" << endl;
    return z;
  }
  // cerr << "from ap_sub not returning zero" << endl;
  z.resize(max(x.size(), y.size()), '\0');
  int borrow= 0;
  int n= min(x.size(), y.size());
  for( int i= 0; i < n; i++ ) {
    int d= (x[i] + BASE) - borrow - y[i];
    z[i]= d % BASE;
    borrow= 1 - d / BASE;
  }
  if( x.size() > y.size() ) {
    for( unsigned int i= n; i < z.size(); i++ ) {
      int d= (x[i] + BASE) - borrow;
      z[i]= d % BASE;
      borrow= 1 - d / BASE;
    }
  } else {
    for( unsigned int i= n; i < z.size(); i++ ) {
      int d= BASE - borrow - y[i];
      z[i]= d % BASE;
      borrow= 1 - d / BASE;
    }
  }
  assert( borrow == 0 );
  return z;
}
int XP_sub(int n, T::iterator z, T::const_iterator x, T::const_iterator y, int borrow) {
  int i;
  for (i = 0; i < n; i++) {
    int d = (x[i] + BASE) - borrow - y[i];
    z[i] = d%BASE;
    borrow = 1 - d/BASE;
  }
  return borrow;
}
void AP_sum(T& z, int carry) {
  for( unsigned int i= 0; i < z.size(); i++ ) {
    carry += z[i];
    z[i] = carry % BASE;
    carry /= BASE;
  }
  while( carry != 0 ) {
    z.push_back( carry % BASE );
    carry /= BASE;
  }
}
int XP_sum(int n, T::iterator z, T::const_iterator x, int y) {
  int i;
  for (i = 0; i < n; i++) {
    y += x[i];
    z[i] = y % BASE;
    y /= BASE;
  }
  return y;
}
int XP_diff(int n, T::iterator z, T::const_iterator x, int y) {
  int i;
  for (i = 0; i < n; i++) {
    int d = (x[i] + BASE) - y;
    z[i] = d%BASE;
    y = 1 - d/BASE;
  }
  return y;
}
int XP_neg(int n, T::iterator z, T::const_iterator x, int carry) {
  int i;
  for (i = 0; i < n; i++) {
    carry += (unsigned char)~x[i];
    z[i] = carry%BASE;
    carry /= BASE;
  }
  return carry;
}
T AP_mul( T x, T y ) {
  int x_size= x.size();
  int y_size= y.size();
  T z(x_size + y_size, '\0');
  int carryout= 0;
  for( int i= 0; i < x_size; i++ ) {
    unsigned carry= 0;
    int j;
    for( j= 0; j < y_size; j++ ) {
      carry += x[i] * y[j] + z[i + j];
      z[i + j]= carry % BASE;
      carry /= BASE;
    }
    for( ; j < x_size + y_size - i; j++ ) {
      carry += z[i + j];
      z[i + j]= carry % BASE;
      carry /= BASE;
    }
    carryout |= carry;
  }
  while( carryout != 0 ) {
    z.push_back( carryout % BASE );
    carryout /= BASE;
  }
  return z;
}
int XP_mul(T::iterator z, int n, T::const_iterator x, int m, T::const_iterator y) {
  int i, j, carryout = 0;
  for (i = 0; i < n; i++) {
    unsigned carry = 0;
    for (j = 0; j < m; j++) {
      carry += x[i] * y[j] + z[i + j];
      z[i+j] = carry % BASE;
      carry /= BASE;
    }
    for ( ; j < n + m - i; j++) {
      carry += z[i + j];
      z[i + j] = carry % BASE;
      carry /= BASE;
    }
    carryout |= carry;
  }
  return carryout;
}
void AP_product( T& z, int y ) {
  int carry= 0;
  for( unsigned int i= 0; i < z.size(); i++ ) {
    carry += z[i] * y;
    z[i] = carry % BASE;
    carry /= BASE;
  }
  while( carry != 0 ) {
    z.push_back( carry % BASE );
    carry /= BASE;
  }
}
int XP_product(int n, T::iterator z, T::const_iterator x, int y) {
  int i;
  unsigned carry = 0;
  for (i = 0; i < n; i++) {
    carry += x[i]*y;
    z[i] = carry%BASE;
    carry /= BASE;
  }
  return carry;
}
pair<T, T> AP_div( T x, T y ) {
  int x_size= x.size();
  int y_size= y.size();
  T q(x_size, '\0');
  T r(y_size, '\0');
  if( XP_div(x_size, q.begin(), x.begin(), y_size, y.begin(), r.begin()) == 0 ) {
    throw "Divide by zero error in AP_div";
  }
  return pair<T, T>(q, r);
}
int XP_div(int n, T::iterator q, T::const_iterator x, int m, T::const_iterator y, T::iterator r) {
  T tmp_rep(n+m+2);
  T::iterator tmp;
  int nx = n, my = m;
  n = XP_length(n, x);
  m = XP_length(m, y);
  if (m == 1) {
    if (y[0] == 0)
      return 0;
    r[0] = XP_quotient(nx, q, x, y[0]);
    for( int i= 1; i < my; i++ ) {
      r[i]= '\0';
    }
  } else if (m > n) {
    for( int i= 0; i < nx; i++ ) {
      q[i] = '\0';
    }
    for( int i= 0; i < n; i++ ) {
      r[i]= x[i];
    }
    for( int i= n; i < n; i++ ) {
      r[i]= '\0';
    }
  } else {
    int k;
    T::iterator rem = tmp_rep.begin();
    T::iterator dq = tmp_rep.begin() + n + 1;
    assert(2 <= m && m <= n);
    for( int i= 0; i < n; i++ ) {
      rem[i]= x[i];
    }
    rem[n] = 0;
    for (k = n - m; k >= 0; k--) {
      int qk;
      {
	int i;
	assert(2 <= m && m <= k+m && k+m <= n);
	{
	  int km = k + m;
	  unsigned long y2 = y[m-1]*BASE + y[m-2];
	  unsigned long r3 = rem[km]*(BASE*BASE) +
	    rem[km-1]*BASE + rem[km-2];
	  qk = r3/y2;
	  if (qk >= BASE)
	    qk = BASE - 1;
	}
	dq[m] = XP_product(m, dq, y, qk);
	for (i = m; i > 0; i--)
	  if (rem[i+k] != dq[i])
	    break;
	if (rem[i+k] < dq[i])
	  dq[m] = XP_product(m, dq, y, --qk);
      }
      q[k] = qk;
      {
	int borrow;
	assert(0 <= k && k <= k+m);
	borrow = XP_sub(m + 1, rem + k, rem + k, dq, 0);
	assert(borrow == 0);
      }
    }
    for( int i= 0; i < m; i++ ) {
      r[i]= rem[i];
    }
    {
      int i;
      for (i = n-m+1; i < nx; i++)
	q[i] = 0;
      for (i = m; i < my; i++)
	r[i] = 0;
    }
  }
  return 1;
}
int XP_quotient(int n, T::iterator z, T::const_iterator x, int y) {
  int i;
  unsigned carry = 0;
  for (i = n - 1; i >= 0; i--) {
    carry = carry*BASE + x[i];
    z[i] = carry/y;
    carry %= y;
  }
  return carry;
}
int AP_cmp( T x, T y ) {
  // cerr << "starting ap_cmp" << endl;
  int x_size= x.size();
  int y_size= y.size();
  int i;
  if( x_size > y_size ) {
    for( i= x_size-1; i >= y_size; i-- ) {
      if( x[i] != 0 ) {
	// cerr << "ap_cmp returning 1" << endl;
	return 1;
      }
    }
  } else {
    for( i= y_size-1; i >= x_size; i-- ) {
      if( y[i] != 0 ) {
	// cerr << "ap_cmp returning -1" << endl;
	return -1;
      }
    }
  }
  for( ; i >= 0; i-- ) {
    if( x[i] != y[i] ) {
      // cerr << "ap_cmp returning " << x[i] - y[i] << endl;
      return x[i] - y[i];
    }
  }
  // cerr << "ap_cmp returning 0" << endl;
  return 0;
}
int XP_cmp(int n, T::const_iterator x, T::const_iterator y) {
  int i = n - 1;
  while (i > 0 && x[i] == y[i])
    i--;
  return x[i] - y[i];
}
void XP_lshift(int n, T::iterator z, int m, T::const_iterator x, int s, int fill) {
  fill = fill ? 0xFF : 0;
  {
    int i, j = n - 1;
    if (n > m)
      i = m - 1;
    else
      i = n - s/8 - 1;
    for ( ; j >= m + s/8; j--)
      z[j] = 0;
    for ( ; i >= 0; i--, j--)
      z[j] = x[i];
    for ( ; j >= 0; j--)
      z[j] = fill;
  }
  s %= 8;
  if (s > 0)
    {
      XP_product(n, z, z, 1<<s);
      z[0] |= fill>>(8-s);
    }
}
void XP_rshift(int n, T::iterator z, int m, T::const_iterator x, int s, int fill) {
  fill = fill ? 0xFF : 0;
  {
    int i, j = 0;
    for (i = s/8; i < m && j < n; i++, j++)
      z[j] = x[i];
    for ( ; j < n; j++)
      z[j] = fill;
  }
  s %= 8;
  if (s > 0)
    {
      XP_quotient(n, z, z, 1<<s);
      z[n-1] |= fill<<(8-s);
    }
}
T AP_fromstr(const char* str, int base) {
  // cout << "doing AP_fromstr, z is ";
  T z;
  // debugprint(z);
  assert( str );
  assert( base >= 2 && base <= 36 );
  // int carry= 0;
  for( const char *p= str; *p && isalnum(*p) && map[*p-'0'] < base; p++ ) {
    // cout << "  top of loop" << endl;
    AP_product(z, base);
    // cout << "  after product, z is ";
    // debugprint(z);
    AP_sum(z, map[*p-'0']);
    // cout << "  after sum, z is ";
    // debugprint(z);
  }
  return z;
}
int XP_fromstr(int n, T::iterator z, const char *str, int base, char **end) {
  const char *p = str;
  assert(p);
  assert(base >= 2 && base <= 36);
  while (*p && isspace(*p))
    p++;
  if ((*p && isalnum(*p) && map[*p-'0'] < base)) {
    int carry;
    for ( ; (*p && isalnum(*p) && map[*p-'0'] < base); p++) {
      carry = XP_product(n, z, z, base);
      if (carry)
	break;
      XP_sum(n, z, z, map[*p-'0']);
    }
    if (end)
      *end = (char *)p;
    return carry;
  } else {
    if (end)
      *end = (char *)str;
    return 0;
  }
}
string AP_tostr(T x, int base) {
  string s;
  assert(base >= 2 && base <= 36);
  do {
    int r= XP_quotient(x.size(), x.begin(), x.begin(), base);
    string digits= "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"; 
    s.append( digits, r, 1 );
  } while( AP_length(x) != 0 );
  string reverse_s( s.rbegin(), s.rend() );
  return reverse_s;
}
void XP_tostr(char *str, int size, int base, int n, T::iterator x) {
  int i = 0;
  assert(str);
  assert(base >= 2 && base <= 36);
  do {
    int r = XP_quotient(n, x, x, base);
    assert(i < size);
    str[i++] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[r];
    while (n > 1 && x[n-1] == 0) {
      n--;
    }
  } while (n > 1 || x[0] != 0);
  assert(i < size);
  str[i] = '\0';
  {
    int j;
    for (j = 0; j < --i; j++) {
      char c = str[j];
      str[j] = str[i];
      str[i] = c;
    }
  }
}
