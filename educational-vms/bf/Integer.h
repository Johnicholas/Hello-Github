/* Copyright 2006 David Moews (dmoews@fastmail.fm)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

//
// Integers (& helping fxns)
//
#ifndef INTEGER_INCL

#define INTEGER_INCL


#include <gmp.h>
#include <iosfwd>
#include <limits>

#include "GenericError.h"

struct IntegerError : public GenericError { };

struct IntegerCantConvertError : public IntegerError 
{ virtual const char *what() const throw() 
  { return "Integer cannot be converted to given integral type"; } };

enum IsPrime
{
  IP_no, IP_yes, IP_probably
};

class Integer
{
  mpz_t val;
public:
  Integer() { mpz_init_set_ui(val, (unsigned long)0); }

  Integer(const char *s) { mpz_init_set_str(val, s, 10); }
  Integer(unsigned long i) { mpz_init_set_ui(val, i); }
  Integer(long i) { mpz_init_set_si(val, i); }
  Integer(double d) { mpz_init_set_d(val, d); }
  Integer(int i) { mpz_init_set_si(val, (long)i); }
  Integer(unsigned int i) { mpz_init_set_ui(val, (unsigned long)i); }
  Integer(const Integer &a) { mpz_init_set(val, a.val); }

  Integer &operator =(const Integer &a) { mpz_set(val, a.val); return(*this); }
  Integer &operator =(int i) { mpz_set_si(val, (long)i); return(*this); }
  Integer &operator =(unsigned int i) 
       { mpz_set_ui(val, (unsigned long)i); return(*this); }
  Integer &operator =(double d) { mpz_set_d(val, d); return(*this); }
  Integer &operator =(long i) { mpz_set_si(val, i); return(*this); }
  Integer &operator =(unsigned long i) { mpz_set_ui(val, i); return(*this); }
  Integer &operator =(const char *s) { mpz_set_str(val, s, 10); return(*this); }

  ~Integer() { mpz_clear(val); }

  Integer &operator --() 
          { mpz_sub_ui(val, val, (unsigned long)1); return *this; }
  Integer &operator ++() 
          { mpz_add_ui(val, val, (unsigned long)1); return *this; }
  Integer operator --(int) 
          { Integer temp = *this; --(*this); return temp; }
  Integer operator ++(int) 
          { Integer temp = *this; ++(*this); return temp; }

  Integer &operator +=(const Integer &a) 
          { mpz_add(val, val, a.val); return(*this); }
  Integer &operator -=(const Integer &a) 
          { mpz_sub(val, val, a.val); return(*this); }
  Integer &operator *=(const Integer &a) 
          { mpz_mul(val, val, a.val); return(*this); }
  // Truncates toward -infinity
  Integer &operator /=(const Integer &a) 
          { mpz_fdiv_q(val, val, a.val); return(*this); }
  // Remainder has same sign as divisors
  Integer &operator %=(const Integer &a) 
          { *this -= (*this / a) * a; return *this; }

  Integer &operator &=(const Integer &a) 
          { mpz_and(val, val, a.val); return(*this); }
  Integer &operator |=(const Integer &a) 
          { mpz_ior(val, val, a.val); return(*this); }
  Integer &operator ^=(const Integer &a) 
          { mpz_xor(val, val, a.val); return(*this); }

  Integer &integral_divide_by(const Integer &a);

  operator long() const 
  { 
    if (!mpz_fits_slong_p(val))
       throw IntegerCantConvertError();
    return(mpz_get_si(val));
  }

  operator unsigned long() const 
  { 
    if (!mpz_fits_ulong_p(val))
       throw IntegerCantConvertError();
    return(mpz_get_ui(val));
  }

  operator int() const 
  { 
    if (!mpz_fits_sint_p(val))
       throw IntegerCantConvertError();
    return(mpz_get_si(val));
  }

  operator unsigned int() const 
  { 
    if (!mpz_fits_uint_p(val))
       throw IntegerCantConvertError();
    return(mpz_get_ui(val));
  }

  operator short() const 
  { 
    if (!mpz_fits_sshort_p(val))
       throw IntegerCantConvertError();
    return(mpz_get_si(val));
  }

  operator unsigned short() const 
  { 
    if (!mpz_fits_ushort_p(val))
       throw IntegerCantConvertError();
    return(mpz_get_ui(val));
  }

  operator signed char() const 
  { 
    if (!mpz_fits_sint_p(val))
       throw IntegerCantConvertError();
    int i = mpz_get_si(val);
    if (i > std::numeric_limits<signed char>::max() ||
        i < std::numeric_limits<signed char>::min())
       throw IntegerCantConvertError();
    return i;
  }

  operator unsigned char() const 
  { 
    if (!mpz_fits_uint_p(val))
       throw IntegerCantConvertError();
    unsigned u = mpz_get_ui(val);
    if (u > std::numeric_limits<unsigned char>::max())
       throw IntegerCantConvertError();
    return u;
  }

  operator char() const 
  {
    if (std::numeric_limits<char>::is_signed)
       return (signed char)*this;
    else
       return (unsigned char)*this;
  }

  operator double() const
  {
    return mpz_get_d(val);
  }

  // Return value that (1) fits into a long and (2) is correct modulo 
  // 2^(8 sizeof(long)).
  long value_modulo_long() const { return mpz_get_si(val); }

  // Maximum i s.t. 2^i divides the number (or ULONG_MAX if the number is zero.)
  unsigned long power_of_2_dividing() const { return mpz_scan1(val, 0ul); }
 
  friend Integer operator -(const Integer &);

  friend Integer operator +(const Integer &, const Integer &);
  friend Integer operator +(const Integer &, long);
  friend Integer operator +(const Integer &, unsigned long);
  friend Integer operator +(long, const Integer &);
  friend Integer operator +(unsigned long, const Integer &);
  inline friend Integer operator +(const Integer &a, int b)
  { return a + (long)b; }
  inline friend Integer operator +(const Integer &a, unsigned int b)
  { return a + (unsigned long)b; }
  inline friend Integer operator +(int a, const Integer &b) 
  { return (long)a + b; }
  inline friend Integer operator +(unsigned int a, const Integer &b) 
  { return (unsigned long)a + b; }

  friend Integer operator -(const Integer &, const Integer &);
  friend Integer operator -(const Integer &, long);
  friend Integer operator -(const Integer &, unsigned long);
  friend Integer operator -(long, const Integer &);
  friend Integer operator -(unsigned long, const Integer &);
  inline friend Integer operator -(const Integer &a, int b)
  { return a - (long)b; }
  inline friend Integer operator -(const Integer &a, unsigned int b)
  { return a - (unsigned long)b; }
  inline friend Integer operator -(int a, const Integer &b) 
  { return (long)a - b; }
  inline friend Integer operator -(unsigned int a, const Integer &b) 
  { return (unsigned long)a - b; }

  friend Integer operator *(const Integer &, const Integer &);
  friend Integer operator *(const Integer &, long);
  friend Integer operator *(const Integer &, unsigned long);
  friend Integer operator *(long, const Integer &);
  friend Integer operator *(unsigned long, const Integer &);
  inline friend Integer operator *(const Integer &a, int b)
  { return a * (long)b; }
  inline friend Integer operator *(const Integer &a, unsigned int b)
  { return a * (unsigned long)b; }
  inline friend Integer operator *(int a, const Integer &b) 
  { return (long)a * b; }
  inline friend Integer operator *(unsigned int a, const Integer &b) 
  { return (unsigned long)a * b; }

  // Truncates toward -infinity
  friend Integer operator /(const Integer &, const Integer &);
  friend Integer operator /(const Integer &, long);
  friend Integer operator /(const Integer &, unsigned long);
  inline friend Integer operator /(const Integer &a, int b)
  { return a / (long)b; }
  inline friend Integer operator /(const Integer &a, unsigned int b)
  { return a / (unsigned long)b; }

  // Remainder has same sign as divisor
  friend Integer operator %(const Integer &, const Integer &);
  friend long operator %(const Integer &, long);
  friend unsigned long operator %(const Integer &, unsigned long);
  friend int operator %(const Integer &, int);
  friend unsigned int operator %(const Integer &, unsigned int);

  friend Integer integral_quotient(const Integer &, const Integer &);

  friend Integer pow(const Integer &, unsigned long);
  friend Integer gcd(const Integer &, const Integer &);

  // Set 3rd and 4th args to s, t s.t. (arg 1)s + (arg 2)t = gcd(args).
  friend Integer gcd_ext(const Integer &, const Integer &,
                         Integer *, Integer *);
    
  friend std::istream &operator >>(std::istream &, Integer &);

  friend std::ostream &operator <<(std::ostream &, const Integer &);


  inline friend bool operator <(const Integer &a, const Integer &b)
  { return mpz_cmp(a.val, b.val) < 0; }

  inline friend bool operator <(long a, const Integer &b)
  { return mpz_cmp_si(b.val, a) > 0; }

  inline friend bool operator <(unsigned long a, const Integer &b)
  { return mpz_cmp_ui(b.val, a) > 0; }

  inline friend bool operator <(const Integer &a, long b)
  { return mpz_cmp_si(a.val, b) < 0; }

  inline friend bool operator <(const Integer &a, unsigned long b)
  { return mpz_cmp_ui(a.val, b) < 0; }

  inline friend bool operator <(int a, const Integer &b)
  { return mpz_cmp_si(b.val, (long)a) > 0; }

  inline friend bool operator <(unsigned int a, const Integer &b)
  { return mpz_cmp_ui(b.val, (unsigned long)a) > 0; }

  inline friend bool operator <(const Integer &a, int b)
  { return mpz_cmp_si(a.val, (long)b) < 0; }

  inline friend bool operator <(const Integer &a, unsigned int b)
  { return mpz_cmp_ui(a.val, (unsigned long)b) < 0; }


  inline friend bool operator >(const Integer &a, const Integer &b)
  { return mpz_cmp(a.val, b.val) > 0; }

  inline friend bool operator >(long a, const Integer &b)
  { return mpz_cmp_si(b.val, a) < 0; }

  inline friend bool operator >(unsigned long a, const Integer &b)
  { return mpz_cmp_ui(b.val, a) < 0; }

  inline friend bool operator >(const Integer &a, long b)
  { return mpz_cmp_si(a.val, b) > 0; }

  inline friend bool operator >(const Integer &a, unsigned long b)
  { return mpz_cmp_ui(a.val, b) > 0; }

  inline friend bool operator >(int a, const Integer &b)
  { return mpz_cmp_si(b.val, (long)a) < 0; }

  inline friend bool operator >(unsigned int a, const Integer &b)
  { return mpz_cmp_ui(b.val, (unsigned long)a) < 0; }

  inline friend bool operator >(const Integer &a, int b)
  { return mpz_cmp_si(a.val, (long)b) > 0; }

  inline friend bool operator >(const Integer &a, unsigned int b)
  { return mpz_cmp_ui(a.val, (unsigned long)b) > 0; }


  inline friend bool operator ==(const Integer &a, const Integer &b)
  { return mpz_cmp(a.val, b.val) == 0; }

  inline friend bool operator ==(long a, const Integer &b)
  { return mpz_cmp_si(b.val, a) == 0; }

  inline friend bool operator ==(unsigned long a, const Integer &b)
  { return mpz_cmp_ui(b.val, a) == 0; }

  inline friend bool operator ==(const Integer &a, long b)
  { return mpz_cmp_si(a.val, b) == 0; }

  inline friend bool operator ==(const Integer &a, unsigned long b)
  { return mpz_cmp_ui(a.val, b) == 0; }

  inline friend bool operator ==(int a, const Integer &b)
  { return mpz_cmp_si(b.val, (long)a) == 0; }

  inline friend bool operator ==(unsigned int a, const Integer &b)
  { return mpz_cmp_ui(b.val, (unsigned long)a) == 0; }

  inline friend bool operator ==(const Integer &a, int b)
  { return mpz_cmp_si(a.val, (long)b) == 0; }

  inline friend bool operator ==(const Integer &a, unsigned int b)
  { return mpz_cmp_ui(a.val, (unsigned long)b) == 0; }


  inline friend bool operator !=(const Integer &a, const Integer &b)
  { return mpz_cmp(a.val, b.val) != 0; }

  inline friend bool operator !=(long a, const Integer &b)
  { return mpz_cmp_si(b.val, a) != 0; }

  inline friend bool operator !=(unsigned long a, const Integer &b)
  { return mpz_cmp_ui(b.val, a) != 0; }

  inline friend bool operator !=(const Integer &a, long b)
  { return mpz_cmp_si(a.val, b) != 0; }

  inline friend bool operator !=(const Integer &a, unsigned long b)
  { return mpz_cmp_ui(a.val, b) != 0; }

  inline friend bool operator !=(int a, const Integer &b)
  { return mpz_cmp_si(b.val, (long)a) != 0; }

  inline friend bool operator !=(unsigned int a, const Integer &b)
  { return mpz_cmp_ui(b.val, (unsigned long)a) != 0; }

  inline friend bool operator !=(const Integer &a, int b)
  { return mpz_cmp_si(a.val, (long)b) != 0; }

  inline friend bool operator !=(const Integer &a, unsigned int b)
  { return mpz_cmp_ui(a.val, (unsigned long)b) != 0; }


  inline friend bool operator >=(const Integer &a, const Integer &b)
  { return mpz_cmp(a.val, b.val) >= 0; }

  inline friend bool operator >=(long a, const Integer &b)
  { return mpz_cmp_si(b.val, a) <= 0; }

  inline friend bool operator >=(unsigned long a, const Integer &b)
  { return mpz_cmp_ui(b.val, a) <= 0; }

  inline friend bool operator >=(const Integer &a, long b)
  { return mpz_cmp_si(a.val, b) >= 0; }

  inline friend bool operator >=(const Integer &a, unsigned long b)
  { return mpz_cmp_ui(a.val, b) >= 0; }

  inline friend bool operator >=(int a, const Integer &b)
  { return mpz_cmp_si(b.val, (long)a) <= 0; }

  inline friend bool operator >=(unsigned int a, const Integer &b)
  { return mpz_cmp_ui(b.val, (unsigned long)a) <= 0; }

  inline friend bool operator >=(const Integer &a, int b)
  { return mpz_cmp_si(a.val, (long)b) >= 0; }

  inline friend bool operator >=(const Integer &a, unsigned int b)
  { return mpz_cmp_ui(a.val, (unsigned long)b) >= 0; }


  inline friend bool operator <=(const Integer &a, const Integer &b)
  { return mpz_cmp(a.val, b.val) <= 0; }

  inline friend bool operator <=(long a, const Integer &b)
  { return mpz_cmp_si(b.val, a) >= 0; }

  inline friend bool operator <=(unsigned long a, const Integer &b)
  { return mpz_cmp_ui(b.val, a) >= 0; }

  inline friend bool operator <=(const Integer &a, long b)
  { return mpz_cmp_si(a.val, b) <= 0; }

  inline friend bool operator <=(const Integer &a, unsigned long b)
  { return mpz_cmp_ui(a.val, b) <= 0; }

  inline friend bool operator <=(int a, const Integer &b)
  { return mpz_cmp_si(b.val, (long)a) >= 0; }

  inline friend bool operator <=(unsigned int a, const Integer &b)
  { return mpz_cmp_ui(b.val, (unsigned long)a) >= 0; }

  inline friend bool operator <=(const Integer &a, int b)
  { return mpz_cmp_si(a.val, (long)b) <= 0; }

  inline friend bool operator <=(const Integer &a, unsigned int b)
  { return mpz_cmp_ui(a.val, (unsigned long)b) <= 0; }


  friend Integer operator ~(const Integer &);
  friend Integer operator &(const Integer &, const Integer &);
  friend Integer operator |(const Integer &, const Integer &);
  friend Integer operator ^(const Integer &, const Integer &);

  friend bool does_divide(const Integer &, const Integer &);
 
  friend IsPrime is_prime(const Integer &);
};

#endif
