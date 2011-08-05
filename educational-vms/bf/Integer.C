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

#include <cstddef>
#include <cstring>
#include <iostream>
#include <gmp.h>

#include "Integer.h"

struct IntegerDoesNotDivideError : public IntegerError 
{ virtual const char *what() const throw() 
  { return "Inexact quotient"; } };

struct IntegerDivideByZeroError : public IntegerError
{ virtual const char *what() const throw() 
  { return "Attempt to divide by zero"; } };

std::ostream &operator <<(std::ostream &o, const Integer &a)
{
  int length = mpz_sizeinbase(a.val, 10) + 2;
  char *chars = new char[length];
  char *result = mpz_get_str(chars, 10, a.val);
  o << result;
  delete[] chars;
  return(o);
}

std::istream &operator >>(std::istream &i, Integer &a)
{
  static char *digits = "0123456789";
  char *p;
  int c;
  bool negative = false;

  for (;;)
  {
    if ((c = i.get()) == std::char_traits<char>::eof())
    {
      i.setstate(std::ios::failbit);
      return i;
    }
    if ((char)(unsigned char)c != ' '  && (char)(unsigned char)c != '\n' &&
        (char)(unsigned char)c != '\t' && (char)(unsigned char)c != '\v' &&
        (char)(unsigned char)c != '\f')
       break;
  }

  assert(c != std::char_traits<char>::eof());

  if ((char)(unsigned char)c == '-')
  {
    negative = true;
    if ((c = i.get()) == std::char_traits<char>::eof())
    {
      i.setstate(std::ios::failbit);
      return i;
    }
  }

  assert(c != std::char_traits<char>::eof());

  if ((char)(unsigned char)c == '\0' ||
      (p = strchr(digits, (char)(unsigned char)c)) == NULL)
  {
    i.setstate(std::ios::failbit);
    return i;
  }

  mpz_set_ui(a.val, (unsigned long)(p - digits));

  for (;;)
  {
    if ((c = i.peek()) == std::char_traits<char>::eof() ||
        (char)(unsigned char)c == '\0' ||
        (p = strchr(digits, (char)(unsigned char)c)) == NULL)
    {
      if (negative)
         mpz_neg(a.val, a.val);
       return i;
    }
    i.ignore();
    mpz_mul_ui(a.val, a.val, (unsigned long)10);
    mpz_add_ui(a.val, a.val, (unsigned long)(p - digits));
  }
  /*NOTREACHED*/
}

Integer operator +(const Integer &a, const Integer &b)
{
  Integer rv;

  mpz_add(rv.val, a.val, b.val);

  return(rv);
}

Integer operator +(const Integer &a, unsigned long b)
{
  Integer rv;

  mpz_add_ui(rv.val, a.val, b);
  return(rv);
}

Integer operator +(const Integer &a, long b)
{
  Integer rv;

  if (b >= 0)
     mpz_add_ui(rv.val, a.val, (unsigned long)b);
  else
     mpz_sub_ui(rv.val, a.val, (unsigned long)-b);

  return(rv);
}

Integer operator +(long a, const Integer &b)
{
  Integer rv;

  if (a >= 0)
     mpz_add_ui(rv.val, b.val, (unsigned long)a);
  else
     mpz_sub_ui(rv.val, b.val, (unsigned long)-a);

  return(rv);
}

Integer operator +(unsigned long a, const Integer &b)
{
  Integer rv;

  mpz_add_ui(rv.val, b.val, a);

  return(rv);
}

Integer operator -(const Integer &a)
{
  Integer rv;

  mpz_neg(rv.val, a.val);

  return(rv);
}

Integer operator -(const Integer &a, const Integer &b)
{
  Integer rv;

  mpz_sub(rv.val, a.val, b.val);

  return(rv);
}

Integer operator -(const Integer &a, unsigned long b)
{
  Integer rv;

  mpz_sub_ui(rv.val, a.val, (unsigned long)b);

  return(rv);
}

Integer operator -(const Integer &a, long b)
{
  Integer rv;

  if (b >= 0)
     mpz_sub_ui(rv.val, a.val, (unsigned long)b);
  else
     mpz_add_ui(rv.val, a.val, (unsigned long)-b);

  return(rv);
}

Integer operator -(long a, const Integer &b)
{
  Integer rv;

  if (a >= 0)
     mpz_ui_sub(rv.val, (unsigned long)a, b.val);
  else
  {
    mpz_add_ui(rv.val, b.val, (unsigned long)-a);
    mpz_neg(rv.val, rv.val);
  }
  return(rv);
}

Integer operator -(unsigned long a, const Integer &b)
{
  Integer rv;

  mpz_ui_sub(rv.val, a, b.val);

  return(rv);
}

Integer operator *(const Integer &a, const Integer &b)
{
  Integer rv;

  mpz_mul(rv.val, a.val, b.val);

  return(rv);
}

Integer operator *(const Integer &a, long b)
{
  Integer rv;

  mpz_mul_si(rv.val, a.val, b);

  return(rv);
}

Integer operator *(const Integer &a, unsigned long b)
{
  Integer rv;

  mpz_mul_ui(rv.val, a.val, b);

  return(rv);
}

Integer operator *(long a, const Integer &b)
{
  Integer rv;

  mpz_mul_si(rv.val, b.val, a);

  return(rv);
}

Integer operator *(unsigned long a, const Integer &b)
{
  Integer rv;

  mpz_mul_ui(rv.val, b.val, a);

  return(rv);
}

Integer operator /(const Integer &a, const Integer &b)
{
  Integer rv;

  mpz_fdiv_q(rv.val, a.val, b.val);

  return(rv);
}

Integer operator /(const Integer &a, long b)
{
  Integer rv;

  if (b < 0)
     return (-a) / (-b);

  if (b == 0)
     throw IntegerDivideByZeroError();
  (void)mpz_fdiv_q_ui(rv.val, a.val, (unsigned long)b);
  return(rv);
}

Integer operator /(const Integer &a, unsigned long b)
{
  Integer rv;

  if (b == 0)
     throw IntegerDivideByZeroError();
  (void)mpz_fdiv_q_ui(rv.val, a.val, b);
  return(rv);
}

Integer operator %(const Integer &a, const Integer &b)
{
  return a - (a / b) * b;
}

long operator %(const Integer &a, long b)
{
  return a - (a / b) * b;
}

unsigned long operator %(const Integer &a, unsigned long b)
{
  return a - (a / b) * b;
}

int operator %(const Integer &a, int b)
{
  return a - (a / b) * b;
}

unsigned int operator %(const Integer &a, unsigned int b)
{
  return a - (a / b) * b;
}

Integer &Integer::integral_divide_by(const Integer &a)
{
  static mpz_t remdr;
  static bool inited = false;

  if (!inited)
  {
    mpz_init(remdr);
    inited = true;
  }
  mpz_fdiv_qr(val, remdr, val, a.val);
  if (mpz_sgn(remdr) != 0)
     throw IntegerDoesNotDivideError();

  return(*this);
}

bool does_divide(const Integer &a, const Integer &b)
{
  static mpz_t remdr;
  static bool inited = false;

  if (mpz_sgn(b.val) == 0)
      return(true);    /* Everything divides 0 */
  if (mpz_sgn(a.val) == 0)
      return(false);   /* 0 doesn't divide nonzero */
  if (!inited)
  {
    mpz_init(remdr);
    inited = true;
  }
  mpz_fdiv_r(remdr, b.val, a.val);
  return(mpz_sgn(remdr) == 0);
}

Integer integral_quotient(const Integer &a, const Integer &b)
{
  Integer rv;

  static mpz_t remdr;
  static bool inited = false;

  if (!inited)
  {
    mpz_init(remdr);
    inited = true;
  }
  mpz_fdiv_qr(rv.val, remdr, a.val, b.val);
  if (mpz_sgn(remdr) != 0)
     throw IntegerDoesNotDivideError();
  return(rv);
}

Integer gcd(const Integer &a, const Integer &b)
{
  Integer rv;
  mpz_gcd(rv.val, a.val, b.val);
  return(rv);
}

Integer gcd_ext(const Integer &a, const Integer &b, 
                Integer *p_s, Integer *p_t)
{
  Integer rv, s, t;

  mpz_gcdext(rv.val, s.val, t.val, a.val, b.val);
  if (p_s)
     *p_s = s;
  if (p_t)
     *p_t = t;

  return(rv);
}

Integer pow(const Integer &a, unsigned long b)
{
  Integer rv;
  mpz_pow_ui(rv.val, a.val, b);
  return(rv);
}


Integer operator ~(const Integer &a)
{
  Integer rv;
  mpz_com(rv.val, a.val);
  return rv;
}

Integer operator &(const Integer &a, const Integer &b)
{
  Integer rv;
  mpz_and(rv.val, a.val, b.val);
  return rv;
}

Integer operator |(const Integer &a, const Integer &b)
{
  Integer rv;
  mpz_ior(rv.val, a.val, b.val);
  return rv;
}

Integer operator ^(const Integer &a, const Integer &b)
{
  Integer rv;
  mpz_xor(rv.val, a.val, b.val);
  return rv;
}

#define NUM_REPS 16
#define NUM_SMALL_PRIMES 7

/* Jaeschke, Math. Comp. 61, 915-926 */
int small_primes[] = {2,3,5,7,11,13,17};
char *sure_pp_limits[] = 
{"2047", "1373653", "25326001", "3215031751", 
 "2152302898747", "3474749660383", "341550071728321"};

IsPrime is_prime(const Integer &a)
{
  static mpz_t a1, d, temp_1, limit[NUM_SMALL_PRIMES];
  static bool inited = false;
  int h, i, j;

  if (!inited)
  {
    inited = true;
    mpz_init(a1);
    mpz_init(d);
    mpz_init(temp_1);
    for (i = 0; i < NUM_SMALL_PRIMES; i++)
        mpz_init_set_str(limit[i], sure_pp_limits[i], 10);
  }

  if (mpz_even_p(a.val))
     return ((mpz_cmp_ui(a.val, (unsigned long)2) == 0) ? IP_yes : IP_no);

  if (mpz_cmp_ui(a.val, (unsigned long)1) <= 0)
     return IP_no;

  mpz_sub_ui(a1, a.val, (unsigned long)1);
  h = mpz_scan1(a1, 0);
  mpz_fdiv_q_2exp(d, a1, h);
  
  for (i = 0; i < NUM_SMALL_PRIMES; i++)
  {
    mpz_set_ui(temp_1, (unsigned long)small_primes[i]);
    mpz_powm(temp_1, temp_1, d, a.val);
    if (mpz_cmp_ui(temp_1, (unsigned long)1) == 0 || mpz_cmp(temp_1, a1) == 0)
    {
      if (mpz_cmp(a.val, limit[i]) < 0)
         return(IP_yes);
      else
         continue;
    }
    for (j = 1; j < h; j++)
    {
      mpz_powm_ui(temp_1, temp_1, (unsigned long)2, a.val);
      if (mpz_cmp(temp_1, a1) == 0)
          break;
      if (mpz_cmp_ui(temp_1, (unsigned long)1) == 0)
          return(IP_no);
    }
    if (j == h)
        return(IP_no);
    if (mpz_cmp(a.val, limit[i]) < 0)
        return(IP_yes);
  }

  if (!mpz_probab_prime_p(a.val, NUM_REPS))
     return(IP_no);

  return(IP_probably);
}
