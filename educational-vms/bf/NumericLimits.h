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

#ifndef NUMERIC_LIMITS_INCL

#define NUMERIC_LIMITS_INCL

#include <limits>

#include "GenericError.h"
#include "Integer.h"

// This class gives information on integral type 
// upper and lower bounds, as Integers.

struct TypeInfoError : public GenericError { };

struct TypeInfoBoundError : public TypeInfoError 
{ virtual const char *what() const throw() 
  { return "Type has no upper and lower bounds"; } };

template<class IntegralType>
struct NumericLimits
{
  static Integer max() { return std::numeric_limits<IntegralType>::max(); }
  static Integer min() { return std::numeric_limits<IntegralType>::min(); }

  static bool is_unbounded() { return false; }
  static bool is_unsigned_binary() 
  {
    unsigned long bits_in_type = (max() - min() + 1).power_of_2_dividing();
    return min() == 0 && max() == pow(Integer(2), bits_in_type) - 1;
  }
  static bool is_signed_binary() 
  {
    unsigned long bits_in_type = (max() - min() + 1).power_of_2_dividing();
    return bits_in_type > 0 &&
           min() == -pow(Integer(2), bits_in_type-1) && 
           max() == pow(Integer(2), bits_in_type-1) - 1;
  }
};

template<>
struct NumericLimits<Integer>
{
  static Integer max() { throw TypeInfoBoundError(); return 0; }
  static Integer min() { throw TypeInfoBoundError(); return 0; }

  static bool is_unbounded() { return true; }
  static bool is_signed_binary() { return false; }
  static bool is_unsigned_binary() { return false; }
};

#endif
