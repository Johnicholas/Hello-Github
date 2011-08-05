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

#include <limits>
#include <cassert>
#include <cstddef>
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <utility>

#include "BF.h"
#include "Integer.h"

#include "BFImpl.h"

// See if arg1 is within bounds arg2 & arg3; should have arg2 <= arg3.
bool is_within(const Integer &i, const Bound &lo, const Bound &hi)
{
  assert(lo.is_infinite() || hi.is_infinite() || lo.value() <= hi.value());
  return lo.is_le(i) && hi.is_ge(i);
}

// Trim arg1 to within bounds arg2 & arg3; should have arg2 <= arg3.
Integer trim_to(const Integer &i, const Bound &lo, const Bound &hi)
{
  assert(lo.is_infinite() || hi.is_infinite() || lo.value() <= hi.value());
  if (!lo.is_le(i))
     return lo.value();
  else if (!hi.is_ge(i))
     return hi.value();
  else
     return i;
}

std::ostream &operator <<(std::ostream &o, const End &e)
{
  if (e.is_infinite())
  {
    o << "infinite";
    return o;
  }

  o << e.value() << ' ';
  switch (e.type())
  {
    case ET_wraparound:
         o << "wraparound";
         break;

    case ET_truncate:
         o << "limiting";
         break;

    case ET_abort:
         o << "abort";
         break;

    case ET_undefined:
         o << "undefined";
         break;

    default:
         assert(0);
         break;
  }
  return o;
}

void BiVector::trim_bivector_at_left()
{
  for (std::map<Integer, Integer>::iterator i = contents.begin();
       i != contents.end(); )
  {
    std::map<Integer, Integer>::iterator i_next = i;
    ++i_next;
    if (!left.is_le(i->first))
       contents.erase(i);
    i = i_next;
  }
}

void BiVector::trim_bivector_at_right() 
{
  for (std::map<Integer, Integer>::iterator i = contents.begin();
       i != contents.end(); )
  {
    std::map<Integer, Integer>::iterator i_next = i;
    ++i_next;
    if (!right.is_ge(i->first))
       contents.erase(i);
    i = i_next;
  }
}
  
// Return success/failure
bool BiVector::get(Integer i, Integer *p_v) const
{
  if (!left.is_le(i) || !right.is_ge(i))
     return false;

  std::map<Integer, Integer>::const_iterator p = contents.find(i);
  if (p == contents.end())
  {
    if (p_v)
       *p_v = 0;
  } else
  {
    if (p_v)
       *p_v = p->second;
  }
  return true;
}

// Return success/failure
bool BiVector::set(Integer i, const Integer &v)
{ 
  if (!left.is_le(i) || !right.is_ge(i))
     return false;

  if (v == 0)
     contents.erase(i);
  else
  {
    std::pair<std::map<Integer, Integer>::iterator, bool> p = 
            contents.insert(std::pair<Integer, Integer>(i, v));

    if (!p.second)
       p.first->second = v;
  }
  return true;
}

// Reset left bound (must be to <= right bound.)
void BiVector::set_left(const Bound &e)
{
  assert(e.is_infinite() || right.is_infinite() || 
         e.value() <= right.value());
  if (!e.is_infinite() && (left.is_infinite() || e.value() > left.value()))
  {
    left = e;
    trim_bivector_at_left();
  } else
    left = e;
}

// Reset right bound (must be to >= left bound.)
void BiVector::set_right(const Bound &e)
{
  assert(left.is_infinite() || e.is_infinite() || 
         left.value() <= e.value());
  if (!e.is_infinite() && (right.is_infinite() || e.value() < right.value()))
  {
    right = e;
    trim_bivector_at_right();
  } else
    right = e;
}

// Trim all values in vector to be between LOW and HIGH, inclusive.
// We should have LOW <= 0, HIGH >= 0.
void BiVector::trim_values(const Bound &low, const Bound &high)
{
  assert(low.is_le(0) && high.is_ge(0));

  for (std::map<Integer, Integer>::iterator i = contents.begin(); 
       i != contents.end(); i++)
  {
    if (!low.is_infinite() && i->second < low.value()) 
       i->second = low.value();
    if (!high.is_infinite() && i->second > high.value())
       i->second = high.value();
  }
}

// Return succ/failure
bool BF::set_left_end(const End &e) 
{ 
  if (!e.is_infinite() && !tape.get_right().is_infinite() && 
      e.value() > tape.get_right().value())
     return false;
  if ((e.is_infinite() && type_right == ET_wraparound) || 
      (e.type() == ET_wraparound && tape.get_right().is_infinite()))
     return false;

  tape.set_left(e.bound());
  type_left = e.type();
  position = trim_to(position, tape.get_left(), tape.get_right());
  return true;
}

// Return succ/failure
bool BF::set_right_end(const End &e) 
{ 
  if (!e.is_infinite() && !tape.get_left().is_infinite() && 
      tape.get_left().value() > e.value())
     return false;
  if ((e.is_infinite() && type_left == ET_wraparound) || 
      (e.type() == ET_wraparound && tape.get_left().is_infinite()))
     return false;

  tape.set_right(e.bound());
  type_right = e.type();
  position = trim_to(position, tape.get_left(), tape.get_right());
  return true;
}

// Return succ/failure
bool BF::set_low_end(const End &e) 
{
  if (!e.is_le(0))
     return false;
  if ((e.is_infinite() && high.type() == ET_wraparound) ||
      (e.type() == ET_wraparound && high.is_infinite()))
     return false;
  low = e;

  tape.trim_values(low.bound(), high.bound());
  eof_value = trim_to(eof_value, low.bound(), high.bound());
  return true;
}

// Return succ/failure
bool BF::set_high_end(const End &e) 
{ 
  if (!e.is_ge(0))
     return false;
  if ((e.is_infinite() && low.type() == ET_wraparound) ||
      (e.type() == ET_wraparound && low.is_infinite()))
     return false;
  high = e;

  tape.trim_values(low.bound(), high.bound());
  eof_value = trim_to(eof_value, low.bound(), high.bound());
  return true;
}

// Returns succ/failure
bool BF::set_code(const std::string &s) 
{ 
  size_t bracket_count = 0;

  for (std::string::const_iterator i = s.begin(); i != s.end(); i++)
  {
    if (*i == '[')
       bracket_count++;
    else if (*i == ']')
    {    
      if (bracket_count-- == 0)
         return false;
    }
  }
  if (bracket_count != 0)
     return false;

  code = s;
  breakpoints.clear();
  num_breakpoints_at.clear();
  num_breakpoints_at.insert(num_breakpoints_at.begin(), code.size(), 0u);
  pc = 0;
  return true;
}

// Returns succ/failure 
bool BF::set_pc(const Integer &i) 
{
  int ii;

  if (i < 0 || i >= std::numeric_limits<int>::max() )
     return false;
  ii = (int)i;
  if (ii > (int)code.length())
     return false;
  pc = ii;
  return true;
}

// Returns succ/failure 
bool BF::set_position(const Integer &i) 
{
  if (!tape.get_left().is_le(i) || !tape.get_right().is_ge(i))
     return false;
  position = i;
  return true;
}

int BF::get_breakpoint(const Integer &a) const
{
  int ia;

  if (a < 0 || a > std::numeric_limits<int>::max())
     return -1;
  ia = (int)a;
  if (ia >= breakpoints.size())
     return -1;
  return breakpoints[ia];
}

int BF::add_breakpoint(const Integer &a)
{ 
  int ia;

  if (a < 0 || a > std::numeric_limits<int>::max())
     return -1;
  ia = (int)a;
  if (ia >= code.size())
     return -1;
  breakpoints.push_back(ia);
  num_breakpoints_at[ia]++;
  return breakpoints.size() - 1;
}

bool BF::delete_breakpoint(const Integer &b) 
{ 
  int ib, place;

  if (b < 0 || b >= std::numeric_limits<int>::max())
     return false;
  ib = (int)b;
  if (ib >= breakpoints.size())
     return false;
  place = breakpoints[ib];
  if (place == -2)
     return true;

  assert(place >= 0 && place < num_breakpoints_at.size() &&
         num_breakpoints_at[place] > 0u);
  num_breakpoints_at[place]--;
  breakpoints[ib] = -2;
  return true;
}

// 
// Try to remap a tape position to the specified range.  Return FALSE if we
// must abort, TRUE o.w.
// 
//
// In the event of one bound being wraparound and the other being undefined,
// tape operations will have been combined and optimization will have been
// done, so tape references may come either from the future or the 
// past of the BF program.  This fact means that we must treat the 
// tape as wraparound in both directions.
// 
bool remap_tape_difficult(Integer *p_i, EndType e_lo, const Bound &lo, 
                                        EndType e_hi, const Bound &hi)
{
  if (!lo.is_le(*p_i))
  {
    switch 
       ((e_hi == ET_wraparound && e_lo == ET_undefined) ? ET_wraparound : e_lo)
    {
      case ET_abort:
      case ET_truncate:
      case ET_undefined:
           assert(!lo.is_infinite());
           *p_i = lo.value();
           if (e_lo == ET_abort || e_lo == ET_undefined)
               return false;
           break;

      case ET_wraparound:
           assert(!lo.is_infinite() && !hi.is_infinite());
           *p_i = (*p_i - lo.value()) % (hi.value() - lo.value() + 1) 
                  + lo.value();
           break;
    }
  }
  else if (!hi.is_ge(*p_i))
  {
    switch 
       ((e_lo == ET_wraparound && e_hi == ET_undefined) ? ET_wraparound : e_hi)
    {
      case ET_abort:
      case ET_truncate:
      case ET_undefined:
           assert(!hi.is_infinite());
           *p_i = hi.value();
           if (e_hi == ET_abort || e_hi == ET_undefined)
               return false;
           break;

      case ET_wraparound:
           assert(!lo.is_infinite() && !hi.is_infinite());
           *p_i = (*p_i - lo.value()) % (hi.value() - lo.value() + 1) 
                  + lo.value();
           break;
    }
  } else
    // Never called in this case
    assert(0);

  return true;
}
