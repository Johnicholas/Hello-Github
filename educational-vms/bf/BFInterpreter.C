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

#include <algorithm>
#include <limits>
#include <cassert>
#include <cstddef>
#include <iostream>
#include <vector>

#include "BF.h"
#include "GenericError.h"
#include "Integer.h"
#include "NumericLimits.h"

#include "BFImpl.h"

struct InterpreterTapeError : public GenericError
{
  virtual const char *what() const throw() 
  { return "Excessively large tape size needed"; }
};

// A chunk is a series of numbers located at an offset.
// Intersect chunk B with chunk A.
template <typename IntegralType>
void intersect_chunk
         (const IntegralType **p_p_b, int *p_len_b, Integer *p_ofs_b,
          const IntegralType *p_a, int len_a, const Integer &ofs_a)
{
  // Clip with left end
  if (ofs_a > *p_ofs_b)
  {
    if (ofs_a - *p_ofs_b >= *p_len_b)
    {
      *p_len_b = 0;
      return;
    }
    *p_p_b += ofs_a - *p_ofs_b;
    *p_len_b -= (int)(ofs_a - *p_ofs_b);
    *p_ofs_b = ofs_a;
  }
  // Clip with right end
  if (ofs_a + len_a < *p_ofs_b + *p_len_b)
  {
    if (ofs_a + len_a <= *p_ofs_b)
    {
      *p_len_b = 0;
      return;
    }
    *p_len_b = ofs_a + len_a - *p_ofs_b;
  }
}

// Copy in the intersections of chunk A and chunk B and chunk A and chunk
// C to chunk A.  Fill in the rest from the tape.
template <typename IntegralType>
void copy_in_chunk(const BiVector &tape,
                   IntegralType *p_a, int len_a, Integer ofs_a,
                   const IntegralType *p_b, int len_b, Integer ofs_b,
                   const IntegralType *p_c, int len_c, Integer ofs_c)
{
  Integer temp;

  intersect_chunk(&p_b, &len_b, &ofs_b, p_a, len_a, ofs_a);
  intersect_chunk(&p_c, &len_c, &ofs_c, p_a, len_a, ofs_a);
  if (len_b == 0 && len_c == 0)
  {
    for (int i = 0; i < len_a; i++)
    {
      assert(tape.get(ofs_a + i, &temp));
      p_a[i] = (IntegralType)temp;
    }
  } else if (len_b == 0)
  {
    int oc = ofs_c - ofs_a;
    assert(oc >= 0 && oc + len_c <= len_a);
    for (int i = 0; i < oc; i++)
    {
      assert(tape.get(ofs_a + i, &temp));
      p_a[i] = (IntegralType)temp;
    }
    for (int i = 0; i < len_c; i++)
        p_a[i + oc] = p_c[i];
    for (int i = oc + len_c; i < len_a; i++)
    {
      assert(tape.get(ofs_a + i, &temp));
      p_a[i] = (IntegralType)temp;
    }
  } else if (len_c == 0)
  {
    int ob = ofs_b - ofs_a;
    assert(ob >= 0 && ob + len_b <= len_a);
    for (int i = 0; i < ob; i++)
    {
      assert(tape.get(ofs_a + i, &temp));
      p_a[i] = (IntegralType)temp;
    }
    for (int i = 0; i < len_b; i++)
        p_a[i + ob] = p_b[i]; 
    for (int i = ob + len_b; i < len_a; i++)
    {
      assert(tape.get(ofs_a + i, &temp));
      p_a[i] = (IntegralType)temp;
    }
  } else
  {
    int ob = ofs_b - ofs_a;
    int oc = ofs_c - ofs_a;
    int mn = (ob < oc) ? ob : oc;
    int mx = (ob > oc) ? ob + len_b : oc + len_c;
    int md0 = (ob < oc) ? ob + len_b : oc + len_c;
    int md1 = (ob > oc) ? oc : ob;
    assert(mn >= 0 && mn < md0 && md0 <= md1 && md1 < mx && mx <= len_a);
    for (int i = 0; i < mn; i++)
    {
      assert(tape.get(ofs_a + i, &temp));
      p_a[i] = (IntegralType)temp;
    }
    for (int i = md0; i < md1; i++)
    {
      assert(tape.get(ofs_a + i, &temp));
      p_a[i] = (IntegralType)temp;
    }
    for (int i = mx; i < len_a; i++)
    {
      assert(tape.get(ofs_a + i, &temp));
      p_a[i] = (IntegralType)temp;
    }

    for (int i = 0; i < len_b; i++)
        p_a[i + ob] = p_b[i]; 
    for (int i = 0; i < len_c; i++)
        p_a[i + oc] = p_c[i]; 
  }
}

// Take the portion of chunk A that is not contained in either chunk B 
// or chunk C, and dump it to the tape.
template <typename IntegralType>
void copy_out_chunk(BiVector *p_tape,
                    IntegralType *p_a, int len_a, Integer ofs_a,
                    const IntegralType *p_b, int len_b, Integer ofs_b,
                    const IntegralType *p_c, int len_c, Integer ofs_c)
{
  intersect_chunk(&p_b, &len_b, &ofs_b, p_a, len_a, ofs_a);
  intersect_chunk(&p_c, &len_c, &ofs_c, p_a, len_a, ofs_a);
  if (len_b == 0 && len_c == 0)
  {
    for (int i = 0; i < len_a; i++)
        p_tape->set(i + ofs_a, p_a[i]);
  } else if (len_b == 0)
  {
    int oc = ofs_c - ofs_a;
    assert(oc >= 0 && oc + len_c <= len_a);
    for (int i = 0; i < oc; i++)
        assert(p_tape->set(i + ofs_a, p_a[i]));
    for (int i = oc + len_c; i < len_a; i++)
        assert(p_tape->set(i + ofs_a, p_a[i]));
  } else if (len_c == 0)
  {
    int ob = ofs_b - ofs_a;
    assert(ob >= 0 && ob + len_b <= len_a);
    for (int i = 0; i < ob; i++)
        assert(p_tape->set(i + ofs_a, p_a[i]));
    for (int i = ob + len_b; i < len_a; i++)
        assert(p_tape->set(i + ofs_a, p_a[i]));
  } else
  {
    int ob = ofs_b - ofs_a;
    int oc = ofs_c - ofs_a;
    int mn = (ob < oc) ? ob : oc;
    int mx = (ob > oc) ? ob + len_b : oc + len_c;
    int md0 = (ob < oc) ? ob + len_b : oc + len_c;
    int md1 = (ob > oc) ? oc : ob;
    assert(mn >= 0 && mn < md0 && md0 <= md1 && md1 < mx && mx <= len_a);
    for (int i = 0; i < mn; i++)
        assert(p_tape->set(i + ofs_a, p_a[i]));
    for (int i = md0; i < md1; i++)
        assert(p_tape->set(i + ofs_a, p_a[i]));
    for (int i = mx; i < len_a; i++)
        assert(p_tape->set(i + ofs_a, p_a[i]));
  }
}

// Output a form of l suitable for feeding to <<.
inline Integer printable_form(char l) { return (Integer)l; }
inline Integer printable_form(signed char l) { return (Integer)l; }
inline Integer printable_form(unsigned char l) { return (Integer)l; }
inline short printable_form(short l) { return l; }
inline unsigned short printable_form(unsigned short l) { return l; }
inline int printable_form(int l) { return l; }
inline unsigned int printable_form(unsigned int l) { return l; }
inline long printable_form(long l) { return l; }
inline unsigned long printable_form(unsigned long l) { return l; }
inline const Integer &printable_form(const Integer &l) { return l; }

// Get the low 8 bits of l.
inline unsigned char get_low_bits(char l) { return (unsigned char)l; }
inline unsigned char get_low_bits(signed char l) { return (unsigned char)l; }
inline unsigned char get_low_bits(unsigned char l) { return l; }
inline unsigned char get_low_bits(short l) { return (unsigned short)l; }
inline unsigned char get_low_bits(unsigned short l) { return l; }
inline unsigned char get_low_bits(int l) { return (unsigned int)l; }
inline unsigned char get_low_bits(unsigned int l) { return l; }
inline unsigned char get_low_bits(long l) { return (unsigned long)l; }
inline unsigned char get_low_bits(unsigned long l) { return l; }

// NB: (unsigned long)v.value_modulo_long() s.b. 
// congruent to v modulo 2^(8 sizeof(unsigned long)).
inline unsigned char get_low_bits(const Integer &a) 
{ return get_low_bits(a.value_modulo_long()); }

inline char remap_cell(char c, const End &, const End &)
{ return c; }
inline signed char remap_cell(signed char c, const End &, const End &)
{ return c; }
inline unsigned char remap_cell(unsigned char c, const End &, const End &)
{ return c; }
inline short remap_cell(short c, const End &, const End &)
{ return c; }
inline unsigned short remap_cell(unsigned short c, const End &, const End &)
{ return c; }
inline int remap_cell(int c, const End &, const End &)
{ return c; }
inline unsigned int remap_cell(unsigned int c, const End &, const End &)
{ return c; }
inline long remap_cell(long c, const End &, const End &)
{ return c; }
inline unsigned long remap_cell(unsigned long c, const End &, const End &)
{ return c; }

#define TAPE_CHUNK_SIZE 100000

template<typename IntegralType>
class Interpreter
{
  // Pointers into the machine
  BiVector *p_the_tape;
  Integer *p_the_position;
  
  // Our local state
  EndType the_type_left;
  EndType the_type_right;
  IntegralType *the_chunk;
  int the_chunk_bound;
  
  // In the case where both ends of the tape are wraparound, it is possible 
  // for the chunk to have a piece at the right end of the tape 
  // ([0...the_chunk_bound_1 - 1], offset by the_chunk_offset_1) followed 
  // by a piece at the left end of the tape 
  // ([the_chunk_bound_1...the_chunk_bound], offset by the_chunk_offset_2.)
  Integer the_chunk_offset_1;
  int the_chunk_bound_1;
  Integer the_chunk_offset_2;

  // Remap the chunk.
  void remap_chunk(int bound, const Integer &ofs_1, 
                   int bound_1, const Integer &ofs_2);

  // Determine where to remap the chunk to cover the indicated positions,
  // and then call remap_chunk() to move it.
  void move_chunk(int num_posns, const Integer *posns);

  Integer chunk_pos_to_pos(int i)
  {
    if (i < the_chunk_bound_1 || the_chunk_bound_1 > the_chunk_bound)
       return i + the_chunk_offset_1;
    else
       return i + the_chunk_offset_2;
  }
 
  int pos_to_chunk_pos(const Integer &i)
  {
    if (i >= the_chunk_offset_1 && i < the_chunk_offset_1 + the_chunk_bound_1)
       return i - the_chunk_offset_1;
    else if (i >= the_chunk_offset_2 + the_chunk_bound_1 &&
             i <= the_chunk_offset_2 + the_chunk_bound)
       return i - the_chunk_offset_2;
    else
       assert(0);
  }

  // Set up chunk.
  void initialize_chunk(BiVector *p_tape, Integer *p_position, 
                        EndType type_left, EndType type_right, 
                        int *p_chunk_pos);

  void fix_access(int *p_chunk_pos, int len, int *p_chunk_locns);

  bool fix_move(int *p_chunk_pos);

  void flush_chunk(int final_chunk_pos);
 
public:

  // Preliminary check to see if we can run this machine on this interpreter.
  static bool is_runnable(const End &low, const End &high);

  // Run machine for given # of instructions (forever if # < 0)
  RunTermination run_machine(BF &machine, Integer a);
};

// Remap the chunk.
template<typename IntegralType>
void Interpreter<IntegralType>::remap_chunk
(int bound, const Integer &ofs_1, int bound_1, const Integer &ofs_2)
{
  IntegralType *new_chunk = new IntegralType[bound + 1];

  // Copy in data from the new chunk that was in the old chunk.
  // Copy in the remaining data from the tape.
  copy_in_chunk(*p_the_tape, 
                  new_chunk, bound_1, ofs_1, 
                the_chunk, the_chunk_bound_1, the_chunk_offset_1,
                the_chunk + the_chunk_bound_1, 
                  the_chunk_bound + 1 - the_chunk_bound_1, 
                  the_chunk_offset_2 + the_chunk_bound_1);
  copy_in_chunk(*p_the_tape,
                  new_chunk + bound_1, bound + 1 - bound_1, ofs_2 + bound_1,
                the_chunk, the_chunk_bound_1, the_chunk_offset_1,
                the_chunk + the_chunk_bound_1, 
                  the_chunk_bound + 1 - the_chunk_bound_1, 
                  the_chunk_offset_2 + the_chunk_bound_1);
  
  // Take the data in the old chunk that was not copied to the new chunk,
  // and write it to the tape.
  copy_out_chunk(p_the_tape,
                   the_chunk, the_chunk_bound_1, the_chunk_offset_1,
                 new_chunk, bound_1, ofs_1, 
                 new_chunk + bound_1, bound + 1 - bound_1, ofs_2 + bound_1);
  copy_out_chunk(p_the_tape,
                   the_chunk + the_chunk_bound_1, 
                   the_chunk_bound + 1 - the_chunk_bound_1, 
                   the_chunk_offset_2 + the_chunk_bound_1,
                 new_chunk, bound_1, ofs_1, 
                 new_chunk + bound_1, bound + 1 - bound_1, ofs_2 + bound_1);

  delete[] the_chunk;
  the_chunk = new_chunk;
  the_chunk_bound = bound;
  the_chunk_offset_1 = ofs_1;
  the_chunk_bound_1 = bound_1;
  the_chunk_offset_2 = ofs_2;
}
  

// Determine where to remap the chunk to cover the indicated positions,
// and then call remap_chunk() to move it.
template<typename IntegralType>
void Interpreter<IntegralType>::move_chunk
   (int num_posns, const Integer *posns)
{
  // If the tape is short, we should just map it all.
  if (!p_the_tape->get_left().is_infinite() &&
      !p_the_tape->get_right().is_infinite() &&
      p_the_tape->get_right().value() - p_the_tape->get_left().value()
          <= TAPE_CHUNK_SIZE)
  {
    if (the_chunk != NULL)
      return;

    int new_bound = p_the_tape->get_right().value() -
                    p_the_tape->get_left().value();
    remap_chunk(new_bound, p_the_tape->get_left().value(),
                new_bound + 1, 0);
    return;
  }

  assert(num_posns >= 1);
  Integer l, r, diameter;

  if (num_posns == 1)
  {
    l = r = posns[0];
    diameter = 0;
  }
  else if ((the_type_left != ET_wraparound && the_type_left != ET_undefined) || 
           (the_type_right != ET_wraparound && the_type_right != ET_undefined))
  {
    // Non-wraparound case.
    l = r = posns[0];
    for (int i = 1; i < num_posns; i++)
    {
      if (l > posns[i])
         l = posns[i];
      if (r < posns[i])
         r = posns[i];
    }
    diameter = r - l;
  } else
  {
    // Wraparound case.  Find smallest possible chunk.
    std::vector<Integer> ps;
    for (int i = 0; i < num_posns; i++)
        ps.push_back(posns[i]);
    sort(ps.begin(), ps.end());
    l = ps[0];
    r = ps[num_posns - 1];
    diameter = r - l;
    Integer tape_length = p_the_tape->get_right().value() 
                        - p_the_tape->get_left().value() + 1;
    for (int i = 1; i < num_posns; i++)
    {
      if (diameter > ps[i - 1] + tape_length - ps[i])
      {
        diameter = ps[i - 1] + tape_length - ps[i];
        l = ps[i];
        r = ps[i-1];
      }
    }
  }
  if (diameter > (int)(0.9 * std::numeric_limits<int>::max()))
      throw InterpreterTapeError();

  if (diameter < TAPE_CHUNK_SIZE)
  {
    Integer expand = (TAPE_CHUNK_SIZE - diameter) / 2;
    if (!p_the_tape->get_left().is_infinite() &&
        l - p_the_tape->get_left().value() < expand)
       expand = l - p_the_tape->get_left().value();
    if (!p_the_tape->get_right().is_infinite() &&
        p_the_tape->get_right().value() - r < expand)
       expand = p_the_tape->get_right().value() - r;
    l -= expand;
    r += expand;
    diameter += 2 * expand;
  }

  if (l < r)
     remap_chunk(diameter, l, diameter + 1, 0);
  else
     remap_chunk(diameter, l, p_the_tape->get_right().value() + 1 - l, 
                     l + p_the_tape->get_left().value()
                       - p_the_tape->get_right().value() - 1);
}

template<typename IntegralType>
void Interpreter<IntegralType>::initialize_chunk
         (BiVector *p_tape, Integer *p_position, 
          EndType type_left, EndType type_right, int *p_chunk_pos)
{
  p_the_tape = p_tape;
  p_the_position = p_position;
  the_type_left = type_left;
  the_type_right = type_right;

  the_chunk = NULL;
  the_chunk_bound = -1;
  the_chunk_bound_1 = 0;
  the_chunk_offset_1 = the_chunk_offset_2 = 0;
  move_chunk(1, p_position);

  *p_chunk_pos = pos_to_chunk_pos(*p_position);
}

template<typename IntegralType>
void Interpreter<IntegralType>::fix_access
         (int *p_chunk_pos, int len, int *p_chunk_locns)
{
  Integer q[MAX_INSTR_LENGTH+2];

  assert(p_chunk_pos != NULL && len >= 0 && len <= MAX_INSTR_LENGTH + 1);

  q[0] = chunk_pos_to_pos(*p_chunk_pos);
  assert(remap_tape(&q[0], the_type_left, p_the_tape->get_left(), 
                           the_type_right, p_the_tape->get_right()));

  for (int j = 0; j < len; j++)
  {
    q[j+1] = chunk_pos_to_pos(p_chunk_locns[j]);
    assert(remap_tape(&q[j+1], the_type_left, p_the_tape->get_left(), 
                             the_type_right, p_the_tape->get_right()));
  }

  move_chunk(len + 1, q);
 
  *p_chunk_pos = pos_to_chunk_pos(q[0]);
  for (int j = 0; j < len; j++)
      p_chunk_locns[j] = pos_to_chunk_pos(q[j+1]);
}

template<typename IntegralType>
bool Interpreter<IntegralType>::fix_move(int *p_chunk_pos)
{
  if (!p_chunk_pos)
     return true;

  Integer p = chunk_pos_to_pos(*p_chunk_pos);
  bool rv = remap_tape(&p, the_type_left, p_the_tape->get_left(), 
                           the_type_right, p_the_tape->get_right());

  move_chunk(1, &p);

  *p_chunk_pos = pos_to_chunk_pos(p);
  return rv;
}

template<typename IntegralType>
void Interpreter<IntegralType>::flush_chunk(int final_chunk_pos)
{
  for (int i = 0; i < the_chunk_bound_1; i++)
      assert(p_the_tape->set(i + the_chunk_offset_1, the_chunk[i]));
  for (int i = the_chunk_bound_1; i <= the_chunk_bound; i++)
      assert(p_the_tape->set(i + the_chunk_offset_2, the_chunk[i]));
  delete[] the_chunk;
  the_chunk = NULL;
  *p_the_position = chunk_pos_to_pos(final_chunk_pos);
  assert(p_the_tape->get_left().is_le(*p_the_position) &&
         p_the_tape->get_right().is_ge(*p_the_position));
  p_the_tape = NULL;
  p_the_position = NULL;
}
 
// Preliminary check to see if we can run this machine on this interpreter.
template<typename IntegralType>
bool Interpreter<IntegralType>::is_runnable(const End &low, const End &high)
{
  if (NumericLimits<IntegralType>::is_unbounded())
     return true;

  if ((NumericLimits<IntegralType>::is_unsigned_binary() ||
       NumericLimits<IntegralType>::is_signed_binary()) &&
      !low.is_infinite() && !high.is_infinite() &&
      low.value() == NumericLimits<IntegralType>::min() &&
      high.value() == NumericLimits<IntegralType>::max())
     return true;

  return false;
}

// Run machine for given # of instructions (forever if # < 0)
template<typename IntegralType>
RunTermination Interpreter<IntegralType>::run_machine
                        (BF &machine, Integer a)
{
  typename std::vector<GeneralInstruction<IntegralType> > insns;
  typename std::vector<GeneralInstruction<IntegralType> >::const_iterator ip;
  int a_int = 0, _position, p, pp;

  if (!make_instruction_sequence(false, a < 0, machine, &insns, &ip))
     return RT_Program_Invalid;

  initialize_chunk(&machine.tape, &machine.position, 
                   machine.type_left, machine.type_right, &_position);

#define GET_P \
if ((p = _position + ip->cell()) < 0 || p > the_chunk_bound) \
   fix_access(&_position, 1, &p);

  if (a >= 0)
  {
    a_int = (a <= std::numeric_limits<int>::max()) ? 
                               (int)a : std::numeric_limits<int>::max();
    a -= a_int;
  } 

  for (int i = 0;; ++ip)
  {
    switch (ip->type())
    {
      case IT_Clock:
               if (++i == a_int)
               {
                 if (a == 0)
                 {
                   machine.pc = ip->location();
                   flush_chunk(_position);
                   return RT_Chunk_Done;
                 }
                 a_int = (a <= std::numeric_limits<int>::max()) ? 
                               (int)a : std::numeric_limits<int>::max();
                 a -= a_int;
                 i = 0;
               }
               break;

      case IT_Start:
               break;

      case IT_Breakpoint:
               machine.pc = ip->location();
               flush_chunk(_position);
               return RT_Breakpoint;

      case IT_Halt:
               machine.pc = ip->location();
               flush_chunk(_position);
               return RT_Normal;
               break;

      case IT_Input:
               {
                 bool at_eof;
                 int ichar;
                 Integer w;

                 switch (machine.get_input_method())
                 {
                   case IM_decimal:
                        at_eof = !(std::cin >> w);
                        break;

                   case IM_signed:
                   case IM_unsigned:
                        at_eof = 
                          ((ichar = std::cin.get()) == 
                           std::char_traits<char>::eof());
                        if (machine.get_input_method() == IM_signed && 
                            ichar >= 128)
                           w = ichar - 256;
                        else
                           w = ichar;
                        break;

                   default:
                        assert(0);
                        at_eof = false;
                        break;
                 }
                 if (at_eof)
                 {
                   switch (machine.get_eof_method())
                   {
                     case EOF_halt:
                          machine.pc = ip->location();
                          GET_P;
                          flush_chunk(p);
                          return RT_Normal;

                     case EOF_abort:
                          machine.pc = ip->location();
                          GET_P;
                          flush_chunk(p);
                          return RT_EOF_Error;

                     case EOF_value:
                          GET_P;
                          the_chunk[p] = machine.get_eof_value();
                          break;

                     case EOF_unchanged:
                          break;
                   }
                 } else
                 {
                   GET_P;
                   the_chunk[p] = trim_to(w, machine.low.bound(), 
                                             machine.high.bound());
                 }
               }
               break;

      case IT_Output:
               GET_P;  
               switch (machine.get_output_method())
               {
                 case OM_decimal:
                      std::cout << printable_form(the_chunk[p]) << '\n';
                      break;

                 case OM_char:
                      std::cout << get_low_bits(the_chunk[p]);
                      break;
               }
               break;

      case IT_While:
               GET_P;
               if (the_chunk[p] == 0)
                  ip = ip->jump_ptr();
               break;

      case IT_EndWhile:
               GET_P;
               if (the_chunk[p] != 0)
                  ip = ip->jump_ptr();
               break;

      case IT_If:
               GET_P;
               if (the_chunk[p] == 0)
                  ip = ip->jump_ptr();
               break;

      case IT_EndIf:
               break;

      case IT_AddPos:
               if ((_position += ip->cell()) < 0 || 
                    _position > the_chunk_bound) 
               { 
                 if (!fix_move(&_position))
                 {
                   machine.pc = ip->location();
                   flush_chunk(_position);
                   return RT_Bound_Error;
                 }
               }
               break;

      case IT_CheckEQ:
               GET_P;
               if (the_chunk[p] != ip->value())
               {
                 flush_chunk(p);
                 machine.pc = ip->location();
                 return ip->is_abort() ? RT_Bound_Error : RT_Infinite_Loop;
               }
               break;

      case IT_CheckLE:
               GET_P;
               if (the_chunk[p] > ip->value())
               {
                 flush_chunk(p);
                 machine.pc = ip->location();
                 return ip->is_abort() ? RT_Bound_Error : RT_Infinite_Loop;
               }
               break;

      case IT_CheckGE:
               GET_P;
               if (the_chunk[p] < ip->value())
               {
                 flush_chunk(p);
                 machine.pc = ip->location();
                 return ip->is_abort() ? RT_Bound_Error : RT_Infinite_Loop;
               }
               break;

      case IT_CheckDiv:
               GET_P;
               assert(ip->value() > 0);
               if (the_chunk[p] % ip->value() != 0)
               {
                 flush_chunk(p);
                 machine.pc = ip->location();
                 return ip->is_abort() ? RT_Bound_Error : RT_Infinite_Loop;
               }
               break;

      case IT_CheckAnd:
               assert(NumericLimits<IntegralType>::is_signed_binary());
               GET_P;
               if ((the_chunk[p] & ip->value()) != 0)
               {
                 flush_chunk(p);
                 machine.pc = ip->location();
                 return ip->is_abort() ? RT_Bound_Error : RT_Infinite_Loop;
               }
               break;

      case IT_Abort:
               GET_P;
               flush_chunk(p);
               machine.pc = ip->location();
               return ip->is_abort() ? RT_Bound_Error : RT_Infinite_Loop;

      case IT_Max:
               GET_P;
               if (the_chunk[p] < ip->value())
                  the_chunk[p] = ip->value();
               break;

      case IT_Min:
               GET_P;
               if (the_chunk[p] > ip->value())
                  the_chunk[p] = ip->value();
               break;

      case IT_Sub:
               GET_P;
               the_chunk[p] = remap_cell(ip->value() - the_chunk[p], 
                                          machine.low, machine.high);
               break;

      case IT_Quotient:
               GET_P;
               assert(ip->value() > 0);
               // Round away from 0
               // We are guaranteed that (a/b)*b + (a%b) == a
               if (the_chunk[p] == 0)
                   ;
               else if (the_chunk[p] > 0)
               {
                 IntegralType remainder = the_chunk[p] % ip->value();
                 the_chunk[p] /= ip->value();
                 if (remainder > 0)
                 {
                   assert(remainder < ip->value());
                   the_chunk[p]++;
                 } else
                   assert(ip->value() > -remainder);
               } else
               {
                 assert(the_chunk[p] < 0);
                 IntegralType remainder = the_chunk[p] % ip->value();
                 the_chunk[p] /= ip->value();
                 if (remainder < 0)
                 {
                   assert(ip->value() > -remainder);
                   the_chunk[p]--;
                 } else
                   assert(remainder < ip->value());
               }
               break;

      case IT_ExactQuotient:
               GET_P;
               assert(ip->value() > 0);
               assert(the_chunk[p] % ip->value() == 0);
               the_chunk[p] /= ip->value();
               break;

      case IT_Remainder:
               GET_P;
               assert(ip->value() > 0);
               {
                 IntegralType remainder = the_chunk[p] % ip->value();
                 if (remainder < 0)
                 {
                   assert(-remainder < ip->value());
                   remainder += ip->value();
                 } else
                   assert(remainder < ip->value());
                 the_chunk[p] = 
                         remap_cell(remainder, machine.low, machine.high);
               }
               break;

      case IT_And:
               assert(NumericLimits<IntegralType>::is_signed_binary());
               GET_P;
               the_chunk[p] = remap_cell(the_chunk[p] & ip->value(), 
                                          machine.low, machine.high);
               break;

      case IT_Set0:
               GET_P;
               the_chunk[p] = ip->value();
               break;

      case IT_Add0:
               GET_P;
               the_chunk[p] = remap_cell(ip->value() + the_chunk[p],
                                         machine.low, machine.high);
               break;

      case IT_Inc:
               GET_P;
               the_chunk[p] = remap_cell(++the_chunk[p],
                                         machine.low, machine.high);
               break;

      case IT_Dec:
               GET_P;
               the_chunk[p] = remap_cell(--the_chunk[p],
                                         machine.low, machine.high);
               break;

      case IT_Set1v1:
               p = _position + ip->cell();
               pp = _position + ip->cells0();
               if (p < 0 || p > the_chunk_bound ||
                   pp < 0 || pp > the_chunk_bound)
               {
                 int q[2];
                 q[0] = p;
                 q[1] = pp;
                 fix_access(&_position, 2, q);
                 p = q[0];
                 pp = q[1];
               }
               the_chunk[p] = the_chunk[pp];
               break;

      case IT_Add1v1:
               p = _position + ip->cell();
               pp = _position + ip->cells0();
               if (p < 0 || p > the_chunk_bound ||
                   pp < 0 || pp > the_chunk_bound)
               {
                 int q[2];
                 q[0] = p;
                 q[1] = pp;
                 fix_access(&_position, 2, q);
                 p = q[0];
                 pp = q[1];
               }
               the_chunk[p] = 
                      remap_cell(the_chunk[p] + the_chunk[pp],
                          machine.low, machine.high);
               break;

      case IT_Set1:
               p = _position + ip->cell();
               pp = _position + ip->cells0();
               if (p < 0 || p > the_chunk_bound ||
                   pp < 0 || pp > the_chunk_bound)
               {
                 int q[2];
                 q[0] = p;
                 q[1] = pp;
                 fix_access(&_position, 2, q);
                 p = q[0];
                 pp = q[1];
               }
               the_chunk[p] = 
                     remap_cell(ip->value() * the_chunk[pp],
                          machine.low, machine.high);
               break;

      case IT_Add1:
               p = _position + ip->cell();
               pp = _position + ip->cells0();
               if (p < 0 || p > the_chunk_bound ||
                   pp < 0 || pp > the_chunk_bound)
               {
                 int q[2];
                 q[0] = p;
                 q[1] = pp;
                 fix_access(&_position, 2, q);
                 p = q[0];
                 pp = q[1];
               }
               the_chunk[p] = 
                  remap_cell(the_chunk[p] + ip->value() * the_chunk[pp],
                            machine.low, machine.high);
               break;

      case IT_Add:
      case IT_Set:
               {
                 int q[1+MAX_INSTR_LENGTH];
                 bool ok = ((q[0] = _position + ip->cell()) >= 0) && 
                            (q[0] <= the_chunk_bound);
                 for (int j = 0; j < ip->length(); j++)
                     if (((q[j+1] = _position + ip->cells(j)) < 0) ||
                         q[j+1] > the_chunk_bound)
                         ok = false;
                 if (!ok)
                    fix_access(&_position, ip->length() + 1, q);
                 IntegralType v = ip->value();
                 for (int j = 0; j < ip->length(); j++)
                     v = remap_cell(v * the_chunk[q[j+1]], 
                                  machine.low, machine.high);
                 if (ip->type() == IT_Add)
                    v = remap_cell(v + the_chunk[q[0]], 
                                 machine.low, machine.high);
                 the_chunk[q[0]] = v;
               }
               break;

      case IT_Permute:
               if (ip->length() > 1)
               {
                 int q[MAX_INSTR_LENGTH];
                 bool ok = true;
                 for (int j = 0; j < ip->length(); j++)
                     if (((q[j] = _position + ip->cells(j)) < 0) ||
                         q[j] > the_chunk_bound)
                         ok = false;
                 if (!ok)
                    fix_access(&_position, ip->length(), q);
                 IntegralType temp = the_chunk[q[0]];
                 for (int j = 0; j < ip->length() - 1; j++)
                   the_chunk[q[j]] = the_chunk[q[j+1]];
                 the_chunk[q[ip->length() - 1]] = temp;
               }
               break;
    }
  }
  /*NOTREACHED*/
}
#undef GET_P

RunTermination BF::run(const Integer &a)
{ 
  if (Interpreter<unsigned char>::is_runnable(get_low_end(), get_high_end()))
     return Interpreter<unsigned char>().run_machine(*this, a);

  if (Interpreter<signed char>::is_runnable(get_low_end(), get_high_end()))
     return Interpreter<signed char>().run_machine(*this, a);

  if (Interpreter<short>::is_runnable(get_low_end(), get_high_end()))
     return Interpreter<short>().run_machine(*this, a);

  if (Interpreter<unsigned short>::is_runnable(get_low_end(), get_high_end()))
     return Interpreter<unsigned short>().run_machine(*this, a);

  if (Interpreter<int>::is_runnable(get_low_end(), get_high_end()))
     return Interpreter<int>().run_machine(*this, a);

  if (Interpreter<unsigned int>::is_runnable(get_low_end(), get_high_end()))
     return Interpreter<unsigned int>().run_machine(*this, a);

  if (Interpreter<long>::is_runnable(get_low_end(), get_high_end()))
     return Interpreter<long>().run_machine(*this, a);

  if (Interpreter<unsigned long>::is_runnable(get_low_end(), get_high_end()))
     return Interpreter<unsigned long>().run_machine(*this, a);

  assert(Interpreter<Integer>::is_runnable(get_low_end(), get_high_end()));
  return Interpreter<Integer>().run_machine(*this, a);
}
