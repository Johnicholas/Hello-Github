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

#ifndef BF_IMPL

#define BF_IMPL

#include <cstddef>
#include <iosfwd>
#include <string>
#include <vector>

#include "Integer.h"
#include "BF.h"
#include "NumericLimits.h"

//
// We compile a BF program into a sequence of these instructions.
//

// IT_Clock: Record that one BF instruction has been executed (used to
//           execute for a specified number of BF instructions)
// IT_Start: start of execution for BF program
// IT_Breakpoint: breakpoint here
// IT_Halt: halt execution
// IT_While: while (T[p+cell] != 0)
// IT_EndWhile: endwhile (T[p+cell] != 0) 
// IT_If: if (T[p+cell] != 0)
// IT_EndIf: endif
// IT_Output: out << T[p+cell]
// IT_Input: in >> T[p+cell] (if abort or halt, p += cell before doing so)
// IT_AddPos: p += cell (may limit, wrap around, or abort, according to
//                       the specified tape bounds)
// IT_CheckGE: If T[p+cell] < val, p += cell and infinite loop or bound abort
//                           (val in [lo..hi])
// IT_CheckLE: If T[p+cell] > val, p += cell and infinite loop or bound abort
//                           (val in [lo..hi])
// IT_CheckEQ: If T[p+cell] != val, p += cell and infinite loop or bound abort
//                           (val in [lo..hi])
// IT_CheckDiv: If ~(val | T[p+cell]), p += cell and infinite loop or bound 
//                                         abort (val > 0)
// IT_CheckAnd: If (val & T[p+cell]) != 0, p += cell and infinite loop or bound 
//                abort (used in case where [lo..hi] is signed integer
//                       range; val in [lo..hi])
// IT_Abort: p += cell and infinite loop or bound abort
// IT_Max: T[p+cell] = max(T[p+cell], val)   
//                   (val in [lo..hi]; ans. is always in [lo..hi])
// IT_Min: T[p+cell] = min(T[p+cell], val)   
//                   (val in [lo..hi]; ans. is always in [lo..hi])
// IT_Sub: T[p+cell] = val - T[p+cell]
//                     (val in [lo...hi] if [lo...hi] finite)
// IT_Quotient: T[p+cell] /= val (val > 0; round away from 0; ans. in [lo..hi].)
// IT_ExactQuotient: Same as IT_Quotient, but we are guaranteed val | T[...].
// IT_Remainder: T[p+cell] %= val (val > 0;
//                                 only called if [lo..hi] finite;
//                                 val | hi - lo + 1 and val < hi - lo + 1
//                                 in this case; remainder is taken in 
//                                 [0...val-1), and possibly remapped 
//                                 from this into [-lo...hi]) 
// IT_And: T[p+cell] &= val (used in case where [lo..hi] is signed integer
//                           range; val in [lo..hi])
// IT_Add: T[p+cell] += val * T[p+cell_1] * ... * T[p+cell_m]
//                         (val in [lo...hi] if [lo...hi] finite)
// IT_Set: T[p+cell] = val * T[p+cell_1] * ... * T[p+cell_m]
//                         (val in [lo...hi] if [lo...hi] finite)
// IT_Add0: as IT_Add, m=0
// IT_Set0: as IT_Set, m=0
// IT_Add1v1: as IT_Add, m=1, val=1
// IT_Set1v1: as IT_Set, m=1, val=1
// IT_Add1: as IT_Add, m=1
// IT_Set1: as IT_Set, m=1
// IT_Permute: 
//    T[p+cell_1], ..., T[p+cell_m] := 
//           T[p+cell_2], ..., T[p+cell_(m-1)], T[p+cell_1]

// If [lo...hi] is not infinite, all arithmetic should be thought of 
// as being done modulo (hi - lo + 1) and reduced to the range [-lo...hi].
// Otherwise, all arithmetic is done as if on integers.

// NB: Unless on both sides the tape is either infinite, wraparound or
//     undefined, so that tape movements are freely combinable,
//     then except for IT_AddPos, we only allow instructions with cell == 0.

enum InstructionType 
{ 
  IT_Clock, IT_Start, IT_Breakpoint, IT_Halt, IT_Input, IT_Output, 
  IT_While, IT_EndWhile, IT_If, IT_EndIf, 
  IT_AddPos, IT_CheckEQ, IT_CheckLE, IT_CheckGE, IT_CheckDiv, IT_Abort,
  IT_Max, IT_Min, IT_Sub, IT_Quotient, IT_ExactQuotient,
  IT_Remainder, IT_Add, IT_Set,
  IT_Permute,

  // Introduced just before running
  IT_And, IT_CheckAnd, IT_Set0, IT_Add0, 
  IT_Set1, IT_Add1, IT_Set1v1, IT_Add1v1,
  IT_Inc, IT_Dec
};

#define MAX_INSTR_LENGTH 4    // For IT_Add, IT_Set, IT_Permute

template <typename IntegralType>
class GeneralInstruction
{
  InstructionType _type;
  bool _abort_type;
  unsigned char _length;
  int _location;
  int _cell;
  IntegralType _val;
  int _cells[MAX_INSTR_LENGTH];
  size_t _jump_to;
  typename std::vector<GeneralInstruction<IntegralType> >::const_iterator 
                                                   _jump_ptr;

public:

  template <class T> friend class GeneralInstruction;

  struct Abort { }; 

  GeneralInstruction(const GeneralInstruction<Integer> &i) :
          _type(i._type), _abort_type(i._abort_type), _length(i._length),
          _location(i._location), _cell(i._cell), _val(i._val)
          {
            assert(_length <= MAX_INSTR_LENGTH);
            for (int j = 0; j < _length; j++)
                _cells[j] = i._cells[j];
          }
          
  GeneralInstruction(InstructionType tt, int loc) 
             : _type(tt), _abort_type(false), _length(0), _location(loc),
               _cell(0), _val(0) { }

  GeneralInstruction(InstructionType tt, int loc, int cell) 
             : _type(tt), _abort_type(false), _length(0),
               _location(loc), _cell(cell), _val(0) { }

  GeneralInstruction(InstructionType tt, int loc, int cell, Abort) 
             : _type(tt), _abort_type(true), _length(0),
               _location(loc), _cell(cell), _val(0) { }

  GeneralInstruction(InstructionType tt, int loc, int cell, 
                     const IntegralType &val) 
             : _type(tt), _abort_type(false), _length(0),
               _location(loc), _cell(cell), _val(val) { }

  GeneralInstruction(InstructionType tt, int loc, int cell, 
                     const IntegralType &val, Abort) 
             : _type(tt), _abort_type(true), _length(0),
               _location(loc), _cell(cell), _val(val) { }

  GeneralInstruction(InstructionType tt, int loc, int cell, 
                     const IntegralType &val, int len, const int *cells) 
             : _type(tt), _abort_type(false), _length(len),
               _location(loc), _cell(cell), _val(val)
             { 
               assert(len >= 0 && len <= MAX_INSTR_LENGTH);
               for (int i = 0; i < len; i++) _cells[i] = cells[i]; 
             }

  InstructionType type() const { return _type; }
  int location() const { return _location; }
  int cell() const { return _cell; }
  bool is_abort() const { return _abort_type; }
  IntegralType value() const { return _val; }
  int length() const { return _length; }
  int cells0() const { return _cells[0]; }
  int cells(int j) const { assert(j >= 0 && j < _length); return _cells[j]; }

  void set_value(const IntegralType &j) { _val = j; }
  void set_cell(int j) { _cell = j; }
  void add_cell_mult(int j)
  { 
    assert(_length < MAX_INSTR_LENGTH);
    _cells[_length++] = j;
  } 

  size_t jump_to() const { return _jump_to; }
  typename std::vector<GeneralInstruction<IntegralType> >::const_iterator 
          jump_ptr() const { return _jump_ptr; }

  void set_jump_to(size_t j) { _jump_to = j; }
  void set_jump_ptr(const typename 
           std::vector<GeneralInstruction<IntegralType> >::const_iterator &p)
           { _jump_ptr = p; }
};

typedef GeneralInstruction<Integer> Instruction;

// Compile without combining instructions.
// 2nd through 7th args are PC, (# of breakpoints) vector,
// left, right, low, & high ends.
bool simple_compile(const std::string &,
                    int,
                    const std::vector<unsigned> &,
                    const End &, const End &, const End &, const End &,
                    std::vector<Instruction> *);

// Compile and combine instructions.
// 2nd through 7th args are PC, (# of breakpoints) vector,
// left, right, low, & high ends.
bool compile(const std::string &,
             int,
             const std::vector<unsigned> &,
             const End &, const End &, const End &, const End &,
             const EOFMethod &, std::vector<Instruction> *);

// Optimize.instruction sequence.  
// 1st argument is whether to print debug information;
// 2nd through 5th arguments are left, right, low & high bounds.
extern void optimize(bool,
              const End &, const End &, const End &, const End &,
              const EOFMethod &, std::vector<Instruction> *);

extern std::ostream &operator <<(std::ostream &, const Instruction &);

// Try to remap a cell value.  We handle limiting + abort bounds elsewhere.
inline Integer remap_cell(const Integer &v, const End &lo, const End &hi)
{
  if (lo.is_infinite() || hi.is_infinite())
     // No wraparound bounds present.
    return v;
  else if (lo.value() <= v && hi.value() >= v)
    return v;
  else
    return (v - lo.value()) % (hi.value() - lo.value() + 1) + lo.value();
}

// 
// Try to remap a tape position to the specified range.  Return FALSE if we
// must abort, TRUE o.w.
// 
extern bool remap_tape_difficult(Integer *, EndType, const Bound &, 
                                            EndType, const Bound &);

inline bool remap_tape(Integer *p_i, EndType e_lo, const Bound &lo, 
                                     EndType e_hi, const Bound &hi)
{
  if (lo.is_le(*p_i) && hi.is_ge(*p_i))
     return true;
  else
     return remap_tape_difficult(p_i, e_lo, lo, e_hi, hi);
}

// Fill in if...endif,
// while...endwhile pointers; return starting instruction pointer
template<typename IntegralType>
typename std::vector<GeneralInstruction<IntegralType> >::const_iterator
          link_control_structures(
        std::vector<GeneralInstruction<IntegralType> > *p_insns)
{
 typename std::vector<GeneralInstruction<IntegralType> >::const_iterator ip;
 std::vector<typename std::vector<GeneralInstruction<IntegralType> >::iterator> 
          if_while_locations;

  for (typename std::vector<GeneralInstruction<IntegralType> >::iterator 
       i = p_insns->begin(); i != p_insns->end(); ++i)
  {
    switch (i->type())
    {
      case IT_While:
      case IT_If:
           if_while_locations.push_back(i);
           break;

      case IT_EndWhile:
      case IT_EndIf:
           assert(!if_while_locations.empty());
           {
             typename std::vector<GeneralInstruction<IntegralType> >::iterator 
                     start = *(if_while_locations.end() - 1);

             i->set_jump_ptr(start);
             start->set_jump_ptr(i);
             i->set_jump_to(start - p_insns->begin());
             start->set_jump_to(i - p_insns->begin());
             if_while_locations.pop_back();
           }
           break;

      case IT_Start:
           ip = i + 1;
           break;

      default:
           break;
    }
  }

  assert(if_while_locations.empty());

  return ip;
}

template<typename IntegralType>
bool make_instruction_sequence(bool debug_info,
                               bool combine_insns,
                               const BF &machine,
  std::vector<GeneralInstruction<IntegralType> > *p_insns_out,
  typename std::vector<GeneralInstruction<IntegralType> >::const_iterator 
                       *p_start)
{
  std::vector<Instruction> insns;

  // Should only be called if destination type is unbounded, 
  // signed binary, or unsigned binary.
  if (!NumericLimits<IntegralType>::is_unbounded() &&
      !NumericLimits<IntegralType>::is_unsigned_binary() &&
      !NumericLimits<IntegralType>::is_signed_binary())
     return false;

  if (!combine_insns)
  {
    if (!simple_compile(machine.get_code(), machine.get_pc(), 
                        machine.get_breakpoint_vector(),
                        machine.get_left_end(), machine.get_right_end(), 
                        machine.get_low_end(), machine.get_high_end(), 
                        &insns))
       return false;
  } else
  {
    if (!compile(machine.get_code(), machine.get_pc(), 
                 machine.get_breakpoint_vector(),
                 machine.get_left_end(), machine.get_right_end(), 
                 machine.get_low_end(), machine.get_high_end(), 
                 machine.get_eof_method(), 
                 &insns))
       return false;

    optimize(debug_info, machine.get_left_end(), machine.get_right_end(), 
             machine.get_low_end(), machine.get_high_end(), 
             machine.get_eof_method(), &insns);
  }

  //
  // Having generated the instructions, we now convert them to
  // the appropriate type.
  // 
  p_insns_out->clear();
  for (std::vector<Instruction>::const_iterator
       i = insns.begin(); i != insns.end(); ++i)
  // In CheckDiv, Quotient, ExactQuotient, and Remainder,
  //     val may be out of the [lo..hi] range even if [lo..hi] is finite.
  //
  // When we change instructions into a machine integral type, this is done
  // in the case where either 
  //             (u) lo=0, hi=2^n-1 or (s) lo=2^(-(n-1)), hi=2^(n-1)-1.
  // The only case where val can be out of range is if (s) and
  // val=2^(n-1), in which case we only call CheckDiv and Remainder.
  // We change CheckDiv to CheckAnd and Remainder to And.
  if (NumericLimits<IntegralType>::is_signed_binary() &&
      i->value() == NumericLimits<IntegralType>::max() + 1 &&
      (i->type() == IT_CheckDiv || i->type() == IT_Remainder))
  {
    if (i->is_abort())
       p_insns_out->push_back(
            GeneralInstruction<IntegralType>(
            (i->type() == IT_CheckDiv ? IT_CheckAnd : IT_And),
            i->location(), i->cell(), NumericLimits<IntegralType>::max(),
            GeneralInstruction<IntegralType>::Abort()));
    else
       p_insns_out->push_back(
            GeneralInstruction<IntegralType>(
            (i->type() == IT_CheckDiv ? IT_CheckAnd : IT_And),
            i->location(), i->cell(), NumericLimits<IntegralType>::max()));
  }
  else 
  {       
    assert(NumericLimits<IntegralType>::is_unbounded() ||
           (i->value() >= NumericLimits<IntegralType>::min() &&
            i->value() <= NumericLimits<IntegralType>::max()));

    // Various Adds, Sets are special-cased for speed
    if (i->type() == IT_Set && i->length() == 0)
         p_insns_out->push_back(
              GeneralInstruction<IntegralType>
           (IT_Set0, i->location(), i->cell(), i->value()));
    else if (i->type() == IT_Add && i->length() == 0 &&
             i->value() == 
                 remap_cell(1, machine.get_low_end(), machine.get_high_end()))
         p_insns_out->push_back(
              GeneralInstruction<IntegralType>
           (IT_Inc, i->location(), i->cell(), i->value()));
    else if (i->type() == IT_Add && i->length() == 0 &&
             i->value() == 
                 remap_cell(-1, machine.get_low_end(), machine.get_high_end()))
         p_insns_out->push_back(
              GeneralInstruction<IntegralType>
           (IT_Dec, i->location(), i->cell(), i->value()));
    else if (i->type() == IT_Add && i->length() == 0)
         p_insns_out->push_back(
              GeneralInstruction<IntegralType>
           (IT_Add0, i->location(), i->cell(), i->value()));
    else if (i->type() == IT_Set && i->length() == 1 && 
             i->value() == 
                 remap_cell(1, machine.get_low_end(), machine.get_high_end()))
    {
      int c = i->cells(0);
      p_insns_out->push_back(GeneralInstruction<IntegralType>
           (IT_Set1v1, i->location(), i->cell(), i->value(), 1, &c));
    }
    else if (i->type() == IT_Add && i->length() == 1 && 
             i->value() == 
                 remap_cell(1, machine.get_low_end(), machine.get_high_end()))
    {
      int c = i->cells(0);
      p_insns_out->push_back(GeneralInstruction<IntegralType>
           (IT_Add1v1, i->location(), i->cell(), i->value(), 1, &c));
    }
    else if (i->type() == IT_Set && i->length() == 1)
    {
      int c = i->cells(0);
      p_insns_out->push_back(GeneralInstruction<IntegralType>
           (IT_Set1, i->location(), i->cell(), i->value(), 1, &c));
    }
    else if (i->type() == IT_Add && i->length() == 1)
    {
      int c = i->cells(0);
      p_insns_out->push_back(GeneralInstruction<IntegralType>
           (IT_Add1, i->location(), i->cell(), i->value(), 1, &c));
    }
    else 
      p_insns_out->push_back(GeneralInstruction<IntegralType>(*i));
  }

  typename std::vector<GeneralInstruction<IntegralType> >::const_iterator  
          ip = link_control_structures(p_insns_out);

  if (p_start)
     *p_start = ip;
  return true;
}
#endif
