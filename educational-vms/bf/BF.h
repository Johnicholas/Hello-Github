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

#ifndef BF_INCL

#define BF_INCL

#include <vector>
#include <string>
#include <iosfwd>
#include <map>

#include "Integer.h"

class Bound
{
  bool is_inf;
  Integer val;

public:

  Bound() : is_inf(true), val() { }
  explicit Bound(const Integer &v) : is_inf(false), val(v) { }

  bool is_infinite() const { return is_inf; }
  Integer value() const { return val; }

  void set_value(const Integer &v) { is_inf = false; val = v; }
  void make_infinite() { is_inf = true; val = 0; }

  bool is_eq(const Integer &a) const { return !is_inf && a == val; }
  bool is_le(const Integer &a) const { return is_inf || val <= a; }
  bool is_ge(const Integer &a) const { return is_inf || val >= a; }
  bool is_eq(int a) const { return !is_inf && a == val; }
  bool is_le(int a) const { return is_inf || val <= a; }
  bool is_ge(int a) const { return is_inf || val >= a; }
};

// Check to see if arg1 is within bounds arg2 & arg3; should have arg2 <= arg3
extern bool is_within(const Integer &, const Bound &, const Bound &);

// Trim arg1 to within bounds arg2 & arg3; should have arg2 <= arg3.
extern Integer trim_to(const Integer &, const Bound &, const Bound &);

enum EndType { ET_abort, ET_truncate, ET_wraparound, ET_undefined };

class End : private Bound
{
  EndType t;

public:

  End() : Bound(), t(ET_truncate) { }
  End(Integer v, EndType tt) : Bound(v), t(tt) { }
  End(const Bound &b, EndType tt) 
        : Bound(b), t(b.is_infinite() ? ET_truncate : tt) { }

  EndType type() const { return t; }
  void set_type(EndType tt) { if (!is_infinite()) t = tt; }

  Bound bound() const { return static_cast<Bound>(*this); }

  void set_value(const Integer &v) { Bound::set_value(v); }
  // We make Bound private so make_infinite() can be securely overridden here.
  void make_infinite() { Bound::make_infinite(); t = ET_truncate; }

  bool is_infinite() const { return Bound::is_infinite(); }
  Integer value() const { return Bound::value(); }
  bool is_eq(const Integer &a) const { return Bound::is_eq(a); }
  bool is_le(const Integer &a) const { return Bound::is_le(a); }
  bool is_ge(const Integer &a) const { return Bound::is_ge(a); }
  bool is_eq(int a) const { return Bound::is_eq(a); }
  bool is_le(int a) const { return Bound::is_le(a); }
  bool is_ge(int a) const { return Bound::is_ge(a); }
};

extern std::ostream &operator <<(std::ostream &, const End &);

//
// Potentially infinite array of Integers, with adjustable bounds.
// All elements are initialized to 0.
//
class BiVector
{
  std::map<Integer, Integer> contents;
  Bound left, right;

  void trim_bivector_at_left();
  void trim_bivector_at_right();
    
public:

  BiVector() : contents(), left(), right() { }

  const Bound &get_left() const { return left; }
  const Bound &get_right() const { return right; }

  // Clear contents of tape (does not reset bounds)
  void clear() { contents.clear(); }

  // Return success/failure
  bool get(Integer, Integer *) const;

  // Return success/failure
  bool set(Integer, const Integer &);

  // Reset left bound (must be to <= right bound.)
  void set_left(const Bound &);

  // Reset right bound (must be to >= left bound.)
  void set_right(const Bound &);

  // Trim all values in vector to be between LOW and HIGH, inclusive.
  // We should have LOW <= 0, HIGH >= 0.
  void trim_values(const Bound &, const Bound &);
};

enum InputMethod { IM_signed, IM_unsigned, IM_decimal };
enum OutputMethod { OM_char, OM_decimal };
enum EOFMethod { EOF_halt, EOF_abort, EOF_unchanged, EOF_value };

enum RunTermination 
     { RT_Program_Invalid, RT_Infinite_Loop,
       RT_Normal, RT_Breakpoint, RT_EOF_Error, RT_Bound_Error, RT_Chunk_Done };

struct BF
{
  std::string code;
  BiVector tape;
  Integer position;
  int pc;

  EndType type_left, type_right;
  End low, high;
  Integer eof_value;
  InputMethod input_method;
  OutputMethod output_method;
  EOFMethod eof_method;

  std::vector<int> breakpoints;
  std::vector<unsigned> num_breakpoints_at;

public:

  BF() : code(), tape(), position(0), pc(0),

         type_left(ET_truncate), type_right(ET_truncate), 
         low(), high(), 
         eof_value(0),
         input_method(IM_unsigned), output_method(OM_char),
         eof_method(EOF_value),

         breakpoints(), num_breakpoints_at() { }

  // Get # of breakpoints
  int num_breakpoints() const { return breakpoints.size(); }

  // Get location of breakpoint with given #; 
  // returns -1 on error, -2 if no breakpoint
  int get_breakpoint(const Integer &) const;

  // Add breakpoint at given location
  // Returns breakpoint number (0...#B-1), or -1 on error
  int add_breakpoint(const Integer &); 

  // Delete breakpoint with given #
  // Returns success/failure (deleting an already deleted breakpoint succeeds)
  bool delete_breakpoint(const Integer &);

  InputMethod get_input_method() const { return input_method; }
  OutputMethod get_output_method() const { return output_method; }
  EOFMethod get_eof_method() const { return eof_method; }
  Integer get_eof_value() const { return eof_value; }

  void set_input_method(InputMethod im) { input_method = im; }
  void set_output_method(OutputMethod om) { output_method = om; }
  void set_eof_method(EOFMethod em) { eof_method = em; }
  void set_eof_value(const Integer &i) 
       { eof_value = trim_to(i, low.bound(), high.bound()); }

  End get_left_end() const { return End(tape.get_left(), type_left); }
  End get_right_end() const { return End(tape.get_right(), type_right); }
  const End &get_low_end() const { return low; }
  const End &get_high_end() const { return high; }

  // Return succ/failure
  bool set_left_end(const End &);

  // Return succ/failure
  bool set_right_end(const End &); 

  // Return succ/failure
  bool set_low_end(const End &);

  // Return succ/failure
  bool set_high_end(const End &);

  void clear_tape() { tape.clear(); }

  // Returns succ/failure 
  bool get_tape(const Integer &i, Integer *p_v) const 
  { return tape.get(i, p_v); }

  // Returns succ/failure 
  bool set_tape(const Integer &i, const Integer &v) 
  { return tape.set(i, trim_to(v, low.bound(), high.bound())); }

  std::string get_code() const { return code; }

  // Returns succ/failure
  bool set_code(const std::string &);

  int get_pc() const { return pc; }

  Integer get_position() const { return position; }
 
  // Returns succ/failure 
  bool set_pc(const Integer &);

  // Returns succ/failure 
  bool set_position(const Integer &); 

  // Get breakpoint vector
  std::vector<unsigned> get_breakpoint_vector() const
  { return num_breakpoints_at; }

  // For debugging purposes, print out the series of instructions
  // that we would compile the BF program into.
  // Args are whether or not to combine insns, whether or not to print debug 
  // info.
  void print_insns(bool = true, bool = false);

  // Run for given # of instructions (forever if # < 0)
  RunTermination run(const Integer &);

  // Compile (as C program) to output stream.  Returns success/failure.
  // Starts with PC=0, position=0, empty tape, no breakpoints.
  // Parameters are as set in machine.  Compilation may be impossible for some
  // choices of parameters.
  bool compile_to(std::ostream &);

  template<typename IntegralType>
  friend class Interpreter;
};

#endif
