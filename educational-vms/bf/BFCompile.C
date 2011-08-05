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

#include <cassert>
#include <cstddef>
#include <iostream>
#include <vector>

#include "BF.h"
#include "Integer.h"
#include "NumericLimits.h"

#include "BFImpl.h"

struct Type_info
{
  Integer mn;
  Integer mx;
  const char *name;
  const char *const_suffix;
  const char *unsigned_coercer;
  const char *format;
};

//
// `char' must have the same bounds as either `signed char' or `unsigned char'.
// 
static const Type_info info[] = 
{
  { 
    NumericLimits<unsigned char>::min(), 
    NumericLimits<unsigned char>::max(), 
    "unsigned char", "u", "", "hhu"
  },
  { 
    NumericLimits<signed char>::min(), 
    NumericLimits<signed char>::max(), 
    "signed char", "", "(unsigned char)", "hhd"
  },
  { 
    NumericLimits<unsigned short>::min(), 
    NumericLimits<unsigned short>::max(), 
    "unsigned short", "u", "", "hu"
  },
  { 
    NumericLimits<short>::min(), 
    NumericLimits<short>::max(), 
    "short", "", "(unsigned short)", "hd"
  },
  { 
    NumericLimits<unsigned>::min(), 
    NumericLimits<unsigned>::max(), 
    "unsigned", "u", "", "u"
  },
  { 
    NumericLimits<int>::min(), 
    NumericLimits<int>::max(), 
    "int", "", "(unsigned)", "d"
  },
  { 
    NumericLimits<unsigned long>::min(), 
    NumericLimits<unsigned long>::max(), 
    "unsigned long", "ul", "", "lu"
  },
  { 
    NumericLimits<long>::min(), 
    NumericLimits<long>::max(), 
    "long", "l", "(unsigned long)", "ld"
  }
};

// Output C code reference to a given tape cell.
class SpittableCell
{
  int i;
  const Integer &tapelen;
  EndType _end_l, _end_r;

public:

  SpittableCell(int j, const Integer &l, EndType end_l, EndType end_r)
          : i(j), tapelen(l), _end_l(end_l), _end_r(end_r) { }
  friend std::ostream &operator <<(std::ostream &, const SpittableCell &);
};

std::ostream &operator <<(std::ostream &o, const SpittableCell &c)
{
  if (c.i == 0)
  {
    o << "a[p]";
    return o;
  }

  // Both ends must be undefined or wraparound.
  assert(c._end_l == ET_undefined || c._end_l == ET_wraparound);
  assert(c._end_r == ET_undefined || c._end_r == ET_wraparound);
  // As for remap_tape_difficult, we must treat both ends as wraparound
  // if either end is.
  if (c._end_l == ET_wraparound || c._end_r == ET_wraparound)
  {
     o << "a[(p + " << ((Integer)c.i) % c.tapelen 
                    << "ul) % " << c.tapelen << "ul]";
  } else if (c.i < 0)
     o << "a[p - " << -c.i << "ul]";
  else
     o << "a[p + " << c.i << "ul]";
  return o;
}

// Compile (as C program) to output stream.  Returns success/failure.
// Starts with PC=0, position=0, empty tape, no breakpoints.
// Parameters are as set in machine.  Compilation may be impossible for some
// choices of parameters.
bool BF::compile_to(std::ostream &o)
{
  std::vector<unsigned> no_breakpoints(code.size(), 0u);
  std::vector<Instruction> insns;
  Integer type_width, tape_length;
  unsigned long bits_in_type;

  enum TypeIndex { T_unsigned_char, T_signed_char, 
         T_unsigned_short, T_short, T_unsigned, T_int, T_unsigned_long, 
         T_long };
  TypeIndex type_index;
  bool found = false;
  const Type_info *p_type_to_use;

  if (tape.get_left().is_infinite() || tape.get_right().is_infinite() ||
      get_left_end().value() > 0 || get_right_end().value() < 0 ||
      get_low_end().is_infinite() || get_high_end().is_infinite() ||
      get_low_end().value() > 0 || get_high_end().value() < 0)
      return false;
  
  for (size_t i = 0; i < sizeof(info) / sizeof(Type_info); i++)
  if (get_low_end().value() == info[i].mn &&
      get_high_end().value() == info[i].mx)
  {
    type_index = (TypeIndex)i;
    found = true;
    break;
  }

  if (!found)
     return false;

  p_type_to_use = &info[type_index];
  type_width = get_high_end().value() - get_low_end().value() + 1;
  tape_length = tape.get_right().value() - tape.get_left().value() + 1;

  // Check to see if type limits consistent with binary representation.
  // Also, we refuse to compile to <8-bit types.
  bits_in_type = type_width.power_of_2_dividing();
  if (bits_in_type < 8ul || type_width != pow(Integer(2), bits_in_type))
     return false;

  // Check to see if signed or unsigned type 
  //               consistent with binary representation.
  if (get_low_end().value() != 0)
  {
    if (get_low_end().value() != -pow(Integer(2), bits_in_type - 1) ||
        get_high_end().value() != pow(Integer(2), bits_in_type - 1) - 1)
       return false;
  } else
  {
    if (get_high_end().value() != pow(Integer(2), bits_in_type) - 1)
       return false;
  }

  if (!make_instruction_sequence(false, true, *this, &insns, 
             (std::vector<Instruction>::const_iterator *)NULL))
     return false;

  // Emit code
  o << "#include <stdio.h>\n"
    << "#include <stdlib.h>\n"
    << '\n'
    << p_type_to_use->name << " a[" << tape_length << "ul];\n"
    << '\n'
    << "int main(int argc, char **argv)\n{\n"
    << "  size_t p = " << -tape.get_left().value() << "ul;\n"
    << "  " << p_type_to_use->name << " temp;\n";
  if (get_input_method() != IM_decimal)
     o << "  int c;\n";

#define VALUE ip->value() << p_type_to_use->const_suffix
#define CELL SpittableCell(ip->cell(), tape_length, type_left, type_right)
#define CELLS(j) SpittableCell(ip->cells(j), tape_length, type_left, type_right)

  for (std::vector<Instruction>::const_iterator ip = insns.begin();
       ip != insns.end();
       ++ip)
  {
    switch (ip->type())
    {
      case IT_Clock:
      case IT_Start:
      case IT_Breakpoint:
               break;

      case IT_Halt:
               o << "  return 0;\n";
               break;

      case IT_Input:
               if (get_input_method() == IM_decimal)
               {
                 o << "  if (1 != scanf(\"%" 
                   << p_type_to_use->format
                   << "\", &" << CELL << "))\n";
               }
               else
                  o << "  if ((c = getchar()) == EOF)\n";
               switch (get_eof_method())
               {
                 case EOF_halt:
                      o << "     return 0;\n";
                      break;

                 case EOF_abort:
                      o << "     return 1;\n";
                      break;

                 case EOF_unchanged:
                      o << "     ;\n";
                      break;
 
                 case EOF_value:
                      o << "     " << CELL << " = " 
                        << get_eof_value() << p_type_to_use->const_suffix 
                        << ";\n";
                      break;
               }
               switch (get_input_method())
               {
                 case IM_decimal:
                      break;

                 case IM_unsigned:
                      if (get_high_end().value() >= 255)
                         o << "  else\n"
                           << "     " << CELL << " = c;\n";
                      else
                      {
                        assert(get_high_end().value() == 127);
                        o << "  else\n" 
                          << "     " << CELL << " = ((c & 128) ? 127 : c);\n";
                      }
                      break;
 
                 case IM_signed:
                      if (get_low_end().value() != 0)
                      {
                        assert(get_low_end().value() <= -128);
                        assert(get_high_end().value() >= 127);
                        o << "  else\n" 
                          << "     " << CELL 
                                     << " = ((c & 128) ? c - 256 : c);\n";
                      } else
                      {
                        assert(get_high_end().value() >= 127);
                        o << "  else\n" 
                          << "     " << CELL << " = ((c & 128) ? 0 : c);\n";
                      }
                      break;
               } 
               break;

      case IT_Output:
               if (get_output_method() == OM_decimal)
               {
                  o << "  printf(\"%" << p_type_to_use->format << "\\n\", "
                    << CELL << ");\n";
               }
               else
                  o << "  putchar((unsigned char)" 
                    << p_type_to_use->unsigned_coercer << CELL << ");\n";
               break;

      case IT_While:
               o << "  while (" << CELL << " != 0) {\n";
               break;

      case IT_EndWhile:
               o << "  }\n";
               break;

      case IT_If:
               o << "  if (" << CELL << " != 0) {\n";
               break;

      case IT_EndIf:
               o << "  }\n";
               break;

      case IT_AddPos:
               if (ip->cell() == 0)
                  ;
               else if (((ip->cell() > 0) && type_right == ET_wraparound) ||
                        ((ip->cell() < 0) && type_left == ET_wraparound))
               {
                 o << "  p = (p + " << ((Integer)ip->cell()) % tape_length
                                    << "ul) % " << tape_length << "ul;\n";
               } else if (ip->cell() <= -tape_length)
               {
                 assert(type_left == ET_abort || type_left == ET_undefined || 
                        type_left == ET_truncate);
                 if (type_left == ET_abort || type_left == ET_undefined)
                    o << "  return 3;\n";
                 else
                    o << "  p = 0ul;\n";

               } else if (ip->cell() >= tape_length)
               {
                 assert(type_right == ET_abort || type_right == ET_undefined || 
                        type_right == ET_truncate);
                 if (type_right == ET_abort || type_right == ET_undefined)
                    o << "  return 3;\n";
                 else
                    o << "  p = " << tape_length - 1 << "ul;\n";
               } else if ((ip->cell() < 0) && (type_left == ET_undefined))
                   o << "  p -= " << -ip->cell() << "ul;\n";
               else if (ip->cell() < 0)
               {
                 assert(type_left == ET_abort || type_left == ET_truncate);
                 o << "  if (p < " << -ip->cell() << "ul)\n";
                 if (type_left == ET_abort)
                    o << "     return 3;\n";
                 else
                    o << "     p = 0ul;\n";
                 o << "  else\n"
                   << "     p -= " << -ip->cell() << "ul;\n";
               } else if ((ip->cell() > 0) && (type_right == ET_undefined))
                   o << "  p += " << ip->cell() << "ul;\n";
               else if (ip->cell() > 0)
               {
                 assert(type_right == ET_abort || type_right == ET_truncate);
                 o << "  if (p > " << tape_length - 1 - ip->cell() << "ul)\n";
                 if (type_right == ET_abort)
                    o << "     return 3;\n";
                 else
                    o << "     p = " << tape_length - 1 << "ul;\n";
                 o << "  else\n"
                   << "     p += " << ip->cell() << "ul;\n";
               } else
                 assert(0);
               break;

      case IT_CheckEQ:
               o << "  if (" << CELL << " != " << VALUE << ")\n"
                 << "       return 2;\n";
               break;

      case IT_CheckLE:
               o << "  if (" << CELL << " > " << VALUE << ")\n"
                 << "       return 2;\n";
               break;

      case IT_CheckGE:
               o << "  if (" << CELL << " < " << VALUE << ")\n"
                 << "       return 2;\n";
               break;

      case IT_CheckDiv:
               if (ip->value() == p_type_to_use->mx + 1)
                  o << "  if ((" << CELL << " & " 
                    << ip->value() - 1 << p_type_to_use->const_suffix
                    << ") != 0)\n"
                    << "       return 2;\n";
               else
               {
                 assert(ip->value() > 0 && ip->value() <= p_type_to_use->mx);
                 o << "  if ((" << CELL << " % " << VALUE << ") != 0)\n"
                   << "       return 2;\n";
               }
               break;

      case IT_CheckAnd:
               o << "  if ((" << CELL << " & " << VALUE << ") != 0)\n"
                 << "       return 2;\n";
               break;

      case IT_Abort:
               o << "       return 2;\n";
               break;

      case IT_Max:
               o << "  if (" << CELL << " < " << VALUE << ")\n"
                 << "       " << CELL << " = " << VALUE << ";\n";
               break;

      case IT_Min:
               o << "  if (" << CELL << " > " << VALUE << ")\n"
                 << "       " << CELL << " = " << VALUE << ";\n";
               break;

      case IT_Sub:
               o << "  " << CELL << " = " 
                         << ip->value() << p_type_to_use->const_suffix
                         << " - " << CELL << ";\n";
               break;

      case IT_Quotient:
               assert(ip->value() > 0 && ip->value() <= p_type_to_use->mx);
               assert(0);    // This insn is not yet generated
               break;

      case IT_ExactQuotient:
               assert(ip->value() > 0 && ip->value() <= p_type_to_use->mx);
               o << "  " << CELL << " /= " << VALUE << ";\n";
               break;

      case IT_Remainder:
               {
                 assert(ip->value() > 0);
                 unsigned long bits = ip->value().power_of_2_dividing();
                 assert(ip->value() == pow((Integer)2, bits));
                 assert(ip->value() - 1 <= p_type_to_use->mx);
                 o << "  " << CELL << " &= "
                   << ip->value() - 1 << p_type_to_use->const_suffix << ";\n";
               }
               break;

      case IT_And:
               o << "  " << CELL << " &= " << VALUE << ";\n";
               break;

      case IT_Add:
      case IT_Add0:
      case IT_Add1v1:
      case IT_Add1:
      case IT_Inc:
      case IT_Dec:
               o << "  " << CELL << " += "; 
               if (ip->length() == 0 || ip->value() != 1)
                     o << VALUE;
               for (int j = 0; j < ip->length(); j++)
               {
                 if (j > 0 || ip->value() != 1)
                    o << " * ";
                 o << CELLS(j);
               }
               o << ";\n";
               break;

      case IT_Set:
      case IT_Set0:
      case IT_Set1v1:
      case IT_Set1:
               o << "  " << CELL << " = ";
               if (ip->length() == 0 || ip->value() != 1)
                     o << VALUE;
               for (int j = 0; j < ip->length(); j++)
               {
                 if (j > 0 || ip->value() != 1)
                    o << " * ";
                 o << CELLS(j);
               }
               o << ";\n";
               break;

      case IT_Permute:
               if (ip->length() > 1)
               {
                 o << "  temp = " << CELLS(0) << ";\n";
                 for (int j = 0; j < ip->length() - 1; j++)
                     o << "  " << CELLS(j) << " = " << CELLS(j+1) << ";\n";
                 o << "  " << CELLS(ip->length() - 1) << " = temp;\n";
               }
               break;
    }
  }

  o << "}\n";

  return true;
}
#undef CELL
#undef VALUE
#undef CELLS
