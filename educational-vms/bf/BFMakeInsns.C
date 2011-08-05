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
#include <set>

#include "BF.h"
#include "Integer.h"

#include "BFImpl.h"

std::ostream &operator <<(std::ostream &o, const Instruction &i)
{
  switch (i.type())
  {
    case IT_Clock:
         o << "clock";
         break;
         
    case IT_Start:
         o << "start";
         break;
         
    case IT_Breakpoint:
         o << "breakpoint";
         break;
         
    case IT_Halt:
         o << "halt";
         break;
         
    case IT_Input:
         o << "read T[p + " << i.cell() << "]";
         break;
         
    case IT_Output:
         o << "write T[p + " << i.cell() << "]";
         break;

    case IT_While:
         o << "while T[p + " << i.cell() << "] != 0";
         o << " (-> " << i.jump_to() << ")";
         break;
         
    case IT_EndWhile:
         o << "endwhile T[p + " << i.cell() << "] != 0";
         o << " (-> " << i.jump_to() << ")";
         break;
         
    case IT_If:
         o << "if T[p + " << i.cell() << "] != 0";
         o << " (-> " << i.jump_to() << ")";
         break;
         
    case IT_EndIf:
         o << "endif";
         o << " (-> " << i.jump_to() << ")";
         break;
         
    case IT_AddPos:
         o << "p += " << i.cell();
         break;
         
    case IT_CheckEQ:
         o << "Check T[p + " << i.cell() << "] == " << i.value();
         o << (i.is_abort() ? ", bound abort" : ", infinite loop");
         break;

    case IT_CheckLE:
         o << "Check T[p + " << i.cell() << "] <= " << i.value();
         o << (i.is_abort() ? ", bound abort" : ", infinite loop");
         break;

    case IT_CheckGE:
         o << "Check T[p + " << i.cell() << "] >= " << i.value();
         o << (i.is_abort() ? ", bound abort" : ", infinite loop");
         break;

    case IT_CheckDiv:
         o << "Check T[p + " << i.cell() << "] % " << i.value() << " == 0";
         o << (i.is_abort() ? ", bound abort" : ", infinite loop");
         break;

    case IT_CheckAnd:
         o << "Check (T[p + " << i.cell() << "] & " << i.value() << ") == 0";
         o << (i.is_abort() ? ", bound abort" : ", infinite loop");
         break;

    case IT_Abort:
         o << "p += " << i.cell();
         o << (i.is_abort() ? ", bound abort" : ", infinite loop");
         break;

    case IT_Max:
         o << "T[p + " << i.cell() << "] max= " << i.value();
         break;

    case IT_Min:
         o << "T[p + " << i.cell() << "] min= " << i.value();
         break;

    case IT_Sub:
         o << "T[p + " << i.cell() << "] = " << i.value() 
           << " - T[p + " << i.cell() << "]";
         break;

    case IT_Quotient:
    case IT_ExactQuotient:
         o << "T[p + " << i.cell() << "] /= " << i.value();
         if (i.type() == IT_ExactQuotient)
            o << " (exact)";
         break;

    case IT_Remainder:
         o << "T[p + " << i.cell() << "] %= " << i.value();
         break;

    case IT_And:
         o << "T[p + " << i.cell() << "] &= " << i.value();
         break;

    case IT_Add:
    case IT_Inc:
    case IT_Dec:
    case IT_Add0:
    case IT_Add1:
    case IT_Add1v1:
         o << "T[p + " << i.cell() << "] += ";
         if (i.length() == 0 || i.value() != 1)
             o << i.value();
         for (int j = 0; j < i.length(); j++)
         {
           if (j > 0 || i.value() != 1)
             o << " * ";
           o << "T[p + " << i.cells(j) << "]";
         }
         switch (i.type())
         {
           case IT_Add: break;
           case IT_Inc: o << " (Inc)"; break;
           case IT_Dec: o << " (Dec)"; break;
           case IT_Add0: o << " (Add0)"; break;
           case IT_Add1: o << " (Add1)"; break;
           case IT_Add1v1: o << " (Add1v1)"; break;
           default: assert(0); break;
         }
         break;

    case IT_Set:
    case IT_Set0:
    case IT_Set1:
    case IT_Set1v1:
         o << "T[p + " << i.cell() << "] = ";
         if (i.length() == 0 || i.value() != 1)
             o << i.value();
         for (int j = 0; j < i.length(); j++)
         {
           if (j > 0 || i.value() != 1)
             o << " * ";
           o << "T[p + " << i.cells(j) << "]";
         }
         switch (i.type())
         {
           case IT_Set: break;
           case IT_Set0: o << " (Set0)"; break;
           case IT_Set1: o << " (Set1)"; break;
           case IT_Set1v1: o << " (Set1v1)"; break;
           default: assert(0); break;
         }
         break;
         
    case IT_Permute:
         o << "Permute: ";
         for (int j = 0; j < i.length(); j++)
         {
           o << "T[p + " << i.cells(j) << "]";
           if (j < i.length() - 1)
              o << ", ";
         }
         o << " = ";
         for (int j = 0; j < i.length(); j++)
         {
           o << "T[p + " << i.cells((j + 1) % i.length()) << "]";
           if (j < i.length() - 1)
              o << ", ";
         }
         break;

    default:
         assert(0);
         break;
  }
  return o;
}

//
// Emit an add of ADDEND to T[p+CELL].
//
void emit_add(int pc, const End &low, const End &high, int addend, int cell,
              std::vector<Instruction> *p_insns)
{
  if (addend == 0)
     return;

  if (addend > 0)
  {
    if (!high.is_infinite())
    {
      switch (high.type())
      {
        case ET_truncate:
             if (!low.is_le(high.value() - addend - 1))
                p_insns->push_back(
                         Instruction(IT_Set, pc, cell, high.value()));
             else
             {
               p_insns->push_back(
                         Instruction(IT_Min, pc, cell, high.value() - addend));
               p_insns->push_back(Instruction(IT_Add, pc, cell, 
                                       remap_cell(addend, low, high)));
             }
             break;

        case ET_abort:
             assert(pc >= 0 && addend == 1);
             if (!low.is_le(high.value() - 1))
                p_insns->push_back(
                   Instruction(IT_Abort, pc, cell, Instruction::Abort()));
             else
             {
                p_insns->push_back(
                   Instruction(IT_CheckLE, pc, cell, high.value() - 1,
                               Instruction::Abort()));
                p_insns->push_back(Instruction(IT_Add, pc, cell, 
                                        remap_cell(addend, low, high)));
             }
             break;

        default: 
             p_insns->push_back(Instruction(IT_Add, pc, cell, 
                                     remap_cell(addend, low, high)));
             break;
      }
    }
    else
       p_insns->push_back(Instruction(IT_Add, pc, cell, addend));
  } else
  {
    if (!low.is_infinite())
    {
      switch (low.type())
      {
        case ET_truncate:
             if (!high.is_ge(low.value() - addend + 1))
                p_insns->push_back(
                         Instruction(IT_Set, pc, cell, low.value()));
             else
             {
               p_insns->push_back(
                         Instruction(IT_Max, pc, cell, low.value() - addend));
                p_insns->push_back(Instruction(IT_Add, pc, cell, 
                                     remap_cell(addend, low, high)));
             }
             break;

        case ET_abort:
             assert(pc >= 0 && addend == -1);
             if (!high.is_ge(low.value() + 1))
                p_insns->push_back(
                    Instruction(IT_Abort, pc, cell, Instruction::Abort()));
             else
             {
                p_insns->push_back(
                    Instruction(IT_CheckGE, pc, cell, low.value() + 1,
                               Instruction::Abort()));
                p_insns->push_back(Instruction(IT_Add, pc, cell, 
                                     remap_cell(addend, low, high)));
             }
             break;

        default: 
             p_insns->push_back(Instruction(IT_Add, pc, cell, 
                                          remap_cell(addend, low, high)));
             break;
      }
    }
    else
       p_insns->push_back(Instruction(IT_Add, pc, cell, addend));
  }
}

//
// Compile BF program into sequence of instructions.
// Returns success/failure.  No instruction combining.
//
bool simple_compile(const std::string &code,
                    int pc,
                    const std::vector<unsigned> &num_breakpoints_at,
                    const End &left, const End &right, 
                    const End &low, const End &high,
                    std::vector<Instruction> *p_insns)
{
  size_t n_brackets = 0;

  assert(p_insns != NULL);

  p_insns->clear();

  for (std::string::const_iterator i = code.begin(); i != code.end(); i++)
  {
    if (num_breakpoints_at[i - code.begin()] != 0)
       p_insns->push_back(Instruction(IT_Breakpoint, i - code.begin()));

    if (i - code.begin() == pc)
       p_insns->push_back(Instruction(IT_Start, i - code.begin()));

    switch (*i)
    {
      case '+':
      case '-':
         emit_add(i - code.begin(), 
                  low, high, (*i == '+' ? 1 : -1), 0, p_insns);
         p_insns->push_back(Instruction(IT_Clock, i - code.begin() + 1));
         break;

      case '<':
      case '>':
         p_insns->push_back(Instruction(IT_AddPos, i - code.begin(), 
                                        (*i == '>' ? 1 : -1)));
         p_insns->push_back(Instruction(IT_Clock, i - code.begin() + 1));
         break;

      case ',': 
         p_insns->push_back(Instruction(IT_Input, i - code.begin(), 0));
         p_insns->push_back(Instruction(IT_Clock, i - code.begin() + 1));
         break;

      case '.': 
         p_insns->push_back(Instruction(IT_Output, i - code.begin(), 0));
         p_insns->push_back(Instruction(IT_Clock, i - code.begin() + 1));
         break;

      case '[': 
         n_brackets++;
         p_insns->push_back(Instruction(IT_While, i - code.begin(), 0));
         p_insns->push_back(Instruction(IT_Clock, i - code.begin() + 1));
         break;

      case ']':
         if (n_brackets-- == 0)
            return false;
         else
         {
           p_insns->push_back(Instruction(IT_EndWhile, i - code.begin(), 0));
           p_insns->push_back(Instruction(IT_Clock, i - code.begin() + 1));
         }
         break;

      default:    // Comment, ignore
         break;
    }
  }

  if (code.size() == pc)
     p_insns->push_back(Instruction(IT_Start, code.size()));

  p_insns->push_back(Instruction(IT_Halt, code.size()));

  return n_brackets == 0;
}

//
// Compile BF program into sequence of instructions.
// Returns success/failure.
// May combine instructions.
//
// If can_combine_tape_ops is set, then we may delay tape position changes;
// this will mean that at some time, when the BF program is at tape position
// x+q, the output insns will place us at position x, acessing tape cell
// T[p+q].  When this happens, the BF program will have been at cell x
// at some point in the past.
bool compile(const std::string &code,
             int pc,
             const std::vector<unsigned> &num_breakpoints_at,
             const End &left, const End &right, const End &low, const End &high,
             const EOFMethod &eof_method,
             std::vector<Instruction> *p_insns)
{
  bool can_combine_tape_ops = 
       (left.is_infinite() || left.type() == ET_wraparound
                           || left.type() == ET_undefined) &&
       (right.is_infinite() || right.type() == ET_wraparound
                            || right.type() == ET_undefined);

  bool can_combine_cell_ops = 
       (low.is_infinite() || low.type() == ET_undefined 
                          || low.type() == ET_wraparound) &&
       (high.is_infinite() || high.type() == ET_undefined
                           || high.type() == ET_wraparound);

  bool eof_may_abort = (eof_method == EOF_halt || eof_method == EOF_abort);

  int cell = 0, addend = 0;
  size_t n_brackets = 0;
  std::set<size_t> easy_while_locations;

  if (can_combine_tape_ops)
  // We prescan the program for WHILE...ENDWHILE loops in which tape
  // movement may be avoided.
  {
    std::vector<size_t> while_offsets;
    std::vector<int> initial_cells;
    std::vector<bool> can_avoid_movement;
    int cell = 0;
    bool must_move = false;
   
    for (std::string::const_iterator i = code.begin(); i != code.end(); i++)
    {
      if (cell != 0 &&
          (i - code.begin() == pc || 
           num_breakpoints_at[i - code.begin()] != 0))
      {
        cell = 0;
        must_move = true;
      }
      switch (*i)
      {
        case '>': cell++; 
                  break;

        case '<': cell--; 
                  break;

        case '[': can_avoid_movement.push_back(must_move);   // Save must_move
                  while_offsets.push_back(i - code.begin());
                  initial_cells.push_back(cell);
                  must_move = false;
                  break;

        case ']': if (initial_cells.empty())
                     return false;
                  if (must_move || *(initial_cells.end() - 1) != cell)
                  {
                    // We must execute AddPos within the loop.  In this
                    // case, we will reset the cell to 0 before the start
                    // of the loop and before each execution of ENDWHILE.
                    cell = 0;
                    must_move = true;
                  } else
                  {
                    // We can avoid all tape movement within the loop.
                    easy_while_locations.insert(*(while_offsets.end() - 1));
                    easy_while_locations.insert(i - code.begin());
                    must_move = *(can_avoid_movement.end() - 1);
                  }
                  can_avoid_movement.pop_back();
                  while_offsets.pop_back();
                  initial_cells.pop_back();
                  break;

        default: break;
      }
    }
    if (!initial_cells.empty())
       return false;
  }
  
  assert(p_insns != NULL);

  p_insns->clear();

  for (std::string::const_iterator i = code.begin(); i != code.end(); i++)
  {
    //
    // If the current operation may abort (or is the start of the program)
    // we must reset the head to its real value.  This does not
    // apply to +, -, and , since these all indicate the head location.
    //
    // In the non-wraparound case, we must also reset the head if we
    // are performing any operation on it, or if we are changing movement
    // direction.
    if (cell != 0 &&
        (i - code.begin() == pc || 
         num_breakpoints_at[i - code.begin()] != 0 ||
         (*i == '[' && !can_combine_tape_ops) ||
         (*i == ']' && !can_combine_tape_ops) ||
         (*i == '+' && !can_combine_tape_ops) ||
         (*i == '-' && !can_combine_tape_ops) ||
         (*i == ',' && !can_combine_tape_ops) ||
         (*i == '.' && !can_combine_tape_ops) ||
         (*i == '<' && cell > 0 && !can_combine_tape_ops) ||
         (*i == '>' && cell < 0 && !can_combine_tape_ops)
       ))
    {
      assert(cell >= 0 || left.type() != ET_abort);
      assert(cell <= 0 || right.type() != ET_abort);
      p_insns->push_back(Instruction(IT_AddPos, -1, cell));
      cell = 0; 
    }

    // If the current operation will or may do something besides alter the 
    // current tape cell, we must output the current tape cell.  We must also 
    // output it if we are changing the direction of alteration in the 
    // non-wraparound case.
    if (addend != 0 &&
        (i - code.begin() == pc ||
         num_breakpoints_at[i - code.begin()] != 0 ||
         *i == '.' || *i == '[' || *i == ']' || *i == '<' || *i == '>' ||
         (*i == '-' && addend > 0 && !can_combine_cell_ops) ||
         (*i == '+' && addend < 0 && !can_combine_cell_ops) ||
         (*i == ',' && eof_may_abort) 
       ))
    {
      assert(addend >= 0 || low.type() != ET_abort);
      assert(addend <= 0 || high.type() != ET_abort);
      assert(can_combine_tape_ops || cell == 0);
      emit_add(-1, low, high, addend, cell, p_insns);
      addend = 0;
    }

    if (num_breakpoints_at[i - code.begin()] != 0)
       p_insns->push_back(Instruction(IT_Breakpoint, i - code.begin()));

    if (i - code.begin() == pc)
       p_insns->push_back(Instruction(IT_Start, i - code.begin()));

    switch (*i)
    {
      case '+':
         if (high.type() == ET_abort)
         {
           assert(addend == 0);
           assert(can_combine_tape_ops || cell == 0);
           emit_add(i - code.begin(), low, high, 1, cell, p_insns);
         } else 
         {
           assert(can_combine_cell_ops || addend >= 0);
           addend++;
         }
         break;

      case '-':
         if (low.type() == ET_abort)
         {
           assert(addend == 0);
           assert(can_combine_tape_ops || cell == 0);
           emit_add(i - code.begin(), low, high, -1, cell, p_insns);
         } else 
         {
           assert(can_combine_cell_ops || addend <= 0);
           addend--;
         }
         break;

      case '<':
         if (left.type() == ET_abort)
         {
           assert(cell == 0);
           p_insns->push_back(Instruction(IT_AddPos, i - code.begin(), -1));
         } else
         {
           assert(can_combine_tape_ops || cell <= 0);
           cell--;
         }
         break;

      case '>':
         if (right.type() == ET_abort)
         {
           assert(cell == 0);
           p_insns->push_back(Instruction(IT_AddPos, i - code.begin(), 1));
         } else
         {
           assert(can_combine_tape_ops || cell >= 0);
           cell++;
         }
         break;

      case ',': 
         addend = 0;
         assert(can_combine_tape_ops || cell == 0);
         p_insns->push_back(Instruction(IT_Input, i - code.begin(), cell));
         break;

      case '.': 
         assert(can_combine_tape_ops || cell == 0);
         p_insns->push_back(Instruction(IT_Output, i - code.begin(), cell));
         break;

      case '[': 
         n_brackets++;
         if (easy_while_locations.find(i - code.begin()) != 
                     easy_while_locations.end())
            assert(can_combine_tape_ops);
         else
         {
           assert(cell >= 0 || left.type() != ET_abort);
           assert(cell <= 0 || right.type() != ET_abort);
           if (cell != 0) 
           {
             p_insns->push_back(Instruction(IT_AddPos, -1, cell));
             cell = 0; 
           }
         }
         p_insns->push_back(Instruction(IT_While, i - code.begin(), cell));
         break;

      case ']':
         if (n_brackets-- == 0)
            return false;
         if (easy_while_locations.find(i - code.begin()) != 
                     easy_while_locations.end())
            assert(can_combine_tape_ops);
         else
         {
           assert(cell >= 0 || left.type() != ET_abort);
           assert(cell <= 0 || right.type() != ET_abort);
           if (cell != 0) 
           {
             p_insns->push_back(Instruction(IT_AddPos, -1, cell));
             cell = 0; 
           }
         }
         p_insns->push_back(Instruction(IT_EndWhile, i - code.begin(), cell));
         break;

      default:    // Comment, ignore
         break;
    }
  }

  if (cell != 0)
  {
    assert(cell >= 0 || left.type() != ET_abort);
    assert(cell <= 0 || right.type() != ET_abort);
    p_insns->push_back(Instruction(IT_AddPos, -1, cell));
  }

  assert(addend >= 0 || low.type() != ET_abort);
  assert(addend <= 0 || high.type() != ET_abort);
  emit_add(-1, low, high, addend, 0, p_insns);

  if (code.size() == pc)
     p_insns->push_back(Instruction(IT_Start, code.size()));

  p_insns->push_back(Instruction(IT_Halt, code.size()));

  return n_brackets == 0;
}

void print_insn_vector(const std::vector<Instruction> *p_insns)
{
  std::cout << "Instructions:\n\n";
  for (std::vector<Instruction>::const_iterator i = p_insns->begin();
       i != p_insns->end();
       ++i)
      std::cout << i - p_insns->begin() << ": (PC=" << i->location()
                << ") " << *i << '\n';
}

void BF::print_insns(bool combine_insns, bool debug_info)
{
  std::vector<Instruction> insns;

  if (!make_instruction_sequence(debug_info, 
                         combine_insns, *this, &insns, 
                         (std::vector<Instruction>::const_iterator *)NULL))
  {
    std::cout << "Invalid program.\n";
    return;
  }

  print_insn_vector(&insns);
}
