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
#include <utility>       
#include <map>
#include <set>
#include <vector>

#include "BF.h"
#include "Integer.h"

#include "BFImpl.h"

class CellsKnown
{
  struct CellRecord;   // forward def'n

  static const int KNOWN = 0;
  static const int WRITTEN = 1;

  bool print_debug_info;
  End _low, _high;

  std::vector<CellRecord *> head_cell[2];


  // Value is sign * (token value) + val
  struct CellRecord
  {
    int _cell;

    // 0 = known, 1 = written
    int _sign[2];
    Integer _val[2];
    int _token[2];
    CellRecord *_last[2], *_next[2];

    bool _flag;
  };

  // NB: This code all depends on pointers to CellRecords within a std::map<...>
  // not getting invalidated.  This should be the case for the usual balanced
  // tree implementation.
  
  void remove_list(CellRecord *c, int i)
  {
    int t = c->_token[i];
    if (t < 0)
       return;
    if (head_cell[i][t] == c)
       head_cell[i][t] = c->_next[i];
    if (c->_last[i])
       c->_last[i]->_next[i] = c->_next[i];
    if (c->_next[i])
       c->_next[i]->_last[i] = c->_last[i];
    c->_last[i] = NULL;
    c->_next[i] = NULL;
  } 
  
  void add_list(CellRecord *c, int i)
  {
    int t = c->_token[i];
    if (t < 0)
       return;
    if (head_cell[i][t])
       head_cell[i][t]->_last[i] = c;
    c->_next[i] = head_cell[i][t];
    head_cell[i][t] = c;
    c->_last[i] = NULL;
  }

  void copy_cell(CellRecord *c_from, CellRecord *c_to, 
                 int cell_ofs, std::vector<Instruction> *p_insns_out)
  {
    assert(c_from->_token[WRITTEN] >= 0);

    if (c_to->_token[WRITTEN] == c_from->_token[WRITTEN] &&
        c_to->_sign[WRITTEN] == c_from->_sign[WRITTEN] &&
        c_to->_val[WRITTEN] == c_from->_val[WRITTEN])
       return;

    remove_list(c_to, WRITTEN);
    int temp = c_from->_cell - cell_ofs;
    p_insns_out->push_back(Instruction(
            IT_Set, -1, c_to->_cell - cell_ofs, 
            remap_cell(1, _low, _high), 1, &temp));
    if (print_debug_info)
       std::cout << "**Out: " << *(p_insns_out->end() - 1) << '\n';
    c_to->_token[WRITTEN] = c_from->_token[WRITTEN];
    c_to->_sign[WRITTEN] = c_from->_sign[WRITTEN];
    c_to->_val[WRITTEN] = c_from->_val[WRITTEN];
    add_list(c_to, WRITTEN);
  }

  // Save token currently written in a cell; 
  //            this may set up a chain or loop of assignments.
  // flag = 0 if no solution found yet; 1 if solution with loops found;
  //        2 if solution without loops found.
  void next_cell(CellRecord *c, 
                 std::vector<CellRecord *> *p_previous_cells,
                 std::vector<CellRecord *> *p_current_found,
                 int *p_loop_flag)
  {
    int t = c->_token[WRITTEN];
    bool no_problem = false;
    CellRecord *cc;

    if (t < 0)
       no_problem = true;
    else
    {
      assert(head_cell[WRITTEN][t]);
      if (head_cell[WRITTEN][t]->_next[WRITTEN] != NULL)
         no_problem = true;           // >= 2 cells have this token written
      else
      {
        cc = head_cell[KNOWN][t];
        if (cc == NULL || (cc == c && cc->_next[KNOWN] == NULL))
           no_problem = true;      // No-one needs it, or only we need it
      }
    }

    if (no_problem)
    {
      if (*p_loop_flag < 2 ||
          p_current_found->size() > p_previous_cells->size())
      {
        *p_loop_flag = 2;
        *p_current_found = *p_previous_cells;
      }
      return;
    }

    c->_flag = true;

    for (; cc != NULL; cc = cc->_next[KNOWN])
    if (cc != c)
    {
      // Copy c into cc and recurse.
      if (cc->_flag)
      {
       // We have encountered a loop.
        assert(p_previous_cells->size() >= 2 && (*p_previous_cells)[0] == cc);
        if (*p_loop_flag == 0 ||
            (*p_loop_flag == 1 &&
             p_current_found->size() > p_previous_cells->size()))
        {
          *p_loop_flag = 1;
          *p_current_found = *p_previous_cells;
        }
      } else
      {
        p_previous_cells->push_back(cc);
        next_cell(cc, p_previous_cells, p_current_found, p_loop_flag);
        p_previous_cells->pop_back();
      }
    }

    c->_flag = false;
  }

  // The token in this cell is about to be clobbered.  Save it.
  void prep(CellRecord *c,
            int cell_ofs, std::vector<Instruction> *p_insns_out)
  {
    std::vector<CellRecord *> cells_to_write;
    std::vector<CellRecord *> cells_to_write_temp;
    int loop_flag = 0;

    cells_to_write_temp.push_back(c);
    next_cell(c, &cells_to_write_temp, &cells_to_write, &loop_flag);
    assert(loop_flag > 0);
    if (loop_flag == 2)
    {
      assert(!cells_to_write.empty());
      for (std::vector<CellRecord *>::const_iterator 
                       i = cells_to_write.end() - 1;
           i != cells_to_write.begin(); )
      {
        --i;
        copy_cell(*i, *(i+1), cell_ofs, p_insns_out);
      }
    } else
    {
      size_t len = cells_to_write.size();
      assert(len >= 2);
      int token = (*(cells_to_write.end() - 1))->_token[WRITTEN];
      int sign = (*(cells_to_write.end() - 1))->_sign[WRITTEN];
      Integer val = (*(cells_to_write.end() - 1))->_val[WRITTEN];

      for (std::vector<CellRecord *>::const_iterator 
                 i = cells_to_write.end(),
                 j = cells_to_write.end() - 1;
                 i != cells_to_write.begin(); )
      {
        --i;
        remove_list(*i, WRITTEN);
        if (j != cells_to_write.begin())
        {
          --j;
          (*i)->_token[WRITTEN] = (*j)->_token[WRITTEN];
          (*i)->_sign[WRITTEN] = (*j)->_sign[WRITTEN];
          (*i)->_val[WRITTEN] = (*j)->_val[WRITTEN];
        } else
        {
          (*i)->_token[WRITTEN] = token;
          (*i)->_sign[WRITTEN] = sign;
          (*i)->_val[WRITTEN] = val;
        }
        add_list(*i, WRITTEN);
      }
      // A big cycle is a product of little cycles:
      // 1->2->3->4->5->6->7->8(->1) =
      // 5->6->7->8(->5) followed by 1->2->3->4->5(->1), etc.
      for (std::vector<CellRecord *>::const_iterator p = cells_to_write.end();
           p != cells_to_write.begin(); )
      {
        int temp[MAX_INSTR_LENGTH];
        size_t chunk_size = 
                std::min(p - cells_to_write.begin(), MAX_INSTR_LENGTH);
        for (size_t j = 0; j < chunk_size; j++)
            temp[j] = (*--p)->_cell - cell_ofs;
        p_insns_out->push_back(Instruction(IT_Permute, -1, 0, 0, 
                                                       chunk_size, temp));
        if (print_debug_info)
           std::cout << "**Out: " << *(p_insns_out->end() - 1) << '\n';
      }
    }
  }

  void _dump(CellRecord *c,
             int cell_ofs, std::vector<Instruction> *p_insns_out)
  {
    if (c->_token[KNOWN] == c->_token[WRITTEN] &&
        c->_sign[KNOWN] == c->_sign[WRITTEN] &&
        c->_val[KNOWN] == c->_val[WRITTEN])
       return;

    if (c->_token[WRITTEN] >= 0 && c->_token[WRITTEN] != c->_token[KNOWN])
       prep(c, cell_ofs, p_insns_out);

    int t = c->_token[KNOWN];
    if (t < 0)
    {
      assert(c->_sign[KNOWN] == 0);
      p_insns_out->push_back(Instruction(
            IT_Set, -1, c->_cell - cell_ofs, c->_val[KNOWN]));
      if (print_debug_info)
         std::cout << "*Out: " << *(p_insns_out->end() - 1) << '\n';
      remove_list(c, WRITTEN);
      c->_token[WRITTEN] = -1;
      c->_sign[WRITTEN] = 0;
      c->_val[WRITTEN] = c->_val[KNOWN];
      add_list(c, WRITTEN);
    } else
    {
      CellRecord *p = NULL;

      // Who's got our token?
      assert(c->_sign[KNOWN] == 1 || c->_sign[KNOWN] == -1);
      if (c->_token[WRITTEN] == t)
         p = c;
      if (p == NULL)
      {
        for (p = head_cell[WRITTEN][t]; p != NULL; p = p->_next[WRITTEN])
        {
          assert(p->_token[WRITTEN] == t);
          if (p->_sign[WRITTEN] == c->_sign[KNOWN] && 
              p->_val[WRITTEN] == c->_val[KNOWN])
             break;
        }
      }
      if (p == NULL)
      {
        for (p = head_cell[WRITTEN][t]; p != NULL; p = p->_next[WRITTEN])
        {
          assert(p->_token[WRITTEN] == t);
          if (p->_sign[WRITTEN] == c->_sign[KNOWN])
              break;
        }
      }
      if (p == NULL)
         p = head_cell[WRITTEN][t];
      assert(p != NULL);
      remove_list(c, WRITTEN);
      assert(p->_token[WRITTEN] == t);
      if (c != p)
      {
        int temp = p->_cell - cell_ofs;
        p_insns_out->push_back(Instruction(
              IT_Set, -1, c->_cell - cell_ofs, 
                      remap_cell(1, _low, _high), 1, &temp));
        if (print_debug_info)
           std::cout << "*Out: " << *(p_insns_out->end() - 1) << '\n';
        c->_sign[WRITTEN] = p->_sign[WRITTEN];
        c->_token[WRITTEN] = p->_token[WRITTEN];
        c->_val[WRITTEN] = p->_val[WRITTEN];
      } 
      if (c->_sign[WRITTEN] != c->_sign[KNOWN])
      {
        p_insns_out->push_back(Instruction(
         IT_Sub, -1, c->_cell - cell_ofs, 
                 remap_cell(c->_val[KNOWN] + c->_val[WRITTEN], _low, _high)));
        if (print_debug_info)
           std::cout << "*Out: " << *(p_insns_out->end() - 1) << '\n';
        c->_sign[WRITTEN] *= -1;
        c->_val[WRITTEN] = c->_val[KNOWN];
      } else
      if (c->_val[WRITTEN] != c->_val[KNOWN])
      {
        p_insns_out->push_back(Instruction(
         IT_Add, -1, c->_cell - cell_ofs, 
            remap_cell(c->_val[KNOWN] - c->_val[WRITTEN], _low, _high)));
        if (print_debug_info)
           std::cout << "*Out: " << *(p_insns_out->end() - 1) << '\n';
        c->_val[WRITTEN] = c->_val[KNOWN];
      }
      assert(c->_token[WRITTEN] == c->_token[KNOWN] &&
             c->_sign[WRITTEN] == c->_sign[KNOWN] &&
             c->_val[WRITTEN] == c->_val[KNOWN]);
      add_list(c, WRITTEN);
    }
  }

  std::map<int, CellRecord> cells;

public:

  CellsKnown(const End &low, const End &high, bool b = false) 
        : print_debug_info(b), _low(low), _high(high) { }

  bool is_known(int cell, int *p_sign, Integer *p_value, int *p_token)
  {
    std::map<int, CellRecord>::const_iterator i = cells.find(cell);
    if (i == cells.end())
       return false;
    if (p_sign)
       *p_sign = i->second._sign[KNOWN];
    if (p_value)
       *p_value = i->second._val[KNOWN];
    if (p_token)
       *p_token = i->second._token[KNOWN];
    return true;
  }

  bool is_known_constant(int cell, Integer *p_value)
  {
    std::map<int, CellRecord>::const_iterator i = cells.find(cell);
    if (i == cells.end() || i->second._token[KNOWN] >= 0)
       return false;
    if (p_value)
       *p_value = i->second._val[KNOWN];
    return true;
  }

  void debug_print()
  {
    for (std::map<int, CellRecord>::iterator i = cells.begin();
         i != cells.end(); ++i)
    {
      std::cout << " T[" << i->first << "]:";
      for (int j = 0; j < 2; j++)
      {
        std::cout << ' ' << (j == KNOWN ? 'K' : 'W') << ":{";
        if (i->second._token[j] >= 0)
        {
          assert(i->second._sign[j] == 1 || i->second._sign[j] == -1);
          if (i->second._sign[j] < 0) 
             std::cout << '-';
          std::cout << 'T' << i->second._token[j];
          if (i->second._val[j] > 0)
             std::cout << '+' << i->second._val[j];
          else if (i->second._val[j] < 0)
             std::cout << i->second._val[j];
        } else
        {
          assert(i->second._token[j] == -1 && i->second._sign[j] == 0);
          std::cout << i->second._val[j];
        }
        std::cout << '}';
      }
    }
  }

  // Dump the known value of this cell out to instruction stream
  void dump(int cell, int cell_ofs, std::vector<Instruction> *p_insns_out)
  {
    std::map<int, CellRecord>::iterator i = cells.find(cell);
    if (i != cells.end())
       _dump(&i->second, cell_ofs, p_insns_out);
  }

  // Dump the known value of all cells out to instruction stream
  void dump_all(int cell_ofs, std::vector<Instruction> *p_insns_out)
  {
    for (std::map<int, CellRecord>::iterator i = cells.begin();
         i != cells.end(); ++i)
       _dump(&i->second, cell_ofs, p_insns_out);
  }

  // Invalidate this cell; it's about to be written over.
  void invalidate(int cell, int cell_ofs, std::vector<Instruction> *p_insns_out)
  {
    std::map<int, CellRecord>::iterator i = cells.find(cell);
    if (i == cells.end())
        return;
    prep(&i->second, cell_ofs, p_insns_out);
    remove_list(&i->second, KNOWN);
    remove_list(&i->second, WRITTEN);
    cells.erase(i);
  }

  // We always call dump_all() before this, which therefore has nothing to do.
  void invalidate_all(int cell_ofs, std::vector<Instruction> *p_insns_out)
  {
    head_cell[KNOWN].clear();
    head_cell[WRITTEN].clear();
    cells.clear();
  }

  // Set known value equal to sign * token + v.
  void set(int cell, int sign, const Integer &v, int token)
  {
    std::map<int, CellRecord>::iterator i = cells.find(cell);
    if (i == cells.end())
    {
      CellRecord new_c;

      assert(head_cell[WRITTEN].size() == head_cell[KNOWN].size());
      new_c._cell = cell;
      new_c._token[KNOWN] = token;
      new_c._val[KNOWN] = v;
      new_c._sign[KNOWN] = sign;
      new_c._token[WRITTEN] = head_cell[WRITTEN].size();
      new_c._val[WRITTEN] = 0;
      new_c._sign[WRITTEN] = 1;
      new_c._flag = false;
      new_c._next[KNOWN] = new_c._last[KNOWN] = NULL;
      new_c._next[WRITTEN] = new_c._last[WRITTEN] = NULL;
      head_cell[WRITTEN].push_back(NULL);
      head_cell[KNOWN].push_back(NULL);
      i = cells.insert(std::pair<int, CellRecord>(cell, new_c)).first;
      add_list(&i->second, KNOWN);
      add_list(&i->second, WRITTEN);
    } else
    {
      remove_list(&i->second, KNOWN);
      i->second._token[KNOWN] = token;
      i->second._val[KNOWN] = v;
      i->second._sign[KNOWN] = sign;
      add_list(&i->second, KNOWN);
    }
  }

  // Get token for unknown cell
  int val_to_token(int cell)
  {
    assert(cells.find(cell) == cells.end());
    CellRecord new_c;

    assert(head_cell[WRITTEN].size() == head_cell[KNOWN].size());
    new_c._cell = cell;
    int token = head_cell[WRITTEN].size();
    new_c._token[KNOWN] = new_c._token[WRITTEN] = token;
    new_c._val[KNOWN] = new_c._val[WRITTEN] = 0;
    new_c._sign[KNOWN] = new_c._sign[WRITTEN] = 1;
    new_c._flag = false;
    new_c._next[KNOWN] = new_c._last[KNOWN] = NULL;
    new_c._next[WRITTEN] = new_c._last[WRITTEN] = NULL;
    head_cell[WRITTEN].push_back(NULL);
    head_cell[KNOWN].push_back(NULL);
    std::map<int, CellRecord>::iterator i = 
                cells.insert(std::pair<int, CellRecord>(cell, new_c)).first;
    add_list(&i->second, KNOWN);
    add_list(&i->second, WRITTEN);
    return(token);
  }

  // Set known value equal to constant.
  void set_constant(int cell, const Integer &v)
  {
    set(cell, 0, v, -1);
  }

  // Remove token TOKEN by setting it equal to V.
  void flush_token(int token, const Integer &v)
  {
    for (int j = 0; j < 2; j++)
    {
      for (CellRecord *c = head_cell[j][token]; c != NULL; )
      {
        CellRecord *c_next = c->_next[j];
        assert(c->_token[j] == token);
        c->_val[j] = remap_cell(c->_val[j] + c->_sign[j] * v, _low, _high);
        c->_sign[j] = 0;
        c->_token[j] = -1;
        c->_next[j] = c->_last[j] = NULL;
        c = c_next;
      }
      head_cell[j][token] = NULL;
    }
  }

  // We have deduced that the current contents of CELL equals V.
  // Furthermore, this fact has been written out.
  void assert_constant(int cell, const Integer &v)
  {
    std::map<int, CellRecord>::iterator i = cells.find(cell);
    if (i == cells.end())
    {
      // We don't know it, so create it and make it equal to v.
      set(cell, 0, v, -1);
      i = cells.find(cell);
      assert(i != cells.end());
      remove_list(&i->second, WRITTEN);
      i->second._token[WRITTEN] = -1;
      i->second._val[WRITTEN] = v;
      i->second._sign[WRITTEN] = 0;
      add_list(&i->second, WRITTEN);
    } else
    {
      for (int j = 0; j < 2; j++)
      if (i->second._sign[j] != 0)
      {
        assert(i->second._sign[j] == 1 || i->second._sign[j] == -1);
        int token = i->second._token[j];
        assert(token >= 0);
        // We can remove this token completely.
        Integer token_value = i->second._sign[j] * (v - i->second._val[j]);
        flush_token(token, token_value);
      }
    }

    for (int j = 0; j < 2; j++)
    {
      assert(i->second._token[j] == -1);
      assert(i->second._val[j] == v);
      assert(i->second._sign[j] == 0);
    }
  }
};

//
// Remove constants.  Assumes cell and tape ops combinable.
// Also collapses increments.
//
// NB: This may delay writes.  After constant squashing, if we have a reference 
// to T[p+q] in the insn stream when the tape position is x, the tape position
// x+q may occur either before or after x in the BF program.
//
void constant_remove(bool print_debug_info,
                     std::vector<Instruction> *p_insns, 
                     const End &lo, const End &hi,
                     const EOFMethod &eof_method)
{
  int cell_ofs = 0;
  int sign, token;
  bool known;
  std::vector<bool> discarded(p_insns->size(), false);
  Integer val;
  CellsKnown cells_known(lo, hi, print_debug_info);
  std::vector<Instruction> insns_out;

  if (print_debug_info)
     std::cout << "Removing constants:\n\n";
  for (std::vector<Instruction>::const_iterator i = p_insns->begin();
       i != p_insns->end();
       ++i)
  if (!discarded[i - p_insns->begin()])
  {
    if (print_debug_info)
    {
      std::cout << "Known: ";
      cells_known.debug_print();
      std::cout << '\n';
      std::cout << "   Processing: " << *i << '\n';
    }
    // Must dump if we may abort.
    if ((i->type() == IT_Input && 
         (eof_method == EOF_halt || eof_method == EOF_abort)) ||
        i->is_abort())
        cells_known.dump_all(cell_ofs, &insns_out);

    switch (i->type())
    {
      case IT_Clock:
           insns_out.push_back(*i);
           if (print_debug_info)
              std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           break;

      case IT_Start:
      case IT_EndIf:
           // Control flow confluence (and possibly branch.)
           // Must dump all before invalidating all.
           cells_known.dump_all(cell_ofs, &insns_out);
           cells_known.invalidate_all(cell_ofs, &insns_out);
           cell_ofs = 0;
           insns_out.push_back(*i);
           if (print_debug_info)
              std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           break;

      case IT_Breakpoint:
      case IT_Halt:
           // Control flow may stop.  Must dump all.
           cells_known.dump_all(cell_ofs, &insns_out);
           insns_out.push_back(*i);
           if (print_debug_info)
              std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           break;

      case IT_Input:
           cells_known.invalidate(cell_ofs + i->cell(), cell_ofs, &insns_out);
           insns_out.push_back(*i);
           if (print_debug_info)
              std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           break;

      case IT_Output:
           cells_known.dump(cell_ofs + i->cell(), cell_ofs, &insns_out);
           insns_out.push_back(*i);
           if (print_debug_info)
              std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           break;

      case IT_While:
           if (cells_known.is_known_constant(cell_ofs + i->cell(), &val)
               && val == 0)
           {
             // Skip loop; no control flow alteration
             size_t while_count = 1;
             while (while_count != 0)
             {
               ++i;
               if (i->type() == IT_While)
                  while_count++;
               else if (i->type() == IT_EndWhile)
                  while_count--;
             }
           } else
           {
             // Control flow confluence (and branch.)
             // Must dump all before invalidating all.
             cells_known.dump_all(cell_ofs, &insns_out);
             cells_known.invalidate_all(cell_ofs, &insns_out);
             cell_ofs = 0;
             insns_out.push_back(*i);
             if (print_debug_info)
                std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           } 
           break;

      case IT_EndWhile:
           if (cells_known.is_known_constant(cell_ofs + i->cell(), &val)
               && val != 0)
              // Infinite loop.
              insns_out.push_back(Instruction(
                  IT_Abort, i->location(), i->cell()));
           // Control flow confluence (and branch if no infinite loop.)
           // Must dump all before invalidating all.
           cells_known.dump_all(cell_ofs, &insns_out);
           cells_known.invalidate_all(cell_ofs, &insns_out);
           cell_ofs = 0;
           insns_out.push_back(*i);
           if (print_debug_info)
              std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           // We can infer that the loop index value is zero.
           cells_known.assert_constant(i->cell(), 0);
           break;

      case IT_If:
           if (cells_known.is_known_constant(cell_ofs + i->cell(), &val))
           {
             std::vector<Instruction>::const_iterator j = i;
             size_t if_count = 1;
             while (if_count != 0)
             {
               ++j;
               if (j->type() == IT_If)
                  if_count++;
               else if (j->type() == IT_EndIf)
                  if_count--;
             }
             if (val == 0)
                // Skip if; no control flow alteration.
                i = j;
             else
                // If always taken; no control flow alteration.
                discarded[j - p_insns->begin()] = true;
           }
           else
           {
             // Control flow branch.
             cells_known.dump_all(cell_ofs, &insns_out);
             insns_out.push_back(*i);
             if (print_debug_info)
                std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           }
           break;

      case IT_AddPos:
           cell_ofs += i->cell();
           insns_out.push_back(*i);
           if (print_debug_info)
              std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           break;

      case IT_CheckGE:
      case IT_CheckLE:
      case IT_CheckEQ:
      case IT_CheckDiv:
           if (i->type() == IT_CheckDiv)
              assert(i->value() > 0);
           else
              assert(is_within(i->value(), lo.bound(), hi.bound()));
           if (cells_known.is_known_constant(cell_ofs + i->cell(), &val))
           {
             bool ok;

             switch (i->type())
             {
               case IT_CheckGE: 
                    ok = (val >= i->value()); 
                    break;

               case IT_CheckLE: 
                    ok = (val <= i->value()); 
                    break;

               case IT_CheckEQ: 
                    ok = (val == i->value()); 
                    break;

               case IT_CheckDiv: 
                    ok = (val % i->value()) == 0; 
                    break;

               default:
                    assert(0);
                    break;
             }
             if (!ok)
             {
               insns_out.push_back(Instruction(
                    IT_Abort, i->location(), i->cell()));
               if (print_debug_info)
                   std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
             }
           } else
           {
             cells_known.dump(cell_ofs + i->cell(), cell_ofs, &insns_out);
             insns_out.push_back(*i);
             if (print_debug_info)
                 std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
             if (i->type() == IT_CheckEQ)
                cells_known.assert_constant(cell_ofs + i->cell(), i->value());
           }
           break;

      case IT_Abort:
           insns_out.push_back(*i);
           if (print_debug_info)
               std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           break;

      case IT_Max:
           assert(is_within(i->value(), lo.bound(), hi.bound()));
           if (cells_known.is_known_constant(cell_ofs + i->cell(), &val))
               cells_known.set_constant(cell_ofs + i->cell(), 
                              (val > i->value() ? val : i->value()));
           else
           {
             cells_known.dump(cell_ofs + i->cell(), cell_ofs, &insns_out);
             cells_known.invalidate(cell_ofs + i->cell(), cell_ofs, &insns_out);
             insns_out.push_back(*i);
             if (print_debug_info)
                 std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           }
           break;

      case IT_Min:
           assert(is_within(i->value(), lo.bound(), hi.bound()));
           if (cells_known.is_known_constant(cell_ofs + i->cell(), &val))
               cells_known.set_constant(cell_ofs + i->cell(), 
                              (val < i->value() ? val : i->value()));
           else
           {
             cells_known.dump(cell_ofs + i->cell(), cell_ofs, &insns_out);
             cells_known.invalidate(cell_ofs + i->cell(), cell_ofs, &insns_out);
             insns_out.push_back(*i);
             if (print_debug_info)
                 std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           }
           break;

      case IT_Sub:
           if (cells_known.is_known(cell_ofs + i->cell(), &sign, &val, &token))
           {
             Integer new_val = remap_cell(i->value() - val, lo, hi);
             if (token < 0)
               cells_known.set_constant(cell_ofs + i->cell(), new_val);
             else
               cells_known.set(cell_ofs + i->cell(), -sign, new_val, token);
           }
           else
           {
             // Cell not present; no need to dump or invalidate
             insns_out.push_back(*i);
             if (print_debug_info)
                 std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           }
           break;

      case IT_Quotient:
           assert(i->value() > 0);
           if (i->value() == 1)
              ;
           else if (cells_known.is_known_constant(cell_ofs + i->cell(), &val))
           {
             Integer new_val = val / i->value();
             if (val > 0 && (val % i->value() != 0))
                // Round away from 0
                new_val++;
             assert(is_within(new_val, lo.bound(), hi.bound()));
             cells_known.set_constant(cell_ofs + i->cell(), new_val);
           } else
           {
             cells_known.dump(cell_ofs + i->cell(), cell_ofs, &insns_out);
             cells_known.invalidate(cell_ofs + i->cell(), cell_ofs, &insns_out);
             insns_out.push_back(*i);
             if (print_debug_info)
                 std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           }
           break;

      case IT_ExactQuotient:
           assert(i->value() > 0);
           if (i->value() == 1)
              ;
           else if (cells_known.is_known_constant(cell_ofs + i->cell(), &val))
           {
             assert(val % i->value() == 0);
             Integer new_val = val / i->value();
             assert(is_within(new_val, lo.bound(), hi.bound()));
             cells_known.set_constant(cell_ofs + i->cell(), new_val);
           } else
           {
             cells_known.dump(cell_ofs + i->cell(), cell_ofs, &insns_out);
             cells_known.invalidate(cell_ofs + i->cell(), cell_ofs, &insns_out);
             insns_out.push_back(*i);
             if (print_debug_info)
                 std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           }
           break;

      case IT_Remainder:
           assert(i->value() > 0);
           if (i->value() == hi.value() - lo.value() + 1)
               ;
           else if (i->value() == 1) 
               cells_known.set_constant(cell_ofs + i->cell(), 0);
           else if (cells_known.is_known_constant(cell_ofs + i->cell(), &val))
           {
             Integer new_val = remap_cell(val % i->value(), lo, hi);
             cells_known.set_constant(cell_ofs + i->cell(), new_val);
           } else
           {
             cells_known.dump(cell_ofs + i->cell(), cell_ofs, &insns_out);
             cells_known.invalidate(cell_ofs + i->cell(), cell_ofs, &insns_out);
             insns_out.push_back(*i);
             if (print_debug_info)
                 std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
           }
           break;

      case IT_Set:
      case IT_Add:
           {
             int out_len = 0, out_cells[MAX_INSTR_LENGTH];
             int tokens[MAX_INSTR_LENGTH], signs[MAX_INSTR_LENGTH];
             Integer vals[MAX_INSTR_LENGTH];
             Integer temp;
             bool knowns[MAX_INSTR_LENGTH];
             Integer v = remap_cell(i->value(), lo, hi);
             int cell = cell_ofs + i->cell();
             known = cells_known.is_known(cell, &sign, &val, &token);

             for (int k = 0; k < i->length(); k++)
             {
               int sign, token;
               Integer val;

               if (cells_known.is_known(cell_ofs + i->cells(k),
                                        &sign, &val, &token))
               {
                 if (token < 0)
                    v = remap_cell(v * val, lo, hi);
                 else
                 {
                   knowns[out_len] = true;
                   tokens[out_len] = token;
                   vals[out_len] = val;
                   signs[out_len] = sign;
                   out_cells[out_len++] = cell_ofs + i->cells(k);
                 }
               } else
               {
                 knowns[out_len] = false;
                 out_cells[out_len++] = cell_ofs + i->cells(k);
               }
             }
             if (v == 0)
             {
               if (i->type() == IT_Set)
                  cells_known.set_constant(cell, 0);
               else
                  ;
             } else 
             if (i->type() == IT_Set && out_len == 0)
             // cell := v
                cells_known.set_constant(cell, v);
             else if (i->type() == IT_Add && out_len == 0)
             // cell := (sign*token + val) + v
             {
               if (!known)
               {
                 sign = 1;
                 token = cells_known.val_to_token(cell_ofs + i->cell());
                 val = 0;
               }
               val = remap_cell(val + v, lo, hi);
               cells_known.set(cell_ofs + i->cell(), sign, val, token);
             } 
             else if (i->type() == IT_Set && out_len == 1 && 
                      (v == remap_cell(1, lo, hi) || 
                       v == remap_cell(-1, lo, hi)))
             // cell := v*(signs[0]*tokens[0]+vals[0])
             //       need v=1 or v=-1
             {
               if (!knowns[0])
               {
                 signs[0] = 1;
                 tokens[0] = cells_known.val_to_token(out_cells[0]);
                 vals[0] = 0;
               }
               if (v == remap_cell(-1, lo, hi))
                  signs[0] = -signs[0];
               val = remap_cell(vals[0] * v, lo, hi);
               cells_known.set(cell_ofs + i->cell(), signs[0], val, tokens[0]);
             }
             else if (i->type() == IT_Add && out_len == 1 &&
                      known && sign == 0 && 
                      (v == remap_cell(1, lo, hi) || 
                       v == remap_cell(-1, lo, hi)))
             // cell := val + v*(signs[0]*tokens[0] + vals[0]);
             //     need v = 1 or v = -1
             {
               if (!knowns[0])
               {
                 signs[0] = 1;
                 tokens[0] = cells_known.val_to_token(out_cells[0]);
                 vals[0] = 0;
               }
               if (v == remap_cell(-1, lo, hi))
                  signs[0] = -signs[0];
               val = remap_cell(v * vals[0] + val, lo, hi);
               assert(token == -1);
               cells_known.set(cell_ofs + i->cell(), signs[0], val, tokens[0]);
             }
             else if (i->type() == IT_Add && out_len == 1 &&
                      known && knowns[0] && token == tokens[0] &&
                      ((temp = remap_cell(sign + v * signs[0], lo, hi)) == 
                               remap_cell(1, lo, hi) ||
                       temp == 0 ||
                       temp == remap_cell(-1, lo, hi)))
             // cell := val + sign*token + v*(signs[0]*tokens[0] + vals[0])
             //           Tokens must be the same, |sign + v*signs[0]| <= 1
             {
               if (temp == remap_cell(1, lo, hi))
                  sign = 1;
               else if (temp == 0)
                  sign = 0;
               else
                  sign = -1;
               val = remap_cell(val + v * vals[0], lo, hi);
               if (sign == 0)
                  token = -1;
               cells_known.set(cell_ofs + i->cell(), sign, val, token);
             }
             else if (i->type() == IT_Add && out_len == 1 &&
                      cell_ofs + i->cell() == out_cells[0] &&
                      (v == remap_cell(-1, lo, hi) || 
                       v == remap_cell(-2, lo, hi)))
             // (same as above case but tokens unknown)
             {
               // (known case will be handled above)
               assert(!known);
               assert(!knowns[0]);
               if (v == remap_cell(-1, lo, hi))
                   cells_known.set_constant(cell_ofs + i->cell(), 0); 
               else
               {
                 int token = cells_known.val_to_token(cell_ofs + i->cell());
                 cells_known.set(cell_ofs + i->cell(), -1, 0, token);
               }
             } else
             // We are not able to infer the value of the desination cell.
             // Emit instruction.
             {
               InstructionType t = i->type();

               for (int k = 0; k < out_len; k++)
                   cells_known.dump(out_cells[k], cell_ofs, &insns_out);
               if (t == IT_Add && known && sign == 0 & val == 0)
                  // Add to cell known to be 0 ==> Set.
                  t = IT_Set;
               if (t == IT_Add)
                   cells_known.dump(cell, cell_ofs, &insns_out);
               cells_known.invalidate(cell, cell_ofs, &insns_out);
               insns_out.push_back(Instruction(
                            t, -1, i->cell(), v, out_len, out_cells));
               if (print_debug_info)
                   std::cout << "Out: " << *(insns_out.end() - 1) << '\n';
             }
           }
           break;

      case IT_Permute:
           {
             int cells[MAX_INSTR_LENGTH];
             int tokens[MAX_INSTR_LENGTH], signs[MAX_INSTR_LENGTH];
             Integer vals[MAX_INSTR_LENGTH];
             bool knowns[MAX_INSTR_LENGTH];
             int k, l;

             for (k = 0; k < i->length(); k++)
             {
               cells[k] = i->cells(k) + cell_ofs;
               knowns[k] = cells_known.is_known(cells[k], 
                               &signs[k], &vals[k], &tokens[k]);
               if (!knowns[k])
               {
                 signs[k] = 1;
                 tokens[k] = cells_known.val_to_token(cells[k]);
                 vals[k] = 0;
               }
             }
             for (k = 0, l = 1; k < i->length(); k++)
             {
               cells_known.set(cells[k], signs[l], vals[l], tokens[l]);
               if (++l == i->length())
                  l = 0;
             }
           }
           break;

      case IT_And:
      case IT_CheckAnd:
      case IT_Add0:
      case IT_Set0:
      case IT_Add1v1:
      case IT_Set1v1:
      case IT_Add1:
      case IT_Set1:
      case IT_Inc:
      case IT_Dec:
           assert(0);    // Should not appear here
           break;
    }
  }

  *p_insns = insns_out;
}

// Tracks dead cells
class SignificantCells
{
  std::set<int> cells;
  bool black_holed;

public:

  SignificantCells() : cells(), black_holed(false) { }
  void all_used() 
  { 
    black_holed = false; 
    cells.clear(); 
  }
  void all_unused() 
  { 
    black_holed = true; 
    cells.clear(); 
  }
  void add_used(int c) 
  { 
    if (black_holed)
       cells.insert(c);
    else
       cells.erase(c);
  }
  void add_unused(int c) 
  { 
    if (black_holed)
       cells.erase(c);
    else
       cells.insert(c);
  }
  bool is_used(int c) const 
  { 
    return black_holed ^ (cells.find(c) == cells.end());
  }
};

//
// Eliminate dead code, e.g., first assignment in ... X:=5 X:=4 ...
// Assumes cell and tape ops combinable.
// Returns TRUE if any code removed.
//
// NB: Data flow analysis is not extended past control flow branches.
//
bool dead_code_remove(std::vector<Instruction> *p_insns, 
                      const EOFMethod &eof_method)
{
  std::vector<bool> is_dead(p_insns->size(), false);
  SignificantCells s;
  int cell_ofs = 0;
  size_t orig_size = p_insns->size();

  for (size_t ii = orig_size; ii != 0; )
  {
    ii--;
    std::vector<Instruction>::const_iterator i = p_insns->begin() + ii;

    switch (i->type())
    {
      case IT_Halt:
      case IT_Breakpoint:
      case IT_EndWhile:
      case IT_If:
      case IT_While:
           // Before a control flow branch or halt, everything is relevant.
           s.all_used();
           cell_ofs = 0;
           break;

      case IT_Clock:
      case IT_EndIf:
      case IT_Start:
           break;

      case IT_Output:
           s.add_used(cell_ofs + i->cell());
           break;

      case IT_Input:
           s.add_unused(cell_ofs + i->cell());
           break;

      case IT_AddPos:
           cell_ofs -= i->cell();
           break;

      case IT_CheckEQ:
      case IT_CheckGE:
      case IT_CheckLE:
      case IT_CheckDiv:
           s.add_used(cell_ofs + i->cell());
           break;

      case IT_Abort:
           s.all_unused();
           break;

      case IT_Max:
      case IT_Min:
      case IT_Sub:
      case IT_Quotient:
      case IT_ExactQuotient:
      case IT_Remainder:
           if (!s.is_used(cell_ofs + i->cell()))
              is_dead[ii] = true;
           break;

      case IT_Set:
           if (!s.is_used(cell_ofs + i->cell()))
              is_dead[ii] = true;
           else
           {
             s.add_unused(cell_ofs + i->cell());
             for (int j = 0; j < i->length(); j++)
                 s.add_used(cell_ofs + i->cells(j));
           }
           break;

      case IT_Add:
           if (!s.is_used(cell_ofs + i->cell()))
              is_dead[ii] = true;
           else
           {
             for (int j = 0; j < i->length(); j++)
                 s.add_used(cell_ofs + i->cells(j));
           }
           break;

      case IT_Permute:
           {
             bool is_used[MAX_INSTR_LENGTH];
             int j, k;

             for (j = 0; j < i->length(); j++)
                 is_used[j] = s.is_used(cell_ofs + i->cells(j));
             for (j = 0, k = 1; j < i->length(); j++)
             {
               if (is_used[k])
                  s.add_used(cell_ofs + i->cells(j));
               else
                  s.add_unused(cell_ofs + i->cells(j));
               if (++k == i->length())
                  k = 0;
             }
           }
           break;

      case IT_And:
      case IT_CheckAnd:
      case IT_Add0:
      case IT_Set0:
      case IT_Add1v1:
      case IT_Set1v1:
      case IT_Add1:
      case IT_Set1:
      case IT_Inc:
      case IT_Dec:
           assert(0);    // Should not appear here
           break;
    }

    // If we may abort, everything is relevant.
    if ((i->type() == IT_Input && 
         (eof_method == EOF_halt || eof_method == EOF_abort)) ||
        i->is_abort())
    {
      s.all_used();
      cell_ofs = 0;
    }
  }

  std::vector<Instruction> insns_out;
  bool white_holed = false;

  for (std::vector<Instruction>::const_iterator i = p_insns->begin();
       i != p_insns->end();
       ++i)
  if (!is_dead[i - p_insns->begin()])
  {
    if (white_holed)
    {
      // It is possible to white hole many more instructions here.
      if (i->type() == IT_If || i->type() == IT_While ||
          i->type() == IT_EndIf || i->type() == IT_EndWhile ||
          i->type() == IT_Start)
      {
        insns_out.push_back(*i);
        white_holed = false;
      }
    } 
    else if (i->type() == IT_Abort)
    {
      insns_out.push_back(*i);
      white_holed = true;
    }
    else if (i->type() == IT_EndIf)
    {
      assert(!insns_out.empty());
      if ((insns_out.end() - 1)->type() == IT_If)
         insns_out.erase(insns_out.end() - 1);
      else
         insns_out.push_back(*i);
    }
    else insns_out.push_back(*i);
  }

  *p_insns = insns_out;

  return p_insns->size() < orig_size;
}

//
// Compute parameters for determining the number of times B must be 
// added to a number to reach 0 and put them into *P_C, *P_D, and *P_E.
// LOW and HIGH are the bounds for the target number, which should be 
// both finite & wraparound.
//
void compute_multiplier_parameters(Integer *p_c, Integer *p_d, Integer *p_e,
                                   Integer b, const End &lo, const End &hi)
{
  Integer s, t, g;

  assert(!lo.is_infinite() && !hi.is_infinite());
  g = gcd_ext(hi.value() - lo.value() + 1, b, &s, &t);
  //
  // Ans. will be infinite if a % g != 0,
  // or (-(a/g) * t) % ((hi.value() - lo.value() + 1) / g) o.w.
  // Note that hi.value() - lo.value() + 1 > 0, so g is always positive.
  //
  if (p_c)
  {
    assert(g > 0);
    *p_c = g;
  }
  if (p_d)
     *p_d = (-t) % ((hi.value() - lo.value() + 1) / g);
  if (p_e)
  {
    *p_e = (hi.value() - lo.value() + 1) / g;
    assert(*p_e > 0);
  }
} 

//
// Squash inner loops into arithmetic operations.
// Assumes cell ops, tape ops combinable.
// Returns TRUE if any loops squashed, FALSE o.w.
//
// We divide tape cells into (1) output_i cells (which suffer an idempotent
// operation during the loop), (2) output_a cells (which suffer an addition
// during the loop); (3) input (which are constant durng the loop); and
// (4) the loop index.   These sets must be disjoint.
//
// We handle loops containing the following operations:
//
// (this control structure does not affect whether the loop is squashable)
// If input; Endif; 
//
// (any sequence of the following operations is idempotent)
//    Abort (non-abortive); Check{EQ,GE,LE,Div} input (non-abortive); 
//    output_i = f(input); output_i = {max, min}(output_i, const);
//
// (any sequence of the following operations is additive)
//    output_a += f(input).
//
// (and the loop index):
//    loop_index = const,       loop_index += const
//    (must occur outside of If...Endif pairs.)
//
// We could squash more loops which execute either 0 or 1 times, but
// there seems no performance advantage in doing so.
//
bool squash_loops(bool print_debug_info, 
                  std::vector<Instruction> *p_insns, 
                  const End &lo, const End &hi)
{
  std::vector<Instruction> insns_out;
  std::vector<Instruction>::const_iterator loop_start;
  std::set<int> idempotent_output_cells, additive_output_cells, input_cells;
  size_t loop_start_out;
  bool possible_squash = false;
  int loop_index;
  size_t if_depth = 0;
  Integer counter_movement;
  bool counter_movement_fixed;
  bool have_idempotent;
  bool squashed_anything = false;

  if (print_debug_info)
     std::cout << "Squashing loops:\n\n";

  for (std::vector<Instruction>::const_iterator i = p_insns->begin();
       i != p_insns->end();
       ++i)
  {
    insns_out.push_back(*i);

    switch (i->type())
    {
      case IT_While:
           loop_start = i;
           loop_start_out = insns_out.size() - 1;
           possible_squash = true;
           counter_movement = 0;
           counter_movement_fixed = false;
           if_depth = 0;
           loop_index = i->cell();
           input_cells.clear();
           idempotent_output_cells.clear();
           additive_output_cells.clear();
           have_idempotent = false;
           break;

      case IT_Clock:
      case IT_Start:
      case IT_Breakpoint:
      case IT_Halt:
      case IT_Input:
      case IT_Output:
      case IT_AddPos:
      case IT_Sub:
      case IT_Quotient:
      case IT_ExactQuotient:
      case IT_Remainder:
      case IT_Permute:
           possible_squash = false;
           break;

      case IT_If:
           if (i->cell() == loop_index ||
               additive_output_cells.count(i->cell()) ||
               idempotent_output_cells.count(i->cell()))
              possible_squash = false;
           else
              input_cells.insert(i->cell());
           if_depth++;
           break;

      case IT_EndIf:
           assert(if_depth-- != 0);
           break;

      case IT_Max:
      case IT_Min:
           if (possible_squash)
           {
             have_idempotent = true;
             if (i->cell() == loop_index ||
                 input_cells.count(i->cell()) || 
                 additive_output_cells.count(i->cell()))
                possible_squash = false;
             else
                idempotent_output_cells.insert(i->cell());
           }
           break;

      case IT_Abort:
           if (i->is_abort())
              possible_squash = false;
           have_idempotent = true;
           break;

      case IT_CheckEQ:
      case IT_CheckLE:
      case IT_CheckGE:
      case IT_CheckDiv:
           if (i->is_abort())
              possible_squash = false;
           if (possible_squash)
           {
             have_idempotent = true;
             if (i->cell() == loop_index ||
                 idempotent_output_cells.count(i->cell()) || 
                 additive_output_cells.count(i->cell()))
                possible_squash = false;
             else
                input_cells.insert(i->cell());
           }
           break;


      case IT_Set:
           if (possible_squash)
           {
             if (i->cell() == loop_index)
             {
               if (if_depth != 0 || i->length() > 0)
                  possible_squash = false;
               else
               {
                 counter_movement_fixed = true;
                 counter_movement = i->value();
               }
             } else if (input_cells.count(i->cell()) ||
                        additive_output_cells.count(i->cell()))
               possible_squash = false;
             else
             {
               idempotent_output_cells.insert(i->cell());
               for (int j = 0; j < i->length(); j++)
                   if (i->cells(j) == loop_index ||
                       additive_output_cells.count(i->cells(j)) ||
                       idempotent_output_cells.count(i->cells(j)))
                   {
                     possible_squash = false;
                     break;
                   } else
                     input_cells.insert(i->cells(j));
               have_idempotent = true;
             }
           }
           break;

      case IT_Add:
           if (possible_squash)
           {
             if (i->cell() == loop_index)
             {
               if (if_depth != 0 || i->length() > 0)
                  possible_squash = false;
               else
                  counter_movement = 
                          remap_cell(counter_movement + i->value(), lo, hi);
             } else if (input_cells.count(i->cell()) ||
                        idempotent_output_cells.count(i->cell()) ||
                        i->length() == MAX_INSTR_LENGTH)
               possible_squash = false;
             else
             {
               additive_output_cells.insert(i->cell());
               for (int j = 0; j < i->length(); j++)
                   if (i->cells(j) == loop_index ||
                       additive_output_cells.count(i->cells(j)) ||
                       idempotent_output_cells.count(i->cells(j)))
                   {
                     possible_squash = false;
                     break;
                   } else
                     input_cells.insert(i->cells(j));
             }
           }
           break;

      case IT_EndWhile:
           if (!possible_squash)
               break;

           assert(if_depth == 0);
           possible_squash = false;
           squashed_anything = true;
           //
           // We are going to squash the loop.
           //
           if (print_debug_info)
           {
             std::cout << "Input loop (index T[p + " << loop_index << "]):\n";
             for (std::vector<Instruction>::const_iterator 
                             j = loop_start + 1; j != i; ++j)
                 std::cout << "    " << *j << '\n';
           }
           insns_out.erase(insns_out.begin() + loop_start_out, 
                           insns_out.end());
           if ((counter_movement_fixed && counter_movement != 0) ||
               (!counter_movement_fixed && counter_movement == 0))
           {
             // This will loop forever if ever executed.
             insns_out.push_back(Instruction(
                   IT_CheckEQ, loop_start->location(), loop_index, 0));
           } else if (counter_movement_fixed)
           {
             assert(counter_movement == 0);
             // The loop sets the index to 0, so it is executed 0 or 1 times.
             insns_out.push_back(Instruction(IT_If, -1, loop_index));
             for (std::vector<Instruction>::const_iterator 
                             j = loop_start + 1; j != i; ++j)
                 if ((j->type() != IT_Set && j->type() != IT_Add) ||
                     j->cell() != loop_index)
                 insns_out.push_back(*j);
             insns_out.push_back(Instruction(IT_EndIf, -1));
             insns_out.push_back(Instruction(IT_Set, -1, loop_index, 0));
           } else 
           {
             int loop_sign;
             bool max_executions_1 = false;
             Integer counter_movement_abs;

             assert(!counter_movement_fixed && counter_movement != 0);

             if (lo.is_infinite() || hi.is_infinite())
             {
               if (counter_movement > 0)
               {
                 insns_out.push_back(
                      Instruction(IT_CheckLE, loop_start->location(), 
                                  loop_index, 0));
                 counter_movement_abs = counter_movement;
                 loop_sign = -1;
               }
               else
               {
                 insns_out.push_back(
                       Instruction(IT_CheckGE, loop_start->location(), 
                                   loop_index, 0));
                 counter_movement_abs = -counter_movement;
                 loop_sign = 1;
               }

               if (counter_movement_abs != 1)
               {
                  insns_out.push_back(Instruction(IT_CheckDiv, 
                                loop_start->location(), loop_index, 
                                counter_movement_abs));

                  // If no additive ops, only need to know whether loop
                  // executes 0, finitely, or infinitely often, so skup
                  // this.
                  if (!additive_output_cells.empty())
                      insns_out.push_back(Instruction(IT_ExactQuotient, 
                                -1, loop_index, counter_movement_abs));
               }
             } else
             {
               Integer c, d, e;

               loop_sign = 1;
               compute_multiplier_parameters(&c, &d, &e, 
                                             counter_movement, lo, hi);

               // d can't be zero unless counter_movement remaps to 0.
               // e is the maximum # of times the loop will be executed 
               //      (plus 1.)  It can't be 1 unless counter_movement
               //      remaps to 0.
               assert(d > 0 && c > 0 && e >= 2);
               if (c != 1)
               {
                  insns_out.push_back(Instruction(IT_CheckDiv, 
                                loop_start->location(), loop_index, c));
               }

               // If no additive instrs, only need to know whether 
               // loop count is zero, finite nonzero, or infinite, so
               // skip this computaton.
               if (!additive_output_cells.empty())
               {
                 if (e == 2)
                 {
                   // Loop executes 0 or 1 times.  We may therefore
                   // treat all operations as idempotent.
                   max_executions_1 = true;
                   have_idempotent = true;
                 } else
                 {
                   if (c != 1)
                       insns_out.push_back(Instruction(IT_ExactQuotient,
                                                  -1, loop_index, c));
  
                   if (e == hi.value() - lo.value() + 1 &&
                       d > hi.value() - lo.value() + 1 - d)
                   {
                     // Reduce d.  This is permissible because whether the
                     // computed number of executions of the loop is zero
                     // will not change and the results computed for
                     // additive operations will not change.
                     d = hi.value() - lo.value() + 1 - d;
                     loop_sign = -1;
                   }
                   if (d != 1)
                   {
                     d = remap_cell(d, lo, hi);
                     insns_out.push_back(Instruction(IT_Set, 
                                    -1, loop_index, d, 1, &loop_index));
                   }
                   if (e != hi.value() - lo.value() + 1)
                       insns_out.push_back(Instruction(IT_Remainder, 
                                           -1, loop_index, e));
                 }
               }
             }

             //
             // All operations inside the loop are either idempotent 
             // or additive.  Futhermore, the additive and idempotent
             // operators commute.
             //
             if (have_idempotent)
             {
               insns_out.push_back(Instruction(IT_If, -1, loop_index));
               for (std::vector<Instruction>::const_iterator 
                                        j = loop_start + 1; j != i; ++j)
                   if ((j->type() == IT_Add && (!max_executions_1 || 
                                                j->cell() == loop_index)) ||
                       (j->type() == IT_Set && j->cell() == loop_index))
                       ;
                   else
                       insns_out.push_back(*j);
               insns_out.push_back(Instruction(IT_EndIf, -1));
             }
             if (!max_executions_1 && !additive_output_cells.empty())
             {
               //
               // The loop will be executed T[loop_index]*loop_sign times
               // (possibly mod H-L+1).
               //
               for (std::vector<Instruction>::const_iterator 
                                        j = loop_start + 1; j != i; ++j)
                   if (j->type() == IT_If || j->type() == IT_EndIf)
                      insns_out.push_back(*j);
                   else if (j->type() == IT_Add && j->cell() != loop_index)
                   {
                     insns_out.push_back(*j);
                     (insns_out.end() - 1)->add_cell_mult(loop_index);
                     if (loop_sign < 0)
                     {
                       Integer v = (insns_out.end() - 1)->value();
                       v = remap_cell(-v, lo, hi);
                       (insns_out.end() - 1)->set_value(v);
                     }
                   }
             }
             insns_out.push_back(Instruction(IT_Set, -1, loop_index, 0));
           }
           if (print_debug_info)
           {
             std::cout << "Squashed output loop:\n";
             for (std::vector<Instruction>::const_iterator 
                             j = insns_out.begin() + loop_start_out;
                             j != insns_out.end();
                             ++j)
                 std::cout << "    " << *j << '\n';
           }
           break;

      case IT_And:
      case IT_CheckAnd:
      case IT_Add0:
      case IT_Set0:
      case IT_Add1v1:
      case IT_Set1v1:
      case IT_Add1:
      case IT_Set1:
      case IT_Inc:
      case IT_Dec:
           assert(0);    // Should not appear here
           break;
    }
  }

  *p_insns = insns_out;
  return squashed_anything;
}

// Optimize.  
void optimize(bool print_debug_info,
              const End &left,
              const End &right, const End &low, const End &high,
              const EOFMethod &eof_method,
              std::vector<Instruction> *p_insns)
{ 
  bool can_combine_tape_ops = 
       (left.is_infinite() || left.type() == ET_undefined || 
                              left.type() == ET_wraparound) &&
       (right.is_infinite() || right.type() == ET_undefined ||
                               right.type() == ET_wraparound);

  if (!can_combine_tape_ops)
     return;

  do
  {
    constant_remove(print_debug_info, p_insns, low, high, eof_method);
  } while (dead_code_remove(p_insns, eof_method) 
                  |
           squash_loops(print_debug_info, p_insns, low, high)
          );
}
