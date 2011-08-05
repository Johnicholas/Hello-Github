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

#ifndef LEXER_INCL

#define LEXER_INCL

#include <cstddef>
#include <string>

#include "Integer.h"

enum LexerReturnType 
{
L_EOF, L_Error, L_Minus, L_Integer, L_String, L_QuestionMark,

P_Missing,      // Will be used by parser to denote a missing argument
P_Error,        // Will be used by parser to denote an error

// Keywords

KW_Abort,
KW_All, KW_At, KW_Break, KW_Cell, KW_Char, KW_Clear, KW_Compile,
KW_Copying, KW_Decimal, KW_Default,
KW_Delete, KW_Eof, KW_Exit, KW_For, KW_Halt, KW_Infinite, KW_Input,
KW_Int, KW_Load, KW_Left, KW_Limiting, KW_Long, KW_Low, KW_Help, KW_High, 
KW_Nop, KW_Output, KW_PC, KW_Position, KW_Print_Insns,
KW_Prog, KW_Read, KW_Right, 
KW_Run, KW_Set, KW_Short, KW_Show, KW_Signed, KW_Tape, 
KW_Undefined, KW_Unsigned, KW_Value, KW_Warranty, KW_Wraparound
};

class LexerReturn
{
  LexerReturnType t;
  Integer i;
  std::string s;

  explicit LexerReturn(LexerReturnType u) : t(u), i(), s() { }
  explicit LexerReturn(const Integer &ii) : t(L_Integer), i(ii), s() { }
  explicit LexerReturn(const std::string &ss) : t(L_String), i(), s(ss) { }

public:

  bool is_string() const { return t == L_String; }
  bool is_integer() const { return t == L_Integer; }

  LexerReturnType type() const { return t; }
  std::string get_string() const { return s; }
  Integer get_integer() const { return i; }
 
  friend class Lexer;
  friend class ParserReturn;
  friend ParserReturn parse(const std::string &);
};

class Lexer
{
  std::string s;
  size_t posn;

public:
  explicit Lexer(const std::string &t) : s(t), posn(0) { }
  LexerReturn get();
};

#endif
