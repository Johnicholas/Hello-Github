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
#include <string>

#include "Integer.h"
#include "Lexer.h"

using namespace std;

LexerReturnType lexer_keyword_values[] =
{
KW_Abort, KW_All, KW_At, KW_Break, KW_Cell, KW_Char, KW_Clear, KW_Compile,
KW_Copying, KW_Decimal, KW_Default,
KW_Delete, KW_Eof, KW_Exit, KW_For, KW_Halt, KW_Infinite, KW_Input, KW_Int,
KW_Copying, KW_Load, KW_Left, KW_Limiting, KW_Long, KW_Low, KW_Help, KW_High, 
KW_Nop, KW_Output, KW_PC, KW_Position, KW_Print_Insns,
KW_Prog, KW_Exit, KW_Read, KW_Right, 
KW_Run, KW_Set, KW_Short, KW_Show, KW_Signed, KW_Tape, 
KW_Undefined, KW_Unsigned, KW_Value, KW_Warranty, KW_Wraparound, KW_Wraparound
};

const char *lexer_keywords[] =
{
"abort", "all", "at", "break", "cell", "char", "clear", "compile",
"copying", "decimal", "default",
"delete", "eof", "exit", "for", "halt", "infinite", "input", "int",
"license", "load", "left", "limiting", "long", "low", "help", "high", 
"nop", "output", "pc", "position", "print_insns",
"prog", "quit", "read", "right", 
"run", "set", "short", "show", "signed", "tape", 
"undefined", "unsigned", "value", "warranty", "wrap", "wraparound"
};

LexerReturn Lexer::get()
{
  int sign;

  for (;;)
  {
    if (posn == s.length())
       return LexerReturn(L_EOF);

    if (s[posn] != ' ' && s[posn] != '\t')
       break;

    posn++;
  }

  sign = 1;

  switch (s[posn])
  {
    case '?':
       posn++;
       return LexerReturn(L_QuestionMark);

    case '-':
       posn++;
       if (posn == s.length() || s[posn] < '0' || s[posn] > '9')
          return LexerReturn(L_Minus);
       sign = -1;

       /* Fall through to number case */

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
       {
         Integer rv(s[posn++] - '0');
         while (posn != s.length() && s[posn] >= '0' && s[posn] <= '9')
               rv = rv * 10 + (s[posn++] - '0');
         return LexerReturn(rv * sign);
       }

    case '"':
       {
         string ss;
         posn++;
         size_t start_posn = posn;
         std::string::size_type end_posn = s.find('"', posn);
         if (end_posn == std::string::npos)   // Not found
            end_posn = posn = s.length();
         else
            posn = end_posn + 1;
         return LexerReturn(s.substr(start_posn, end_posn - start_posn));
       }
  }

  /* None of the above */

  if (s[posn] != '_' && (s[posn] < 'a' || s[posn] > 'z'))
     return LexerReturn(L_Error);

  std::string::size_type end_posn = 
              s.find_first_not_of("abcdefghijklmnopqrstuvwxyz_", posn);
  if (end_posn == std::string::npos)
     end_posn = s.length();
  assert(sizeof(lexer_keywords) / sizeof(const char *) ==
         sizeof(lexer_keyword_values) / sizeof(LexerReturnType));
  for (unsigned i = 0; i < sizeof(lexer_keywords) / sizeof(const char *); i++)
  if (s.compare(posn, end_posn - posn, lexer_keywords[i]) == 0)
  {
    posn = end_posn;
    return LexerReturn(lexer_keyword_values[i]);
  }
  return LexerReturn(L_Error);
}
