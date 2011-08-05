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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. */

#ifndef PARSER_INCL

#define PARSER_INCL

#include <string>

#include "Lexer.h"

// We reuse the lexer return tokens for parsing purposes.
// This is a type abuse.

class ParserReturn
{
  LexerReturn a, b, c, d;

  explicit ParserReturn(LexerReturnType t) 
        : a(t), b(P_Missing), c(P_Missing), d(P_Missing) { }
  explicit ParserReturn(LexerReturn t) 
        : a(t), b(P_Missing), c(P_Missing), d(P_Missing) { }

  ParserReturn(LexerReturnType t, LexerReturnType u) 
        : a(t), b(u), c(P_Missing), d(P_Missing) { }
  ParserReturn(LexerReturnType t, LexerReturn u) 
        : a(t), b(u), c(P_Missing), d(P_Missing) { }
  ParserReturn(LexerReturn t, LexerReturn u) 
        : a(t), b(u), c(P_Missing), d(P_Missing) { }

  ParserReturn(LexerReturnType t, LexerReturnType u, LexerReturn v)
        : a(t), b(u), c(v), d(P_Missing) { }
  ParserReturn(LexerReturnType t, LexerReturn u1, LexerReturn u2)
        : a(t), b(u1), c(u2), d(P_Missing) { }

  ParserReturn(LexerReturnType t, LexerReturnType t2, 
               LexerReturn u1, LexerReturn u2)
        : a(t), b(t2), c(u1), d(u2) { }
  ParserReturn(LexerReturnType t, LexerReturnType t2, 
               LexerReturnType t3, LexerReturnType t4)
        : a(t), b(t2), c(t3), d(t4) { }

public:
  LexerReturn get_a() const { return a; }
  LexerReturn get_b() const { return b; }
  LexerReturn get_c() const { return c; }
  LexerReturn get_d() const { return d; }

  friend ParserReturn parse(const std::string &s);
};

#endif
