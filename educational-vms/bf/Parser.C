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

#include <string>

#include "Integer.h"
#include "Lexer.h"
#include "Parser.h"

ParserReturn parse(const std::string &s)
{
  Lexer l(s);
  LexerReturn r(l.get());

  switch (r.type())
  {
    case L_EOF:
             return ParserReturn(L_EOF);

    case KW_Break:
             if (l.get().type() != KW_At)
                return ParserReturn(P_Error);
             r = l.get();
             if (!r.is_integer() || l.get().type() != L_EOF)
                return ParserReturn(P_Error);
             return ParserReturn(KW_Break, r);
              
    case KW_Exit:
    case KW_Clear:
             if (l.get().type() != L_EOF)
                return ParserReturn(P_Error);
             return ParserReturn(r);

    case KW_Print_Insns:
             r = l.get();
             if (r.type() == L_EOF)
                return ParserReturn(KW_Print_Insns);
             else if (!r.is_integer() || l.get().type() != L_EOF)
                return ParserReturn(P_Error);
             else
                return ParserReturn(KW_Print_Insns, r);
         
    case KW_Help:
    case L_QuestionMark:
             if (l.get().type() != L_EOF) 
                return ParserReturn(P_Error);
             return ParserReturn(KW_Help);

    case KW_Delete:
             r = l.get();
             if (!r.is_integer() || l.get().type() != L_EOF)
                return ParserReturn(P_Error);
             return ParserReturn(KW_Delete, r);
 
    case KW_Load:
    case KW_Read:
    case KW_Compile:
             {
               LexerReturn s(l.get());
               if (s.type() == L_EOF)
                  return ParserReturn(r);
               else if (!s.is_string() || l.get().type() != L_EOF)
                  return ParserReturn(P_Error);
               return ParserReturn(r, s);
             }
             /*NOTREACHED*/
 
    case KW_Run:
             {
               bool set_at = false;
               bool set_for = false;
               Integer at_val, for_val;
               for (;;)
               {
                 r = l.get();
                 if (r.type() == L_EOF)
                    break;

                 if (r.type() == KW_At && !set_at)
                 {
                   r = l.get();
                   if (!r.is_integer())
                       return ParserReturn(P_Error);
                   set_at = true;
                   at_val = r.get_integer();
                 }
                 else if (r.type() == KW_For && !set_for)
                 {
                   r = l.get();
                   if (!r.is_integer())
                       return ParserReturn(P_Error);
                   set_for = true;
                   for_val = r.get_integer();
                 }
                 else
                    return ParserReturn(P_Error);
               }
               return ParserReturn(KW_Run, 
                    (set_at ? LexerReturn(at_val) : LexerReturn(P_Missing)),
                    (set_for ? LexerReturn(for_val) : LexerReturn(P_Missing))
                                  );
             }
             /*NOTREACHED*/
 
    case KW_Set:
             r = l.get();
             switch (r.type())
             {
               case KW_Cell:
                    {
                      LexerReturnType t_length(P_Missing), t_signed(P_Missing);
                      LexerReturnType tp;

                      for (;;)
                      {
                        if ((tp = l.get().type()) == L_EOF)
                           break;

                        switch (tp)
                        {
                          case KW_Int:
                                 break;

                          case KW_Char:
                          case KW_Short:
                          case KW_Long:
                                 if (t_length != P_Missing)
                                      return ParserReturn(P_Error);
                                 t_length = tp;
                                 break;

                          case KW_Signed:
                          case KW_Unsigned:
                                 if (t_signed != P_Missing)
                                      return ParserReturn(P_Error);
                                 t_signed = tp;
                                 break;

                          default:
                                 return ParserReturn(P_Error);
                                 break;
                        }
                      }
                      return ParserReturn(KW_Set, KW_Cell, t_signed, t_length);
                    }
                    break;
                   
               case KW_Default:
                    if (l.get().type() != L_EOF)
                       return ParserReturn(P_Error);
                    return ParserReturn(KW_Set, r);

               case KW_Position:
               case KW_PC:
                    {
                      LexerReturnType tp = r.type(); 
                      r = l.get();
                      if (!r.is_integer() || l.get().type() != L_EOF)
                         return ParserReturn(P_Error);
                      return ParserReturn(KW_Set, tp, r);
                    }
                    /*NOTREACHED*/

               case KW_Tape:
                    {
                      r = l.get();
                      if (!r.is_integer())
                         return ParserReturn(P_Error);
                      LexerReturn s = l.get();
                      if (!s.is_integer())
                         return ParserReturn(P_Error);
                      return ParserReturn(KW_Set, KW_Tape, r, s);
                    }
                    /*NOTREACHED*/

               case KW_Low:
               case KW_High:
               case KW_Left:
               case KW_Right:
                    {
                      LexerReturnType tp = r.type(); 

                      r = l.get();
                      if (r.type() != KW_Wraparound && r.type() != KW_Limiting
                          && r.type() != KW_Abort && r.type() != KW_Undefined
                          && r.type() != KW_Infinite && r.type() != L_Integer)
                          return ParserReturn(P_Error);

                      LexerReturn s(l.get());

                      if (s.type() == L_EOF)
                           return ParserReturn(KW_Set, tp, r);

                      if (r.type() == KW_Infinite)
                          return ParserReturn(P_Error);

                      if (r.type() == L_Integer)
                      {
                        if (s.type() != KW_Wraparound && s.type() != KW_Limiting
                            && s.type() != KW_Abort && s.type() != KW_Undefined)
                           return ParserReturn(P_Error);
                        else if (l.get().type() != L_EOF)
                           return ParserReturn(P_Error);
                        else 
                           return ParserReturn(KW_Set, tp, r, s);
                      } else
                      {
                        if (s.type() != L_Integer)
                           return ParserReturn(P_Error);
                        else if (l.get().type() != L_EOF)
                           return ParserReturn(P_Error);
                        else
                           return ParserReturn(KW_Set, tp, s, r);
                      }
                    }
                    /*NOTREACHED*/

               case KW_Input:
                    r = l.get();
                    if (r.type() != KW_Signed && r.type() != KW_Unsigned
                        && r.type() != KW_Char && r.type() != KW_Decimal)
                       return ParserReturn(P_Error);
                    if (r.type() == KW_Signed || r.type() == KW_Unsigned)
                    {
                      LexerReturn s = l.get();
                      if (s.type() == KW_Char)
                      {
                        if (l.get().type() != L_EOF)
                           return ParserReturn(P_Error);
                      }
                      else if (s.type() != L_EOF)
                          return ParserReturn(P_Error);
                    } else if (l.get().type() != L_EOF)
                      return ParserReturn(P_Error);
                    if (r.type() == KW_Char)
                       return ParserReturn(KW_Set, KW_Input, 
                                           LexerReturn(KW_Unsigned));
                    else
                       return ParserReturn(KW_Set, KW_Input, r);
                    /*NOTREACHED*/
                     
               case KW_Eof:
                    r = l.get();
                    if (r.type() == KW_Value)
                    {
                       r = l.get();
                       if (r.type() != L_Integer)
                          return ParserReturn(P_Error);
                    } 
                    else
                    if (r.type() != L_Integer && r.type() != KW_Nop &&
                        r.type() != KW_Halt && r.type() != KW_Abort &&
                        r.type() != KW_Value)
                          return ParserReturn(P_Error);
                    if (l.get().type() != L_EOF)
                          return ParserReturn(P_Error);
                    return ParserReturn(KW_Set, KW_Eof, r);

               case KW_Output:
                    r = l.get();
                    if (r.type() != KW_Char && r.type() != KW_Decimal)
                          return ParserReturn(P_Error);
                    if (l.get().type() != L_EOF)
                          return ParserReturn(P_Error);
                    return ParserReturn(KW_Set, KW_Output, r);

               default: 
                    return ParserReturn(P_Error);
             }
             /*NOTREACHED*/
 
    case KW_Show:
             r = l.get();
             switch (r.type())
             {
               case L_EOF: 
                    return ParserReturn(KW_Show);

               case KW_Copying:
               case KW_Warranty:
                    if (l.get().type() != L_EOF)
                          return ParserReturn(P_Error);
                    return ParserReturn(KW_Show, r);

               case KW_Prog:
               case KW_Tape:
                    {
                      LexerReturnType tp = r.type();
                      bool set_num = false;
                      bool set_at = false;
                      Integer num_val, at_val; 

                      r = l.get();
                      if (r.is_integer())
                      { 
                        set_num = true;
                        num_val = r.get_integer();
                        r = l.get();
                      }
                      if (r.type() == KW_At)
                      {
                        r = l.get();
                        if (!r.is_integer())
                           return ParserReturn(P_Error);
                        set_at = true;
                        at_val = r.get_integer();
                        r = l.get();
                      }
                      if (r.type() != L_EOF)
                         return ParserReturn(P_Error);
                      return ParserReturn(KW_Show, tp,
                    (set_num ? LexerReturn(num_val) : LexerReturn(P_Missing)),
                    (set_at ? LexerReturn(at_val) : LexerReturn(P_Missing))
                                  );
                    }
                    /*NOTREACHED*/

               default: 
                    return ParserReturn(P_Error);
             }
             /*NOTREACHED*/

    default: return ParserReturn(P_Error);
  }
  /*NOTREACHED*/
}
