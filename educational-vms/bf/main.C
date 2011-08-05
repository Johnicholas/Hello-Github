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
#include <cstring>
#include <iostream>
#include <fstream>

#include "BF.h"
#include "Integer.h"
#include "Lexer.h"
#include "Parser.h"
#include "NumericLimits.h"

using namespace std;

const char *version_str =
#include "version.h"
;

void help()
{
cout << "\
\n\
COMMAND SUMMARY\n\
\n\
% ... comment ...            Comment; does nothing\n\
?                            Show this summary\n\
break at #                   Set breakpoint at location #\n\
clear                        Clear tape\n\
compile (\"output-file\")      Compile to C program, write to output-file\n\
                                     (or stdout if output-file absent)\n\
delete #                     Delete breakpoint number #\n\
exit                         Quit\n\
help                         Show this summary\n\
load \"code-string\"           Load new BF code string (resets breakpoints, PC)\n\
quit                         Quit\n\
read \"filename\"              Load new BF code file (resets breakpoints, PC)\n\
run (at #1) (for #2)         Run program for #1 instructions (or forever\n\
                               if no #1), starting at #2 (or PC if no #2)\n\
set pc #                     Set PC to #\n\
set position #               Set head position to #\n\
set tape #1 #2               Set cell on tape numbered #1 to #2\n\
show                         Show PC, position, breakpoints, and\n\
                               execution parameters (bounds, input method, \n\
                               output method, and EOF processing.)\n\
show copying                 Show license info\n\
show license                 Show license info\n\
show prog (#1) (at #2)       Show #1 chars of program around #2\n\
                               (or PC if no #2)\n\
show tape (#1) (at #2)       Show #1 locations of tape around #2\n\
                               (or tape position if no #2)\n\
show warranty                Inform user that there is no warranty\n\
\n\
         Execution parameter setting commands:\n\
\n\
set <bound> #                Set <bound>: cell value <bound>s are\n\
set <bound> (#) abort          `low' and `high'; tape <bound>s are\n\
set <bound> (#) limiting       `left' and `right'.\n\
set <bound> (#) undefined\n\
set <bound> (#) wrap(around)\n\
set <bound> infinite\n\
set cell (un)(signed) char   Set low and high cell bounds to be wraparound\n\
set cell (unsigned) short       and appropriate for the indicated C type.\n\
set cell (unsigned) int         Compilation to C is only possible for these\n\
set cell (unsigned) long        cell bounds; also, they speed interpretation.\n\
set default                  Reset all parameters to default\n\
set eof abort                Abort program upon reading EOF\n\
set eof halt                 Halt program normally upon reading EOF\n\
set eof nop                  Do nothing upon read of EOF\n\
set eof (value) #            EOF becomes # when read into tape\n\
set input decimal            Input (decimal) numbers\n\
set input signed (char)      Input characters; values become -128..127 on tape\n\
set input (unsigned) char    Input characters; values become 0..255 on tape\n\
set input unsigned           Same as `set input unsigned char'\n\
set output char              Output characters\n\
set output decimal           Output (decimal) numbers\n";
}

void copying()
{
cout << "\
\n\
                    GNU GENERAL PUBLIC LICENSE\n\
                       Version 2, June 1991\n\
\n\
 Copyright (C) 1989, 1991 Free Software Foundation, Inc.\n\
 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA\n\
 Everyone is permitted to copy and distribute verbatim copies\n\
 of this license document, but changing it is not allowed.\n\
\n\
                            Preamble\n\
\n\
  The licenses for most software are designed to take away your\n\
freedom to share and change it.  By contrast, the GNU General Public\n\
License is intended to guarantee your freedom to share and change free\n\
software--to make sure the software is free for all its users.  This\n\
General Public License applies to most of the Free Software\n\
Foundation's software and to any other program whose authors commit to\n\
using it.  (Some other Free Software Foundation software is covered by\n\
the GNU Lesser General Public License instead.)  You can apply it to\n\
your programs, too.\n\
\n\
  When we speak of free software, we are referring to freedom, not\n\
price.  Our General Public Licenses are designed to make sure that you\n\
have the freedom to distribute copies of free software (and charge for\n\
this service if you wish), that you receive source code or can get it\n\
if you want it, that you can change the software or use pieces of it\n\
in new free programs; and that you know you can do these things.\n\
\n\
  To protect your rights, we need to make restrictions that forbid\n\
anyone to deny you these rights or to ask you to surrender the rights.\n\
These restrictions translate to certain responsibilities for you if you\n\
distribute copies of the software, or if you modify it.\n\
\n\
  For example, if you distribute copies of such a program, whether\n\
gratis or for a fee, you must give the recipients all the rights that\n\
you have.  You must make sure that they, too, receive or can get the\n\
source code.  And you must show them these terms so they know their\n\
rights.\n\
\n\
  We protect your rights with two steps: (1) copyright the software, and\n\
(2) offer you this license which gives you legal permission to copy,\n\
distribute and/or modify the software.\n\
\n\
  Also, for each author's protection and ours, we want to make certain\n\
that everyone understands that there is no warranty for this free\n\
software.  If the software is modified by someone else and passed on, we\n\
want its recipients to know that what they have is not the original, so\n\
that any problems introduced by others will not reflect on the original\n\
authors' reputations.\n\
\n\
  Finally, any free program is threatened constantly by software\n\
patents.  We wish to avoid the danger that redistributors of a free\n\
program will individually obtain patent licenses, in effect making the\n\
program proprietary.  To prevent this, we have made it clear that any\n\
patent must be licensed for everyone's free use or not licensed at all.\n\
\n\
  The precise terms and conditions for copying, distribution and\n\
modification follow.\n\
\n\
                    GNU GENERAL PUBLIC LICENSE\n\
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION\n\
\n\
  0. This License applies to any program or other work which contains\n\
a notice placed by the copyright holder saying it may be distributed\n\
under the terms of this General Public License.  The \"Program\", below,\n\
refers to any such program or work, and a \"work based on the Program\"\n\
means either the Program or any derivative work under copyright law:\n\
that is to say, a work containing the Program or a portion of it,\n\
either verbatim or with modifications and/or translated into another\n\
language.  (Hereinafter, translation is included without limitation in\n\
the term \"modification\".)  Each licensee is addressed as \"you\".\n\
\n\
Activities other than copying, distribution and modification are not\n\
covered by this License; they are outside its scope.  The act of\n\
running the Program is not restricted, and the output from the Program\n\
is covered only if its contents constitute a work based on the\n\
Program (independent of having been made by running the Program).\n\
Whether that is true depends on what the Program does.\n\
\n\
  1. You may copy and distribute verbatim copies of the Program's\n\
source code as you receive it, in any medium, provided that you\n\
conspicuously and appropriately publish on each copy an appropriate\n\
copyright notice and disclaimer of warranty; keep intact all the\n\
notices that refer to this License and to the absence of any warranty;\n\
and give any other recipients of the Program a copy of this License\n\
along with the Program.\n\
\n\
You may charge a fee for the physical act of transferring a copy, and\n\
you may at your option offer warranty protection in exchange for a fee.\n\
\n\
  2. You may modify your copy or copies of the Program or any portion\n\
of it, thus forming a work based on the Program, and copy and\n\
distribute such modifications or work under the terms of Section 1\n\
above, provided that you also meet all of these conditions:\n\
\n\
    a) You must cause the modified files to carry prominent notices\n\
    stating that you changed the files and the date of any change.\n\
\n\
    b) You must cause any work that you distribute or publish, that in\n\
    whole or in part contains or is derived from the Program or any\n\
    part thereof, to be licensed as a whole at no charge to all third\n\
    parties under the terms of this License.\n\
\n\
    c) If the modified program normally reads commands interactively\n\
    when run, you must cause it, when started running for such\n\
    interactive use in the most ordinary way, to print or display an\n\
    announcement including an appropriate copyright notice and a\n\
    notice that there is no warranty (or else, saying that you provide\n\
    a warranty) and that users may redistribute the program under\n\
    these conditions, and telling the user how to view a copy of this\n\
    License.  (Exception: if the Program itself is interactive but\n\
    does not normally print such an announcement, your work based on\n\
    the Program is not required to print an announcement.)\n\
\n\
These requirements apply to the modified work as a whole.  If\n\
identifiable sections of that work are not derived from the Program,\n\
and can be reasonably considered independent and separate works in\n\
themselves, then this License, and its terms, do not apply to those\n\
sections when you distribute them as separate works.  But when you\n\
distribute the same sections as part of a whole which is a work based\n\
on the Program, the distribution of the whole must be on the terms of\n\
this License, whose permissions for other licensees extend to the\n\
entire whole, and thus to each and every part regardless of who wrote it.\n\
\n\
Thus, it is not the intent of this section to claim rights or contest\n\
your rights to work written entirely by you; rather, the intent is to\n\
exercise the right to control the distribution of derivative or\n\
collective works based on the Program.\n\
\n\
In addition, mere aggregation of another work not based on the Program\n\
with the Program (or with a work based on the Program) on a volume of\n\
a storage or distribution medium does not bring the other work under\n\
the scope of this License.\n\
\n\
  3. You may copy and distribute the Program (or a work based on it,\n\
under Section 2) in object code or executable form under the terms of\n\
Sections 1 and 2 above provided that you also do one of the following:\n\
\n\
    a) Accompany it with the complete corresponding machine-readable\n\
    source code, which must be distributed under the terms of Sections\n\
    1 and 2 above on a medium customarily used for software interchange; or,\n\
\n\
    b) Accompany it with a written offer, valid for at least three\n\
    years, to give any third party, for a charge no more than your\n\
    cost of physically performing source distribution, a complete\n\
    machine-readable copy of the corresponding source code, to be\n\
    distributed under the terms of Sections 1 and 2 above on a medium\n\
    customarily used for software interchange; or,\n\
\n\
    c) Accompany it with the information you received as to the offer\n\
    to distribute corresponding source code.  (This alternative is\n\
    allowed only for noncommercial distribution and only if you\n\
    received the program in object code or executable form with such\n\
    an offer, in accord with Subsection b above.)\n\
\n\
The source code for a work means the preferred form of the work for\n\
making modifications to it.  For an executable work, complete source\n\
code means all the source code for all modules it contains, plus any\n\
associated interface definition files, plus the scripts used to\n\
control compilation and installation of the executable.  However, as a\n\
special exception, the source code distributed need not include\n\
anything that is normally distributed (in either source or binary\n\
form) with the major components (compiler, kernel, and so on) of the\n\
operating system on which the executable runs, unless that component\n\
itself accompanies the executable.\n\
\n\
If distribution of executable or object code is made by offering\n\
access to copy from a designated place, then offering equivalent\n\
access to copy the source code from the same place counts as\n\
distribution of the source code, even though third parties are not\n\
compelled to copy the source along with the object code.\n\
\n\
  4. You may not copy, modify, sublicense, or distribute the Program\n\
except as expressly provided under this License.  Any attempt\n\
otherwise to copy, modify, sublicense or distribute the Program is\n\
void, and will automatically terminate your rights under this License.\n\
However, parties who have received copies, or rights, from you under\n\
this License will not have their licenses terminated so long as such\n\
parties remain in full compliance.\n\
\n\
  5. You are not required to accept this License, since you have not\n\
signed it.  However, nothing else grants you permission to modify or\n\
distribute the Program or its derivative works.  These actions are\n\
prohibited by law if you do not accept this License.  Therefore, by\n\
modifying or distributing the Program (or any work based on the\n\
Program), you indicate your acceptance of this License to do so, and\n\
all its terms and conditions for copying, distributing or modifying\n\
the Program or works based on it.\n\
\n\
  6. Each time you redistribute the Program (or any work based on the\n\
Program), the recipient automatically receives a license from the\n\
original licensor to copy, distribute or modify the Program subject to\n\
these terms and conditions.  You may not impose any further\n\
restrictions on the recipients' exercise of the rights granted herein.\n\
You are not responsible for enforcing compliance by third parties to\n\
this License.\n\
\n\
  7. If, as a consequence of a court judgment or allegation of patent\n\
infringement or for any other reason (not limited to patent issues),\n\
conditions are imposed on you (whether by court order, agreement or\n\
otherwise) that contradict the conditions of this License, they do not\n\
excuse you from the conditions of this License.  If you cannot\n\
distribute so as to satisfy simultaneously your obligations under this\n\
License and any other pertinent obligations, then as a consequence you\n\
may not distribute the Program at all.  For example, if a patent\n\
license would not permit royalty-free redistribution of the Program by\n\
all those who receive copies directly or indirectly through you, then\n\
the only way you could satisfy both it and this License would be to\n\
refrain entirely from distribution of the Program.\n\
\n\
If any portion of this section is held invalid or unenforceable under\n\
any particular circumstance, the balance of the section is intended to\n\
apply and the section as a whole is intended to apply in other\n\
circumstances.\n\
\n\
It is not the purpose of this section to induce you to infringe any\n\
patents or other property right claims or to contest validity of any\n\
such claims; this section has the sole purpose of protecting the\n\
integrity of the free software distribution system, which is\n\
implemented by public license practices.  Many people have made\n\
generous contributions to the wide range of software distributed\n\
through that system in reliance on consistent application of that\n\
system; it is up to the author/donor to decide if he or she is willing\n\
to distribute software through any other system and a licensee cannot\n\
impose that choice.\n\
\n\
This section is intended to make thoroughly clear what is believed to\n\
be a consequence of the rest of this License.\n\
\n\
  8. If the distribution and/or use of the Program is restricted in\n\
certain countries either by patents or by copyrighted interfaces, the\n\
original copyright holder who places the Program under this License\n\
may add an explicit geographical distribution limitation excluding\n\
those countries, so that distribution is permitted only in or among\n\
countries not thus excluded.  In such case, this License incorporates\n\
the limitation as if written in the body of this License.\n\
\n\
  9. The Free Software Foundation may publish revised and/or new versions\n\
of the General Public License from time to time.  Such new versions will\n\
be similar in spirit to the present version, but may differ in detail to\n\
address new problems or concerns.\n\
\n\
Each version is given a distinguishing version number.  If the Program\n\
specifies a version number of this License which applies to it and \"any\n\
later version\", you have the option of following the terms and conditions\n\
either of that version or of any later version published by the Free\n\
Software Foundation.  If the Program does not specify a version number of\n\
this License, you may choose any version ever published by the Free Software\n\
Foundation.\n\
\n\
  10. If you wish to incorporate parts of the Program into other free\n\
programs whose distribution conditions are different, write to the author\n\
to ask for permission.  For software which is copyrighted by the Free\n\
Software Foundation, write to the Free Software Foundation; we sometimes\n\
make exceptions for this.  Our decision will be guided by the two goals\n\
of preserving the free status of all derivatives of our free software and\n\
of promoting the sharing and reuse of software generally.\n";
}

void warranty()
{
cout << "\
                            NO WARRANTY\n\
\n\
  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY\n\
FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN\n\
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES\n\
PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED\n\
OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF\n\
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS\n\
TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE\n\
PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,\n\
REPAIR OR CORRECTION.\n\
\n\
  12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING\n\
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR\n\
REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,\n\
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING\n\
OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED\n\
TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY\n\
YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER\n\
PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE\n\
POSSIBILITY OF SUCH DAMAGES.\n";
}

void notice(const char *name)
{
  cout << name << " version " << version_str << '\n'
       << "\n\
Copyright 2006 David Moews.\n\
\n\
This program is free software, covered by the GNU General Public License,\n\
and you are welcome to change it and/or distribute copies of it under\n\
certain conditions, as given in version 2 (or, if you prefer, a later\n\
version) of the GNU GPL.  Type \"show copying\" to see the conditions.\n\
\n\
There is absolutely no warranty.  Type \"show warranty\" for details.\n\
\n\
Type \"help\" or \"?\" for a list of commands.\n";
}

void usage(const char *name)
{
  cout << name << " is an interpreter and debugger for the BF language.\n\
Usage: " << name << " [-[n][s][u]] [BF-program-filename]\n\
-n indicates noninteractive mode (default: interactive.)\n\
-s suppresses program termination messages (default: not suppressed.)\n\
-u makes cell size unsigned int (default: unsigned char.)\n\n";
  cout << name << " version: " << version_str << '\n';
}

End set_end(const End &e, const LexerReturn &r, const LexerReturn &s)
{
  End rv = e;

  switch (r.type())
  {
    case L_Integer:
         rv.set_value(r.get_integer());
         switch (s.type())
         {
           case KW_Wraparound:
                rv.set_type(ET_wraparound);
                break;
        
           case KW_Limiting:
                rv.set_type(ET_truncate);
                break;
        
           case KW_Abort:
                rv.set_type(ET_abort);
                break;
     
           case KW_Undefined: 
                rv.set_type(ET_undefined);
                break;

           case P_Missing: 
                break;
     
           default:
                assert(0);   // Can't happen
                break;
         }
         break;

    case KW_Infinite:
         rv.make_infinite();
         break;

    case KW_Wraparound:
         rv.set_type(ET_wraparound);
         break;
 
    case KW_Limiting:
         rv.set_type(ET_truncate);
         break;
 
    case KW_Abort:
         rv.set_type(ET_abort);
         break;

    case KW_Undefined: 
         rv.set_type(ET_undefined);
         break;

    default:
         assert(0);   // Can't happen
         break;
  }
  return rv;
}

void show_prog(const BF &mach, int length, int at)
{
  if (length <= 0 || at < 0 || at > mach.get_code().size())
  {
    cout << "Cannot show code.\n";
    return;
  }
  for (int i = at - ((length-1) / 2); i <= at + (length / 2); i++)
      if (i >= 0 && i <= mach.get_code().size())
      {
        if (i == mach.get_pc())
           cout << ">>> ";
        else
           cout << "    ";
        cout << i << " ";
        if (i == mach.get_code().size())
           cout << "(end)\n";
        else
           cout << mach.get_code()[i] << '\n';
      }
}

void show_tape(const BF &mach, int length, const Integer &at)
{
  if (length <= 0 || 
      !mach.get_left_end().is_le(at) || !mach.get_right_end().is_ge(at))
  {
    cout << "Cannot show data.\n";
    return;
  }
  for (Integer i = at - ((length - 1) /2); i <= at + (length / 2); ++i)
      if (mach.get_left_end().is_le(i) && mach.get_right_end().is_ge(i))
      {
        Integer v;

        assert(mach.get_tape(i, &v));
        if (i == mach.get_position())
           cout << ">>> ";
        else
           cout << "    ";
        cout << i << " " << v << '\n';
      }
}

template<class IntegralType>
void set_cell(BF *p_mach)
{
  p_mach->set_low_end(End(0, ET_truncate));
  p_mach->set_high_end(End(0, ET_truncate));
  p_mach->set_low_end(End(NumericLimits<IntegralType>::min(), ET_wraparound));
  p_mach->set_high_end(End(NumericLimits<IntegralType>::max(), ET_wraparound));
}

#define BUFSIZE 32768
bool read_file_into_string(const char *fn, string *p_s)
{
  ifstream i(fn);
  char bf[BUFSIZE];

  if (!i)
     return false;

  *p_s = string();

  for (;;)
  {
    i.read(bf, BUFSIZE);
    *p_s += string(bf, i.gcount());
    if (i.gcount() != BUFSIZE)
       break;
  }
  return true;
}
#undef BUFSIZE

#define DEFAULT_CODE_SHOW_LEN 20
#define DEFAULT_TAPE_SHOW_LEN 20

int main(int argc, char **argv)
{
  BF mach;
  string cmd, prog;
  int b_num;
  bool interactive = true;
  bool squash = false;
  bool default_unsigned = false;
  const char *input_method_names[] = 
        { "signed char", "unsigned char", "decimal" };
  const char *output_method_names[] = { "char", "decimal" };
  const char *eof_method_names[] = { "halt", "abort", "nop", "value" };
  const char *program_file_name = NULL;
  RunTermination termination_type;

                
  for (int i = 1; i < argc; i++)
  if (argv[i][0] != '-')
  {
    if (program_file_name != NULL)
    {
      usage(argv[0]);
      return 99;
    }
    program_file_name = argv[i];
  } else
  for (const char *p = &argv[i][1]; *p != '\0'; p++)
  {
    switch (*p)
    {
      case 'n':
             interactive = false;
             break;

      case 's':
             squash = true;
             break;

      case 'u':
             default_unsigned = true;
             break;

      default:
             usage(argv[0]);
             return 99;
    }
  }

  if (default_unsigned)
     set_cell<unsigned>(&mach);
  else
     set_cell<unsigned char>(&mach);

  if (program_file_name != NULL)
  {
    if (!read_file_into_string(program_file_name, &prog))
    {
      cout << "Cannot open file.\n";
      return 1;
    } else
    {
      if (!mach.set_code(prog) || 
          (termination_type = mach.run(-1)) == RT_Program_Invalid)
      {
        cout << "Invalid program.\n";
        return 3;
      }
      if (termination_type == RT_Infinite_Loop)
         return 2;
      assert(termination_type == RT_Normal);
      return 0;
    }
  }

  if (interactive)
     notice(argv[0]);

  for (;;)
  {
    if (interactive)
        cout << "BF> ";
    if (!getline(cin, cmd))
       return 0;              // EOF
  
    if (cmd.size() > 0 && cmd[0] == '%')
       continue;              // Comment

    ParserReturn r = parse(cmd);
    switch (r.get_a().type())
    {
      case L_EOF:
          break;

      case KW_Break:
          b_num = mach.add_breakpoint(r.get_b().get_integer());
          if (b_num < 0)
             cout << "Cannot add breakpoint at " 
                  << r.get_b().get_integer() << ".\n";
          else
             cout << "Breakpoint " << b_num + 1 << " added at "
                  << r.get_b().get_integer() << ".\n";
          break;

      case KW_Clear:
          mach.clear_tape();
          break;

      case KW_Delete:
          if (mach.num_breakpoints() == 0)
             cout << "No breakpoints are set.\n";
          else if (!mach.delete_breakpoint(r.get_b().get_integer() - 1))
             cout << "Breakpoints are numbered from 1 to " 
                  << mach.num_breakpoints() << ".\n";
          break;

      case KW_Exit:
          return 0;
          break;

      case KW_Help:
          help();
          break;

      case KW_Load:
          if (!mach.set_code(r.get_b().get_string()))
             cout << "Invalid program.\n";
          break;

      case KW_Read:
          if (!read_file_into_string(r.get_b().get_string().c_str(), &prog))
             cout << "Cannot open file.\n";
          else if (!mach.set_code(prog))
             cout << "Invalid program.\n";
          break;

      case KW_Compile:
          if (r.get_b().type() != P_Missing)
          {
            ofstream o(r.get_b().get_string().c_str());

            if (!o)
            {
              cout << "Cannot open file.\n";
              continue;
            }
            if (!mach.compile_to(o))
               cout << "Cannot compile program.\n";
          } else
            if (!mach.compile_to(cout))
               cout << "Cannot compile program.\n";
          break;

      case KW_Print_Insns:
          if (r.get_b().is_integer())
             mach.print_insns((r.get_b().get_integer() % 4) >= 2,
                              (r.get_b().get_integer() % 2) == 1);
          else
             mach.print_insns();
          break;

      case KW_Run:
          if (r.get_b().is_integer())
          {
            if (!mach.set_pc(r.get_b().get_integer()))
            {
              cout << "Cannot set PC to " << r.get_b().get_integer() << ".\n";
              continue;
            }
          }
          termination_type = 
                  mach.run(r.get_c().is_integer() ?  r.get_c().get_integer() : Integer(-1));
          if (!squash)
          {
            switch (termination_type)
            {
              case RT_Normal:
                   cout << "Program terminated normally.\n";
                   break;
  
              case RT_Program_Invalid:
                   cout << "Program is invalid and cannot be run.\n";
                   break;
  
              case RT_Infinite_Loop:
                   cout << "Infinite loop detected in program.\n";
                   break;
  
              case RT_Breakpoint:
                   cout << "Program stopped at breakpoint.\n";
                   break;
  
              case RT_EOF_Error:
                   cout << "Program hit end of file and aborted.\n";
                   break;
  
              case RT_Bound_Error:
                   cout << 
                      "Program exceeded its tape or cell bounds and aborted.\n";
                   break;
  
              case RT_Chunk_Done:
                   cout << "Finished executing " << r.get_c().get_integer()
                        << " instructions.\n";
                   break;
            }
          }
          // 
          // Reset stdin, in case we encountered EOF.
          // 
          cin.clear();
          break;

      case KW_Set:
          switch (r.get_b().type())
          {
            case KW_Cell:
                switch (r.get_d().type())
                {
                  case KW_Char:
                       switch (r.get_c().type())
                       {
                         case P_Missing:
                              set_cell<char>(&mach);
                              break;

                         case KW_Signed:
                              set_cell<signed char>(&mach);
                              break;

                         case KW_Unsigned:
                              set_cell<unsigned char>(&mach);
                              break;

                         default:
                              assert(0);
                              break;
                       }
                       break;

                  case KW_Short:
                       if (r.get_c().type() == KW_Unsigned)
                          set_cell<unsigned short>(&mach);
                       else
                          set_cell<short>(&mach);
                       break;

                  // int
                  case P_Missing:
                       if (r.get_c().type() == KW_Unsigned)
                          set_cell<unsigned int>(&mach);
                       else
                          set_cell<int>(&mach);
                       break;

                  case KW_Long:
                       if (r.get_c().type() == KW_Unsigned)
                          set_cell<unsigned long>(&mach);
                       else
                          set_cell<long>(&mach);
                       break;

                  default:
                       assert(0);
                       break;
                }
                break;

            case KW_Default:
                mach.set_left_end(
                        End(mach.get_left_end().bound(), ET_truncate));
                mach.set_right_end(
                        End(mach.get_right_end().bound(), ET_truncate));
                mach.set_left_end(End());
                mach.set_right_end(End());

                if (default_unsigned)
                   set_cell<unsigned>(&mach);
                else
                   set_cell<unsigned char>(&mach);

                mach.set_input_method(IM_unsigned);
                mach.set_output_method(OM_char);
                mach.set_eof_method(EOF_value);
                mach.set_eof_value(0);
                break;

            case KW_Input:
                switch (r.get_c().type())
                {
                  case KW_Signed: 
                       mach.set_input_method(IM_signed);
                       break;

                  case KW_Unsigned: 
                       mach.set_input_method(IM_unsigned);
                       break;

                  case KW_Decimal: 
                       mach.set_input_method(IM_decimal);
                       break;

                  default:
                       cout << "Syntax error.\n";
                       break;
                }
                break;

            case KW_Output:
                switch (r.get_c().type())
                {
                  case KW_Char: 
                       mach.set_output_method(OM_char);
                       break;

                  case KW_Decimal: 
                       mach.set_output_method(OM_decimal);
                       break;

                  default:
                       cout << "Syntax error.\n";
                       break;
                }
                break;

            case KW_Eof:
                switch (r.get_c().type())
                {
                  case KW_Abort: 
                       mach.set_eof_method(EOF_abort);
                       break;

                  case KW_Halt: 
                       mach.set_eof_method(EOF_halt);
                       break;

                  case KW_Nop: 
                       mach.set_eof_method(EOF_unchanged);
                       break;

                  case L_Integer: 
                       mach.set_eof_method(EOF_value);
                       mach.set_eof_value(r.get_c().get_integer());
                       break;

                  default:
                       cout << "Syntax error.\n";
                       break;
                }
                break;

            case KW_Left:
                if (!mach.set_left_end(set_end(mach.get_left_end(), 
                                               r.get_c(), r.get_d())))
                   cout << "Cannot reset left end of tape.\n";
                break;
                   
            case KW_Right:
                if (!mach.set_right_end(set_end(mach.get_right_end(), 
                                              r.get_c(), r.get_d())))
                   cout << "Cannot reset right end of tape.\n";
                break;
                   
            case KW_Low:
                if (!mach.set_low_end(set_end(mach.get_low_end(), 
                                              r.get_c(), r.get_d())))
                   cout << "Cannot reset cell lower bound.\n";
                break;
                   
            case KW_High:
                if (!mach.set_high_end(set_end(mach.get_high_end(), 
                                               r.get_c(), r.get_d())))
                   cout << "Cannot reset cell upper bound.\n";
                break;
                
            case KW_Position:  
                if (!mach.set_position(r.get_c().get_integer()))
                   cout << "Cannot set position.\n";
                break;
                
            case KW_PC:  
                if (!mach.set_pc(r.get_c().get_integer()))
                   cout << "Cannot set PC.\n";
                break;

            case KW_Tape:
                if (!mach.set_tape(r.get_c().get_integer(), 
                                   r.get_d().get_integer()))
                   cout << "Cannot set tape cell " << r.get_c().get_integer()
                        << ".\n";
                break;

            default: 
               cout << "Syntax error.\n";
               break;
          }
          break;


      case KW_Show:
          switch (r.get_b().type())
          {
            case P_Missing:
                 cout << "PC is " << mach.get_pc() << ", tape position is "
                      << mach.get_position() << ".\n";
                 for (int i = 0; i < mach.num_breakpoints(); i++)
                     if (mach.get_breakpoint(i) >= 0)
                        cout << "Breakpoint " << i + 1 << " set at "
                             << mach.get_breakpoint(i) << ".\n"; 
                 cout << "Left end of tape: " << mach.get_left_end() << ".\n";
                 cout << "Right end of tape: " << mach.get_right_end() << ".\n";
                 cout << "Cell lower bound: " << mach.get_low_end() << ".\n";
                 cout << "Cell upper bound: " << mach.get_high_end() << ".\n";
                 cout << "Input method: " 
                      << input_method_names[mach.get_input_method()]
                      << ".\n";
                 cout << "Output method: " 
                      << output_method_names[mach.get_output_method()]
                      << ".\n";
                 cout << "EOF processing: " 
                      << eof_method_names[mach.get_eof_method()];
                 if (mach.get_eof_method() == EOF_value)
                    cout << " " << mach.get_eof_value();
                 cout << ".\n";
                 break;

            case KW_Copying:
                 copying();
                 break;

            case KW_Warranty:
                 warranty();
                 break;

            case KW_Prog:
                 show_prog(mach, 
                           (r.get_c().type() != L_Integer) 
                                  ? DEFAULT_CODE_SHOW_LEN
                                    : (int)r.get_c().get_integer(),
                           (r.get_d().type() != L_Integer) 
                                  ? mach.get_pc() 
                                    : (int)r.get_d().get_integer());
                 break;

            case KW_Tape:
                 show_tape(mach, 
                           (r.get_c().type() != L_Integer) 
                                  ? DEFAULT_TAPE_SHOW_LEN
                                    : (int)r.get_c().get_integer(),
                           (r.get_d().type() != L_Integer) 
                                  ? mach.get_position() 
                                    : r.get_d().get_integer());
                 break;

            default: 
               cout << "Syntax error.\n";
               break;
          }
          break;

      default:
          cout << "Syntax error.\n";
          break;
    }
  }
  return 0;  
}

#undef BUFSIZE
#undef DEFAULT_CODE_SHOW_LEN
#undef DEFAULT_TAPE_SHOW_LEN
