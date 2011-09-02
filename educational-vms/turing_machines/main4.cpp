/*
 *  TuringMachine.cpp simulates a universal Turing Machine
 *
 *  This code is a modified version of Hector Zenil's code 
 *
 *  Released under the GNU GENERAL PUBLIC LICENSE 
 *   ( http://www.gnu.org/licenses/gpl-3.0.txt )
 *  
 *  see http://www.mathrix.org/experimentalAIT/TuringMachine.html for full rules and other details.
 *  
 *  Written by Hein Hundal 12-21-08
 * 
 *  
 *  Permission is hereby granted, free of charge, to any person obtaining a copy of this software without restriction and without 
 *  limitation the rights to use, copy, modify, merge, publish, or distribute, and to permit persons to whom the Software is 
 *  furnished to do so, subject to the following condition: This permission notice shall be included in all copies or substantial 
 *  portions of the Software.
 *  
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
 *  FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION 
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 *  The remarks below were written by Hector Zenil
 *
 *  This code was developed for an experiment on algorithmic complexity (in joint with Jean-Paul Delahaye). It was designed to 
 *  be and remain as short and simple as possible. There is neither consistency verification of the rules nor any error catching. 
 *  If the number of parameters or type is incorrect the program retrieves a 'Bus Error'. 
 * 
 *  Any improvements or suggestions keeping simplicity and following the described formalism are welcome 
 *  (visit http://zenil.mathrix.org to get my updated current email)
 *  
 *  This Turing machine implementation follows accurately the formalism of the busy beaver competitions.
 *  
 *  Formalism:
 *  A Turing machine is a 5-tuple of sets: M = (Q, Sigma, delta, q0, qH)
 *   where Q is finite set of states including q0 and qH
 *         Sigma is a finite set of input symbols (there is no special 'blank' character, the background is a list of zeros)
 *         delta is a transition mapping Q-{qH} X Sigma to Q X Sigma X {1,-1,0}
 *         q0 is the initial state (1)
 *         qH is the halting state (0)
 *         F is a finite set of final states
 * 
 *  Input syntax:
 *     TuringMachine n transition_rules
 *  with 'n' a long integer and 'transition_rules' a list of transition rules as integers.
 *  'n' is the max number of steps 
 *  
 *  A transition is a five tuple:
 *  from-state  input-symbol  to-state  symbol-to-write  move
 *  Every input entry must be an integer. The number of rules are assumed to be 2n with n the number of states.
 *  
 * The transition rules are flattened 5-tuples of the form: 
 *  's_i_1' 'c_i_1' 's_n_1' 'c_n_1' 'd(1|-1)' ... 's_i_k' 'c_i_k' 's_n_k' 'c_n_k' 'd(1|-1)'
 *  for k rules (the number of rules is s*k with s the total number of states and the number of symbols)
 *  each rule encoded as:
 *  s_i from-state (1,...j) (the starting state is always the state '1')
 *  c_i input-symbol (0|1, otherwise change 'symbols' variable)
 *  s_i to-state (1,...,j) (the halting state is always state '0')
 *  c_n symbol-to-write 
 *  d the head direction, either left ('-1'), right ('1') or none ('0' when a transition rule enters into the halting state)
 *  
 * compile with  GNU g++  2.8.1 or later or Microsoft 6.0 or later (ANSI/ISO C++)
 * g++ -o TuringMachine TuringMachine.cpp    or   cl /GX /ML TuringMachine.cpp
 *   
 * Input example:
 * bash$./TuringMachineD2v3 6 1 0 2 1 -1 1 1 2 1 1 2 0 1 1 1 2 1 0 1 0
 * 
 * where '6' is the max number of steps and '1 0 2 1 -1 1 1 2 1 1 2 0 1 1 1 2 1 0 1 0' are the four transition rules according to the 
 * busy beaver for 2 states, as an example.
 * 
 * By default the input tape is filled with enough zeros (determined by the max given number of steps) to guarantee that 
 * the head never reachs the tape boundary. The head is always placed in the middle, so the tape is 2*t in length. If 2*t
 * is even the head is placed at any of the 2 cells in the center of the tape (in the worst of the cases the head will reach
 * the left or right tape boundary (case in which certainly will never halt).
 *  
 * Should you decide to start with another init configuration, delete the tape content intitialization code and define any other.
 * 
 * Output syntax:
 * the output string written in the tape and bounded by the places reached by the machine head followed by a new line with the number
 * of steps before halting.
 * 
 * If the machine does not halt, the output string is the number of symbols if the machine does not halt up to the given 
 * number of steps and the given rule input running over an empty tape. In the case of 2-state Turing machines, '2' is 
 * the output string that will be printed in this case. 
 * 
 * Examples (corresponding to the input example above):
 * 
 * bash$./TuringMachine 10 1 0 2 1 -1 1 1 2 1 1 2 0 1 1 1 2 1 0 1 0
 * 1111
 * 6
 * 
 * where, as explained before, '1111' is the output string determined by the places over which the machine head passed over,
 * and '6' the number of steps before halting. This particular machine is known as the busy beaver for s = 2, with s 
 * the number of states of the Turing machine.
 * 
 * More examples:
 * 
 * bash$./TuringMachine 21 1 0 2 1 1 1 1 0 1 0 2 0 2 1 -1 2 1 3 0 1 3 0 3 1 -1 3 1 1 1 -1
 * 11111
 * 21
 * bash$
 *  
 * bash$./TuringMachine 107 1 0 2 1 1 1 1 2 1 -1 2 0 1 1 -1 2 1 3 0 -1 3 0 0 1 0 3 1 4 1 -1 4 0 4 1 1 4 1 1 0 1
 * 10111111111111
 * 107
 * bash$
 *   
 * which are the busy beavers for n=3 and n-4 respectively.
 *
 * More examples:
 *
 * bash$ ./TuringMachine 100 1 0 2 1 -1 1 1 2 1 1 2 1 1 1 1 2 1 0 1 -1
 * 2
 * 100
 * 
 * The above Turing machine does not halt after 100 steps (actually it will never halt, one can say it from the rule
 * specification, but also because busy beaver values are known for Turing machines with 2 states. Therefore the output
 * after running this machine is 2 (the number of rules and their specification determine that the Turing machine has 2
 * states (and 2 symbols). 
 *
 * The number of rules given to the Turing machine must be always n*k. From the number of rules the machine determines 
 * the number of states and symbols.
 *
 * The number of steps, and the number and specification of the rules (and then the number of symbols and states) is 
 * constrained only by the variable types of the program code (int).
 *
 */

#include <iostream>
#include <stdlib.h>
using namespace std;

#define ATRANS(j) atoi(argv[5*i+j+2])

int main(int argc, char ** argv)
{
   int  i, iState=1;
   int *aTape;
   int  iMaxSteps = atoi(argv[1]);
   int  iStates  = ((argc - 2) / 5) / 2;
   long iMaxTape=0, iMinTape, iHead; 

   aTape  = (int *) calloc(2*iMaxSteps+1, sizeof(int));

   iMinTape = iHead  = iMaxSteps/2 -1;
   
   while(iState !=0 && (iMaxSteps--)>0) 
   {
      for(i=0;i<iStates*2;i++)
      {
         if (ATRANS(0) == iState && ATRANS(1) == aTape[iHead])
         {
            iState       = ATRANS(2);
            aTape[iHead] = ATRANS(3);
            iHead       += ATRANS(4);
            if (iHead<iMinTape) iMinTape = iHead;
            if (iHead>iMaxTape) iMaxTape = iHead;
            break;
         }
      }
   }

   /* Printing the output */
   if (iState==0) 
     for (i=iMinTape; i<=iMaxTape; i++) 
       cout << aTape[i];
   else cout << "2";

   cout << "\n" << (atoi(argv[1])-iMaxSteps);
   
   free(aTape);
   
   return 0;
}
