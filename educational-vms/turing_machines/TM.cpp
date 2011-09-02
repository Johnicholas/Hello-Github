/*
 *  TuringMachine.cpp simulates a universal Turing Machine
 *
 *  This code is a modified version of Hector Zenil's code 
 *
 *  Released under the GNU GENERAL PUBLIC LICENSE 
 *   ( http://www.gnu.org/licenses/gpl-3.0.txt )
 *
 *  Written by John Tromp 12-24-08
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
 * Edited slightly (reformatting, to make it more readable, though longer) by Johnicholas Hines, 2011.
*/
 
#include <iostream>

#define X atoi(*++v)

using namespace std;

int main(int i, char**v) {
  int s=1;
  int nt=1+i/10;
  int ns[nt];
  int d[nt];
  int sl=X;
  int t[2*sl];
  int l;
  int r;
  int h;
  for (i/=5; i--; d[h]=X)
    ns[h=2*X+X] = 2*X+X; // read transitions
  for (i=2*sl; i;)
    t[--i]=0; // zero tape
  for (l=r=h=sl; s && sl--; l=min(l,h+=d[s]), r=max(r,h), s=ns[s]/2, i++)
    t[h]=ns[s=2*s+t[h]]&1; // simulate tm
  if (s)
    cout << 2;
  else
    while (l<=r)
      cout << t[l++];
  cout << endl << i << endl;
  return 0;
}
