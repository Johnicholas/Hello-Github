# This is taken from:
# http://codegolf.stackexchange.com/questions/1864/simulate-a-minsky-register-machine-i
# Peter Taylor was the author of the question.
# Keith Randall was the author of the answer that I took and modified.
#
# Small changes (mostly making it bigger and worse) by Johnicholas

import sys, os, shlex

GetToken= shlex.shlex(sys.stdin).get_token

Init= ''
Flow= ''

Vars= {}

def JumpTo( x ):
  if x[0] != '"':
    return 'goto ' + x + ';\n'
  else: 
    return 'puts(' + x + ');\ngoto done;\n'

while 1:
  Label= GetToken()
  c= GetToken()
  if c == '':
    break
  if c == ':':
    var= GetToken()
    d= GetToken()
    Vars[var]= 1
    Flow += Label + ':\n'
    if d == '+':
      Flow += var + '++;\n'
      Flow += JumpTo(GetToken())
    else:
      assert d == '-'
      Flow += 'if (' + var + ' > 0) {\n'
      Flow += var + '--;\n'
      Flow += JumpTo(GetToken())
      Flow += '} else {'
      Flow += JumpTo(GetToken())
      Flow += '}'
  else:
    assert c == '='
    # in this circumstance, Label is actually a var
    Init += Label + '=' + GetToken() + ';\n'


sys.stdout.write('#include <stdio.h>\n')

sys.stdout.write('int ' + ', '.join(Vars) + ';\n')

sys.stdout.write('int main() {\n')

sys.stdout.write(Init)

sys.stdout.write(Flow)

sys.stdout.write('done:\n')

# report the ending values of the variables
sys.stdout.write('printf("')
sys.stdout.write(' '.join(map(lambda x: x + '=%d', sorted(Vars))))
sys.stdout.write('\\n", ')
sys.stdout.write(', '.join(sorted(Vars)))
sys.stdout.write(');\n')

sys.stdout.write('}\n')
