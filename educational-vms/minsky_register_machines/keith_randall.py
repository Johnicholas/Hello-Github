import sys, os, shlex

G= shlex.shlex(sys.stdin).get_token

A= ''
B= ''

C= 'done:'

V= {}

def J( x ):
  if x[0] != '"':
    return 'goto ' + x + ';\n'
  else: 
    return 'puts(' + x + ');\ngoto done;\n'

while 1:
  L= G()
  c= G()
  if c == '':
    break
  if c == ':':
    v= G()
    d= G()
    V[v]= 1
    B += L + c + v + d + d + ';\n'
    if d == '+':
      B += J(G())
    else:
      B += 'if(' + v + '>= 0) {\n'
      B += J(G())
      B += '} else {'
      B += v + '= 0;\n'
      B += J(G())
      B += '}'
  else:
    A += L + c + G() + ';\n'

C += 'printf("'
first= True
for v in sorted(V.keys()):
  if first:
    first= False
  else:
    C += ' '
  C += (v + '=%d')
C += '\\n"'
for v in sorted(V.keys()):
  C += (', ' + v)
C += ');\n'

sys.stdout.write('#include <stdio.h>\n');
sys.stdout.write('int ' + ', '.join(V) + ';\nmain(){' + A + B + C + '}\n')


