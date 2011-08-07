#include <assert.h>

// This is the part of the syntax that occurs
// inside the .OUT args, alone or after
// 
void out1() {
  if (istoken(TOKEN_LABEL1)) {
    emit(OP_GN1);
  } else if (istoken(TOKEN_LABEL2)) {
    emit(OP_GN2);
  } else if (istoken(TOKEN_STAR)) {
    emit(OP_C1);
  } else if (istoken(TOKEN_STRING)) {
    emit_op(OP_CL, lexval);
  } else {
    assert(false);
  }
}

// examples might be ".OUT('GN1')" or ".OUT(.LABEL *1)"?
// I think I have a typo or three:
// OUTPUT = ('.OUT' '('
// \$ OUT1 ')' / '.LABEL' .OUT('LB') OUT1) .OUT('OUT') .,
void output() {
  istoken(TOKEN_OUT);
  istoken(TOKEN_OPEN_PAREN);
  is_token(TOKEN_LABEL);
  emit(OP_LB);
  out1();
  emit(OP_OUT);
}

void ex3() {
  if (is_token(TOKEN_ID)) {
    emit_op(OP_CLL, lexval);
  } else if (is_token(TOKEN_STRING)) {
    emit_op(OP_TST, lexval);
  } else if (is_token(TOKEN_ID_LITERAL)) {
    emit(OP_ID);
  } else if (is_token(TOKEN_NUMBER)) {
    emit(OP_NUM);
  } else if (is_token(TOKEN_STRING)) {
    emit(OP_SR);
  } else if (is_token(TOKEN_OPEN_PAREN)) {
    ex1();
    is_token(TOKEN_CLOSE_PAREN);
  } else if (is_token(TOKEN_EMPTY)) {
    op(OP_SET);
  } else if (is_token(TOKEN_SEQ)) {
    // output label *1
    ex3();
    emit_op(OP_BT, label1);
    emit(OP_SET);
  } 
}

// EX2 is 
void ex2() {
  if (ex3()) {
    emit_op(OP_BF, label1);
  } else {
    output();
    emit(OP_BE);
  }
  while (
	 if (ex3()) {
	   emit(OP_BE);
	 } else {
	   output();
	   // output label *1
	 }

// EX1 is EX2
// followed by a sequence of '/' ex2
void ex1() {
     ex2();
     while (is_token(TOKEN_SLASH)) {
     	   emit_op(OP_BT, *1); // that is, if the previous one succeeded, branch to a label
     	   ex2();
     }
     // output label *1?
}

// ST is is an ID followed by "="
// followed by an EX1
// followed by a semicolon
// when you've got that far, output "return"
void st() {
     is_token(TOKEN_ID);
     // store the current position in the output bytecode
     // indexed by the current lexval?
     is_token(TOKEN_EQUALS);
     ex1();
     is_token(TOKEN_SEMI);
     emit(OP_RETURN);
}


//a program is ".SYNTAX" followed by an id
//when you've got that far, print out 'ADR' with arg that id
//Then a program is a sequence of ST
//followed by ".END".
//When you've got that far, print out 'END'

void program() {
    is_token(TOKEN_SYNTAX);
    is_token(TOKEN_ID);
    emit_op(OP_ADR, lexval);
    while (!is_token(TOKEN_END)) {
    	  st();
    }
    emit(OP_END);
}

