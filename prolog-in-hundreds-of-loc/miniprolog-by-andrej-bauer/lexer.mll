(** Taken from Andrej Bauer's PL Zoo, specifically andrej.com/plzoo/html/miniprolog.html *)
(* My (Johnicholas's) understanding is the source code is released under the BSD open source license. *)

{
  open Parser
  open Lexing

  let incr_linenum lexbuf =
    let pos = lexbuf.lex_cur_p in
    lexbuf.lex_cur_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol pos.pos_cnum;
    }
}
let const = ['a'-'z'] [ '_' 'a'-'z' 'A'-'Z' '0'-'9']*
let var = ['A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*


rule token = parse
   '#' [^'\n']* '\n' { incr_linenum lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | "$use"         { USE }
  | "$quit"         { QUIT }
  | "?-"            { GOAL }
  | ":-"            { FROM }
  | ";;"            { SEMICOLON2 }
  | "true"          { TRUE }
  | '\"'[^'\"']*'\"' { let str = lexeme lexbuf in
                       STRING (String.sub str 1 (String.length str - 2)) }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { COMMA }
  | '.'             { PERIOD }
  | const           { CONST ( lexeme lexbuf ) }
  | var             { VAR (lexeme lexbuf ) }
  | eof             { EOF }

 {
 }
