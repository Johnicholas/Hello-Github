#include <stdio.h> // TODO(johnicholas.hines@gmail.com): Maybe also curses?
#include <string.h> // for strlen

// Copyright 1986 - MicroExpert Systems
// Box 430 R.D. 2
// Nassau, NY 12123
//
// VTPROLOG implements the data base searching and pattern matching of
// PROLOG. It is described in "PROLOG from the Bottom Up" in issues
// 1 and 2 of AI Expert.
// This program has been tested using Turbo ver 3.01A on an IBM PC. It has
// been run under both DOS 2.1 and Concurrent 4.1 .
// We would be pleased to hear your comments, good or bad, or any applications
// and modifications of the program. Contact us at:
//
//   AI Expert
//   CL Publications Inc.
//   650 Fifth St.
//   Suite 311
//   San Francisco, CA 94107
//
// or on the AI Expert BBS. Our id is BillandBev Thompson. You can also
// contact us on BIX, our id is bbt.
//
// Bill and Bev Thompson
//
//
// Edits are copyright 2011 IDEXX Laboratories
//
// However, the translation to C has not been tested anywhere,
// and should NOT be trusted.
//
// Johnicholas Hines(johnicholas.hines@gmail.com)

typedef int boolean;
const boolean debug= 0;
const char back_space= 0x08; // in ascii
const char tab= '\t';
// const char eof_mark // TODO(johnicholas.hines@gmail.com): eof isn't a character, really, in the C/Unix world
const char esc= 0x1B; // in ascii
const char quote_char= '\'';
// const char left_arrow= 75; //TODO(johnicholas.hines@gmail.com): WTF?
//const char end_key= 79; // TODO(johnicholas.hines@gmail.com): WTF?
//const char del_line= ^X; // TODO(johnicholas.hines@gmail.com): I don't know what they mean by this
//const char return= ^M;  // TODO(johnicholas.hines@gmail.com): I don't know what they mean by this
const char bell= '\b';

typedef int counter;
typedef char string80[80];
typedef char string132[132];
typedef char string255[255];

// text_file = text ; // TODO(johnicholas.hines@gmail.com): What does this mean? is it a FILE*? or maybe an fd?

// char_set = SET Of char ; // TODO(johnicholas.hines@gmail.com): What does this mean?

typedef enum node_type {
  CONS_NODE,
  FUNC,
  VARIABLE,
  CONSTANT,
  FREE_NODE
} node_type;

typedef struct node* node_ptr;
struct node {
  boolean in_use;
  node_type tag;
  union {
    struct {
      node_ptr tail_ptr;
      node_ptr head_ptr;
    } cons_node;
    struct {
      // no fields?
    } func;
    struct {
      // no fields?
    } constant;
    struct {
      string80 string_data;
    } variable;
    struct {
      node_ptr next_free;
      counter block_cnt;
    } free_node;
  } u;
};
// node is the basic allocation unit for lists. The fields are used as
// follows:
//
// in_use     - in_use = false tells the garbage collector that this node
//              is available for re-use.
// tag        - which kind of node this is.
// cons_node  - cons_nodes consist of two pointers. one to the head (first item)
//              the other to the rest of the list. They are the "glue" which
//              holds the list together. The list (A B C) would be stored as
//                 -------         --------          --------
//                 | .| . |----->  |  .| . |------> |  .| . |---> NIL
//                 --|-----         --|------        --|-----
//                   |                |                |
//                   V                V                V
//                   A                B                C
//
//               The boxes are the cons nodes, the first part of the box
//               holds the head pointer, then second contains the tail.
//  constant   - holds string values, we don't actually use the entire 80
//               characters in most cases.
//  variable   - also conatins a string value, these nodes will be treated as
//               PROLOG variables rather than constants.
//  free_node  - the garbage collector gathers all unused nodes and puts
//               them on a free list. It also compacts the free space into
//               contiguous blocks. next_free points to the next free block.
//               block_cnt contains a count of the number of contiguous 8 byte free
//               blocks which follow this one.


string132 line, saved_line;
string80 token;
// text_file source_file // TODO(johnicholas.hines@gmail.com): Is this a FILE*?
boolean error_flag, in_comment;
// char_set delim_set, text_chars; // TODO(johnicholas.hines@gmail.com): Are these const char* as used, for example, in strtok?
node_ptr data_base, initial_heap, free, saved_list, HeapPtr;
float total_free;

// The important globals are:
// source_file  - text file containing PROLOG statements.
// line         - line buffer for reading in the text file
// saved_list   - list of all items that absolutely must be saved if garbage
//                collection occurs. Usually has at least the data_base and
//                the currents query attached to it.
// initial_heap - the value of the heap pointer at the start of the program.
//                used by the garbage collector
// free         - the list of free nodes.
// total_free   - total number of free blocks on the free list.
// data_base    - a pointer to the start of the data base. It points to a
//                node pointing to the first sentence in the data base. Nodes
//                pointing to sentences are linked together to form the data
//                base.
// delim_set    - set of characters which delimit tokens.



// ----------------------------------------------------------------------
//      Utility Routines
// ----------------------------------------------------------------------

void noise()
  // Make a noise on the terminal - used for warnings.
{
  putchar(bell);
}
// noise

//int vtprolog_open(text_file f, string80 f_name)
//// open a file - returns true if the file exists and was opened properly
////   f      - file pointer
////   f_name - external name of the file
//{
//  // assign(f,f_name) ; // TODO(johnicholas.hines@gmail.com): This should become something using posix's, but what mode? reading? writing? open(f, f_name, "r")
//  // reset(f) ;
//   return // ioresult == 0 // TODO(johnicholas.hines@gmail.com): What is ioresult?
//}
//// vtprolog_open


// Johnicholas says: I think this is trying to do something like posix's isatty
//Function is_console(Var f : text_file) : boolean ;
// return true if f is open on the system console
//   for details of fibs and fib_ptrs see the Turbo Pascal ver 3.0 reference
//   manual chapter 20. This should work under CP/M-86 or 80, but we haven't
//   tried it. *)
//
//Type 
//  fib = ARRAY [0 .. 75] Of byte ;
//
//Var 
//  fib_ptr : ^fib ;
//  dev_type : byte ;
//Begin
//  fib_ptr := addr(f) ;
//  dev_type := fib_ptr^[2] And $07 ;
//  is_console := (dev_type = 1) Or (dev_type = 2) ;
//End ;
//(* is_console *)


void strip_leading_blanks(string80 s)
{
  if (strlen(s) > 0)
    {
      if (s[0] == ' ' || s[0] == tab)
	{
	  // TODO(johnicholas.hines@gmail.com): Recursion? Quadratic time complexity? Isn't there a better way?
	  // delete(s,1,1) ;
	  // strip_leading_blanks(s) ;
	}
    }
}
// strip_leading_blanks


void vtprolog_toupper(string80 s)
// converts s to upper case
{
  int i;
  if (strlen(s) > 0) 
    {
      for (i= 0; i < strlen(s); ++i) {
	s[i]= toupper(s[i]);
      }
    }
}
// vtprolog_toupper

boolean is_number(string80 s)
  // checks to see if s contains a legitimate numerical string.
  // It ignores leading and trailing blanks
{
  float num;
  int code;
  strip_trailing_blanks(s);
  strip_leading_blanks(s);
  if (strcmp(s, "") != 0) {
    val(s, num, code);
  } else {
    code= -1;
  }
  return code == 0;
}
// is_number

node_ptr head(node_ptr list)
  // returns a pointer to the first item in the list
  // if the list is empty, returns NULL.
{
  if (list == NULL) {
    return NULL;
  } else {
    return list->u.cons_node.head_ptr;
  }
}
// head

node_ptr tail(node_ptr list)
  // returns a pointer to a list starting at the second item in the list.
  // Note - tail( (a b c) ) points to the list (b c), but
  //        tail( ((a b) c d) ) points to the list (c d) .
{
  if (list == NULL) {
    return NULL;
  } else {
    switch (list->tag) {
    case CONS_NODE: return list->u.cons_node.tail_ptr;
    case FREE_NODE: return list->u.free_node.next_free;
    default: return NULL;
    }
  }
}
// tail

counter allocation_size(counter x)
  // Turbo 3.0 allocates memory in 8 byte blocks, this routine calculates the
  // actual number of bytes returned for a request of x bytes.
  // TODO(johnicholas.hines@gmail.com): this is probably completely irrelevant
{
  return (((x - 1) / 8) + 1) * 8;
}
// allocation_size

/*
counter node_size()
  // calculates the base size of a node. Add the rest of the node to this
  // to get the actual size of a node.
// TODO(johnicholas.hines@gmail.com): Remove this, it's probably completely irrelevant.
Begin
  node_size := 2 * sizeof(node_ptr) + sizeof(boolean) + sizeof(node_type) ;
End ;
(* node_size *)


Function normalize(pt : node_ptr) : node_ptr ;

(* returns a normalized pointer. Pointers are 32 bit addresses. The first
     16 bits contain the segment number and the second 16 bits contain the
     offset within the segment. Normalized pointers have offsets in the range
     $0 to $F (0 .. 15)    *)

Var 
  pt_seg,pt_ofs : integer ;
Begin
  pt_seg := seg(pt^) + (ofs(pt^) Div 16) ;
  pt_ofs := ofs(pt^) Mod 16 ;

(* Johnicholas says: so ptr is some primitive that builds a (32-bit) pointer from a pair of (16-bit) ints?
      *  Maybe I just need to cast the FarPointer to the appropriate type? *)
  normalize := node_ptr(ptr(pt_seg, pt_ofs)) ;
End ;
(* normalize *)


Function string_val(list : node_ptr) : string80 ;

(* returns the string pointed to by list. If list points to a number
     node, it returns a string representing that number *)

Var 
  s : string[15] ;
Begin
  If list = Nil
    Then string_val := ''
  Else If list^.tag In [constant,variable,func]
         Then string_val := list^.string_data
  Else string_val := '' ;
End ;
(* string_val *)


Function tag_value(list : node_ptr) : node_type ;
  (* returns the value of the tag for a node.     *)
Begin
  If list = Nil
    Then tag_value := free_node
  Else tag_value := list^.tag ;
End ;
(* tag_value *)


Procedure print_list(list : node_ptr) ;

(* recursively traverses the list and prints its elements. This is
     not a pretty printer, so the lists may look a bit messy.  *)

Var 
  p : node_ptr ;
Begin
  If list <> Nil
    Then
    Case list^.tag Of 
      constant,
      func,
      variable  : write(string_val(list),' ') ;
      cons_node :
                  Begin
                    write('(') ;
                    p := list ;
                    While p <> Nil Do
                      Begin
                        print_list(head(p)) ;
                        p := tail(p) ;
                      End ;
                    write(') ') ;
                  End ;
    End ;
End ;
(* print_list *)


Procedure get_memory(Var p : node_ptr ; size : counter) ;

(* On exit p contains a pointer to a block of allocation_size(size) bytes.
     If possible this routine tries to get memory from the free list before
     requesting it from the heap *)

Var 
  blks : counter ;
  allocated : boolean ;

Procedure get_from_free(Var list : node_ptr) ;

(* Try and get need memory from the free list. This routine uses a
      first-fit algorithm to get the space. It takes the first free block it
      finds with enough storage. If the free block has more storage than was
      requested, the block is shrunk by the requested amount.  *)
Begin
  If list <> Nil
    Then
    If list^.block_cnt >= (blks - 1)
      Then
      Begin
        p := normalize(node_ptr(ptr(seg(list^), ofs(list^) +
             (list^.block_cnt - blks + 1) * 8))) ;
        If list^.block_cnt = blks - 1
          Then list := list^.next_free
        Else list^.block_cnt := list^.block_cnt - blks ;
        allocated := true ;
        total_free := total_free - (blks * 8.0) ;
      End
  Else get_from_free(list^.next_free) ;
End ;
(* get_from_free *)

Begin
  blks := ((size - 1) Div 8) + 1 ;
  allocated := false ;
  get_from_free(free) ;
  If Not allocated
    Then getmem(p,blks * 8) ;
End ;
(* get_memory *)


Function alloc_str(typ : node_type ; s : string80) : node_ptr ;

(* Allocate storage for a string and return a pointer to the new node.
     This routine only allocates enough storage for the actual number of
     characters in the string plus one for the length. Because of this,
     concatenating anything to the end of a string stored in a symbol node
     will lead to disaster. Copy the string to a new string do the
     concatenation and then allocate a new node.  *)

Var 
  pt : node_ptr ;
Begin
  get_memory(pt,allocation_size(sizeof(node_type) + sizeof(boolean) +
  length(s) + 1)) ;
  pt^.tag := typ   ;
  pt^.string_data := s ;
  alloc_str := pt ;
End ;
(* alloc_str *)


Function cons(new_node,list : node_ptr) : node_ptr ;

(* Construct a list. This routine allocates storage for a new cons node.
     new_node points to the new head of the list. The tail pointer of the
     new node points to list. This routine adds the new cons node to the
     beginning of the list and returns a pointer to it. The list described
     in the comments at the beginning of the program could be constructed
     as cons(alloc_str('A'),cons(alloc_str('B'),cons(alloc_str('C'),NIL))). *)

Var 
  p : node_ptr ;
Begin
  get_memory(p,allocation_size(node_size)) ;
  p^.tag := cons_node ;
  p^.head_ptr := new_node ;
  p^.tail_ptr := list ;
  cons := p ;
End ;
(* cons *)


Function append_list(list1,list2 : node_ptr) : node_ptr ;

(* Append list2 to list1. This routine returns a pointer to the
     combined list. Appending is done by consing each item on the first
     list to the second list. This routine is one of the major sources of
     garbage so if garbage collection becomes a problem, you may want to
     rewrite it. *)
Begin
  If list1 = Nil
    Then append_list := list2
  Else append_list := cons(head(list1),append_list(tail(list1),list2)) ;
End ;
(* append_list *)


Function list_length(list : node_ptr) : counter ;
  (* returns the length of a list.
     Note - both (A B C) and ( (A B) C D) have length 3.   *)
Begin
  If list = Nil
    Then list_length := 0
  Else list_length := 1 + list_length(list^.tail_ptr) ;
End ;
(* list_length *)


Procedure collect_garbage ;

(* This routine is specific to Turbo Pascal Ver 3.01
     It depends upon the fact that Turbo allocates memory in 8 byte blocks
     on the PC. If you recompile this program on another system be very
     careful with this routine.
     Garbage collection proceeds in three phases:
      unmark  - free all memory between the initial_heap^ and the current
                top of the heap.
      mark    - mark everything on the saved_list as being in ues.
      release - gather all unmarked blocks and put them on the free list.
     The collector displays a '*' on the screen to let you know it is
      operating.  *)

Function lower(p1,p2 : node_ptr) : boolean ;
   (* returns true if p1 points to a lower memory address than p2 *)
Begin
  p1 := normalize(p1) ;
  p2 := normalize(p2) ;
  lower := (seg(p1^) < seg(p2^)) Or
           ((seg(p1^) = seg(p2^)) And (ofs(p1^) < ofs(p2^))) ;
End ;
(* lower *)

Procedure mark(list : node_ptr) ;

(* Mark the blocks on list as being in use. Since a node may be on several
      lists at one time, if it is already marked we don't continue processing
      the tail of the list. *)
Begin
  If list <> Nil
    Then
    Begin
      If Not list^.in_use
        Then
        Begin
          list^.in_use := true ;
          If list^.tag = cons_node
            Then
            Begin
              mark(head(list)) ;
              mark(tail(list)) ;
            End ;
        End ;
    End ;
End ;
(* mark *)

Procedure unmark_mem ;

(* Go through memory from initial_heap^ to HeapPtr^ and mark each node
      as not in use. The tricky part here is updating the pointer p to point
      to the next cell. *)

Var 
  p : node_ptr ;
  string_base,node_allocation : counter ;
Begin
  string_base := sizeof(node_type) + sizeof(boolean) ;
  p := normalize(initial_heap) ;
  node_allocation := allocation_size(node_size) ;

(* Johnicholas says: HeapPtr is something from the TP runtime - is there something analogous in the fpc runtime?
 *  Are we walking over all of memory, or just some specific region? *)
  While lower(p, HeapPtr) Do
    Begin
      p^.in_use := false ;
      Case p^.tag Of 
        cons_node : p := normalize(node_ptr(ptr(seg(p^),ofs(p^) + node_allocation))) ;
        free_node : p := normalize(node_ptr(ptr(seg(p^),ofs(p^) + (p^.block_cnt + 1) * 8))) ;
        func,
        constant,
        variable  : p := normalize(node_ptr(ptr(seg(p^),
                         ofs(p^) +
                         allocation_size(string_base +
                         length(p^.string_data) + 1)))) ;
      End ;
    End ;
End ;
(* unmark_mem *)

Procedure release_mem ;

(* This procedure does the actual collection and compaction of nodes.
      This is the slow phase of garbage collection because of all the pointer
      manipulation.  *)

Var 
  heap_top : node_ptr ;
  string_base,node_allocation,string_allocation,block_allocation : counter ;

Procedure free_memory(pt : node_ptr ; size : counter) ;

(* return size bytes pointed to by pt to the free list. If pt points to
       a block next to the head of the free list combine it with the top
       free node. total_free keeps track of the total number of free bytes. *)

Var 
  blks : counter ;
Begin
  blks := ((size - 1) Div 8) + 1 ;
  pt^.tag := free_node ;
  If normalize(node_ptr(ptr(seg(pt^),ofs(pt^) + 8 * blks))) = free
    Then
    Begin
      pt^.next_free := free^.next_free ;
      pt^.block_cnt := free^.block_cnt + blks ;
      free := pt ;
    End
  Else If normalize(node_ptr(ptr(seg(free^),ofs(free^) + 8 * (free^.block_cnt + 1)))) =
          normalize(pt)
         Then free^.block_cnt := free^.block_cnt + blks
  Else
    Begin
      pt^.next_free := free ;
      pt^.block_cnt := blks - 1 ;
      free := pt ;
    End ;
  total_free := total_free + (blks * 8.0) ;
End ;
(* free_memory *)

Procedure do_release ;
    (* This routine sweeps through memory and checks for nodes with
       in_use = false. *)

Var 
  p : node_ptr ;
Begin
  p := normalize(initial_heap) ;
  While lower(p,heap_top) Do
    Case p^.tag Of 
      cons_node :
                  Begin
                    If Not p^.in_use
                      Then free_memory(p,node_size) ;
                    p := normalize(node_ptr(ptr(seg(p^),ofs(p^) + node_allocation))) ;
                  End ;
      free_node :
                  Begin
                    block_allocation := (p^.block_cnt + 1) * 8 ;
                    free_memory(p,block_allocation) ;
                    p := normalize(node_ptr(ptr(seg(p^),ofs(p^) + block_allocation))) ;
                  End ;
      func,
      constant,
      variable  :
                  Begin
                    string_allocation := allocation_size(string_base +
                                         length(p^.string_data) + 1) ;
                    If Not p^.in_use
                      Then free_memory(p,string_base + length(p^.string_data)
                      + 1) ;
                    p := normalize(node_ptr(ptr(seg(p^),ofs(p^) + string_allocation))) ;
                  End ;
    End ;
End ;
(* do_release *)

Begin
  free := Nil ;
  total_free := 0.0 ;
  heap_top := HeapPtr ;
  string_base := sizeof(node_type) + sizeof(boolean) ;
  node_allocation := allocation_size(node_size) ;
  do_release ;
End ;
(* release_mem *)

Begin
  write('*') ;
  unmark_mem ;
  mark(saved_list) ;
  release_mem ;
  write(back_space) ;

  ClrEol ;

End ;
(* collect_garbage *)


Procedure test_memory ;

(* This routine activates the garbage collector, if the the total available
     memory (free_list + heap) is less than a specified amount. Lowering the
     minimum causes garbage collection to be called less often, but if you
     make it too small you may not have enough room left for recursion or any
     temporary lists you need. Using 10000 is probably being overly
     cautious.   *)
Begin
     (* Johnicholas says: is memavail something in the legacy context? yes
      *  The fpc docs say: The MemAvail and MaxAvail functions are no longer available.
      *  On modern operating systems, the idea of "Available Free Memory" is not valid for an application.
      *  The reasons are:
      *  1. One processor cycle after an application asked the OS how much memory was free, another application
      *  may have allocated everything.
      *  2. It is not clear what "free memory" means [...snip...]

      If (memavail * 16.0) + total_free < 10000 
      Then collect_garbage ;
      *)
End ;
(* test_memory *)


Procedure wait ;
  (* Just like it says. It waits for the user to press a key before
     continuing. *)

Var 
  ch : char ;
Begin
  writeln ;
  writeln ;
  write('Press any key to continue. ') ;
  read(Input,ch) ;
  write(return) ;

  ClrEol ;

End ;
(* wait *)



(* ------------------------------------------------------------------------
        End of utility routines
   ------------------------------------------------------------------------ *)

Procedure read_kbd(Var s : string80) ;
  (* Read a line from the keyboard *)
Begin
  write('-> ') ;
  readln(s) ;
End ;
(* read_kbd *)


Procedure read_from_file(Var f : text_file) ;

(* Read a line from file f and store it in the global variable line.
     It ignores blank lines and when the end of file is reached an
     eof_mark is returned. *)

Procedure read_a_line ;
Begin
    (*$I- *)
  readln(f,line) ;
    (*$I+ *)
  If ioresult <> 0
    Then line := eof_mark
  Else If eof(f)
         Then line := concat(line,eof_mark) ;
End ;
(* read_a_line *)

Begin
  line := '' ;
  If is_console(f)
    Then read_kbd(line)
  Else read_a_line ;
  If in_comment
    Then
    If pos('*)',line) > 0
      Then
      Begin
        delete(line,1,pos('*)',line) + 1) ;
        in_comment := false ;
      End
  Else read_from_file(f) ;
  saved_line := line ;
End ;
(* read_from_file *)


Procedure get_token(Var t_line : string132 ; Var token : string80) ;

(* Extract a token from t_line. Comments are ignored. A token is
     a string surrounded by delimiters or an end of line. Tokens may
     contain embedded spaces if they are surrounded by quote marks *)

Procedure get_word ;

Var 
  done : boolean ;
  cn : integer ;
  len : byte ;
Begin
  cn := 1 ;
  len := length(t_line) ;
  done := false ;
  While Not done Do
    If cn > len
      Then done := true
    Else If t_line[cn] In delim_set
           Then done := true
    Else cn := cn + 1 ;
  token := copy(t_line,1,cn-1) ;
  delete(t_line,1,cn-1) ;
End ;
(* get_word *)

Procedure comment ;
Begin
  If pos('*)',t_line) > 0
    Then
    Begin
      delete(t_line,1,pos('*)',t_line)+1) ;
      get_token(line,token) ;
    End
  Else
    Begin
      t_line := '' ;
      token := '' ;
      in_comment := true ;
    End ;
End ;
(* comment *)

Procedure get_quote ;
Begin
  delete(t_line,1,1) ;
  If pos(quote_char,t_line) > 0
    Then
    Begin
      token := concat(quote_char,copy(t_line,1,pos(quote_char,t_line) - 1)) ;
      delete(t_line,1,pos(quote_char,t_line)) ;
    End
  Else
    Begin
      token := t_line ;
      t_line := '' ;
    End ;
End ;
(* get_quote *)

Begin
  strip_leading_blanks(t_line) ;
  If length(t_line) > 0
    Then
    Begin
      If copy(t_line,1,2) = '(*'
        Then comment
      Else If (copy(t_line,1,2) = ':-') Or (copy(t_line,1,2) = '?-')
             Then
             Begin
               token := copy(t_line,1,2) ;
               delete(t_line,1,2) ;
             End
      Else If t_line[1] = quote_char
             Then get_quote
      Else If t_line[1] In delim_set
             Then
             Begin
               token := t_line[1] ;
               delete(t_line,1,1) ;
             End
      Else get_word ;
    End
  Else token := '' ;
End ;
(* get_token *)


Procedure scan(Var f : text_file ; Var token : string80) ;

(* Scan repeatedly calls get_token to retreive tokens. When the
     end of a line has been reached, read_from_file is called to
     get a new line. *)
Begin
  If length(line) > 0
    Then
    Begin
      get_token(line,token) ;
      If token = ''
        Then scan(f,token) ;
    End
  Else
    Begin
      read_from_file(f) ;
      scan(f,token) ;
    End ;
End ;
(* scan *)


Procedure compile(Var source : text_file) ;

(* The recursive descent compiler. It reads tokens until the token
     'EXIT' is found. If the token is '?-', a query is performed, a '@' token
     is the command to read a new file and source statements are read form that
     file, otherwise the token is assumed to be part of a sentence and the rest
     of the sentence is parsed. *)

Procedure error(error_msg : string80) ;

(* Signal an error. Prints saved_line to show where the error is located.
      saved_line contains the current line being parsed. it is required,
      because get_token chews up line as it reads tokens. *)

Procedure runout ;
Begin
  While (token <> '.') And (token <> eof_mark) Do
    scan(source,token) ;
End ;
(* runout *)

Begin
  error_flag := true ;
  writeln ;
  writeln(error_msg) ;
  writeln ;
  writeln(saved_line) ;
  writeln('' : length(saved_line) - length(line) - 1,'^') ; ;
  runout ;
  wait ;
End ;
(* error *)

Procedure goal(Var l_ptr : node_ptr) ;

(* Read a goal. The new goal is appended to l_ptr. Each goal is appended
      to l_ptr as a list. Thus, the sentence 'likes(john,X) :- likes(X,wine) .'
      becomes the list ( (likes john X) (likes X wine) ) *)

Var 
  goal_token : string80 ;

Procedure functor(Var f_ptr : node_ptr ; func_token : string80) ;

(* The current goal is a functor. This routine allocates a node
       to store the functor and then processes the components of the
       functor. On exit, f_ptr points to the list containing the functor
       and its components. func_token contains the functor name. *)

Var 
  c_ptr : node_ptr ;

Procedure components(Var cm_ptr : node_ptr) ;

(* Process the components of the functor. The components are terms
        seperated by commas. On exit, cm_ptr points to the list of
        components. *)

Procedure term(Var t_ptr : node_ptr) ;
      (* Process a single term. The new term is appended to t_ptr. *)

Var 
  t_token : string80 ;

Procedure quoted_str(Var q_ptr : node_ptr) ;
       (* Process a quote *)
Begin
  q_ptr := append_list(q_ptr,cons(alloc_str(constant,
           copy(token,2,length(token) - 1)),
           Nil)) ;
  scan(source,token) ;
End ;
(* quoted_str *)

Procedure varbl(Var v_ptr : node_ptr) ;
       (* The current token is a varaible, allocate a node and return
          a pointer to it. *)
Begin
  v_ptr := append_list(v_ptr,cons(alloc_str(variable,token),Nil)) ;
  scan(source,token) ;
End ;
(* varbl *)

Procedure number(Var n_ptr : node_ptr) ;

(* Numbers are treated as string constants. This isn't particularly
          efficent, but it is easy. *)
Begin
  n_ptr := append_list(n_ptr,cons(alloc_str(constant,token),Nil)) ;
  scan(source,token) ;
End ;
(* handle_number *)

Begin
  If token[1] In ['A' .. 'Z','_']
    Then varbl(t_ptr)
  Else If token[1] = quote_char
         Then quoted_str(t_ptr)
  Else If is_number(token)
         Then number(t_ptr)
  Else If token[1] In ['a' .. 'z']
         Then
         Begin
           t_token := token ;
           scan(source,token) ;
           If token = '('
             Then functor(t_ptr,t_token)
           Else t_ptr := append_list(t_ptr,
                         cons(alloc_str(constant,t_token),Nil)) ;
         End
  Else error('Illegal Symbol.') ;
End ;
(* term *)

Begin
  term(cm_ptr) ;
  If token = ','
    Then
    Begin
      scan(source,token) ;
      components(cm_ptr) ;
    End ;
End ;
(* components *)

Begin
  c_ptr := cons(alloc_str(func,func_token),Nil) ;
  scan(source,token) ;
  components(c_ptr) ;
  If token = ')'
    Then
    Begin
      f_ptr := append_list(f_ptr,cons(c_ptr,Nil)) ;
      scan(source,token) ;
    End
  Else error('Missing '')''.') ;
End ;
(* functor *)

Begin
  If token[1] In ['a' .. 'z',quote_char]
    Then
    Begin
      If token[1] = quote_char
        Then
        Begin
          l_ptr := append_list(l_ptr,
                   cons(cons(alloc_str(constant,
                   copy(token,2,length(token) - 1)),Nil),Nil)) ;
          scan(source,token) ;
        End
      Else
        Begin
          goal_token := token ;
          scan(source,token) ;
          If token = '('
            Then functor(l_ptr,goal_token)
          Else l_ptr := append_list(l_ptr,
                        cons(cons(alloc_str(constant,goal_token),
                        Nil),Nil)) ;
        End
    End
  Else error('A goal must begin with ''a .. z'' or be a quoted string.') ;
End ;
(* goal *)

Procedure tail_list(Var t_ptr : node_ptr) ;

(* Process the tail of a rule. Since the a query is syntactically identical
      to the tail of a rule, this routine is used to compile queries.
      On exit, t_ptr points to the list containing the tail. *)
Begin
  goal(t_ptr) ;
  If token = ','
    Then
    Begin
      scan(source,token) ;
      tail_list(t_ptr) ;
    End ;
End ;
(* tail *)

Procedure rule ;

(* Procees a rule, actually any sentence. If no error occurs the
      new sentence is appended to the data base. *)

Var 
  r_ptr : node_ptr ;

Procedure head_list(Var h_ptr : node_ptr) ;
Begin
  goal(h_ptr) ;
End ;
(* head *)

Begin
  saved_list := cons(data_base,Nil) ;
  test_memory ;
  r_ptr := Nil ;
  head_list(r_ptr) ;
  If token = ':-'
    Then
    Begin
      scan(source,token) ;
      tail_list(r_ptr) ;
    End ;
  If token <> '.'
    Then error('''.'' expected.') ;
  If Not error_flag
    Then data_base := append_list(data_base,cons(r_ptr,Nil)) ;
End ;
(* rule *)

Procedure query ;

(* Process a query. Compile the query, and then call solve to search the
      data base. q_ptr points to the compiled query and solved is a boolean
      indicating whether the query was successfully solved. *)

Var 
  q_ptr : node_ptr ;
  solved : boolean ;

Procedure solve(list,env : node_ptr ; level : counter) ;

(* This is where all the hard work is done. This routine follows the
       steps outlined in the article. list is the query to be soved, env is
       the current environment and level is the recursion level. level can
       only get to 32767, but you'll run out of stack space long before you
       get that far.
       solve saves list and env on the saved list so that they won't be
       destroyed by garbage collection. The data base is always on the
       saved list. At the end of solve, list and env are removed from
       saved_list. *)

Var 
  new_env,p : node_ptr ;

Function look_up(var_str : string80 ; environ : node_ptr) : node_ptr ;

(* Search the environment list pointed to by environ for the variable,
        var_str. If found return a pointer to var_str's binding, otherwise
        return NIL *)

Var 
  found : boolean ;
  p : node_ptr ;
Begin
  p := environ ;
  found := false ;
  While (p <> Nil) And (Not found) Do
    Begin
      If var_str = string_val(head(head(p)))
        Then
        Begin
          found := true ;
          look_up := tail(head(p)) ;
        End
      Else p := tail(p) ;
    End ;
  If Not found
    Then look_up := Nil ;
End ;
(* look_up *)

Procedure check_continue ;

(* Print the bindings and see if the user is satisfied. If nothing
        is printed from the environment, then print 'Yes' to indicate
        that the query was successfully satisfied. *)

Var 
  printed : boolean ;
  ch : char ;

Procedure print_bindings(list : node_ptr) ;

(* Print the bindings for level 0 variables only, intermediate variables
         aren't of interest. The routine recursivley searches for the
         end of the environments list and then prints the binding. This
         is so that variables bound first are printed first. *)

Procedure print_functor(l : node_ptr) ;
FORWARD ;

Procedure print_variable(var_str : string80) ;

(* The varaible in question was bound to another varaible, so look
          up that variable's binding and print it. If a match can't be found
          print '_' to tell the user that the variable is anonymous. *)

Var 
  var_ptr : node_ptr ;
Begin
  var_ptr := look_up(var_str,env) ;
  If var_ptr <> Nil
    Then
    Case tag_value(head(var_ptr)) Of 
      constant  : write(string_val(head(var_ptr)),' ') ;
      variable  : print_variable(string_val(head(var_ptr))) ;
      cons_node : print_functor(head(var_ptr)) ;
    End
  Else write('_ ') ;
End ;
(* print_variable *)

Procedure print_functor (* l : node_ptr *) ;
       (* The variable was bound to a functor. Print the functor and its
          components. *)

Procedure print_components(p : node_ptr) ;

(* Print the components of a functor. These may be variables or
           other functors, so call the approriate routines to print them. *)
Begin
  If p <> Nil
    Then
    Begin
      Case tag_value(head(p)) Of 
        constant  : write(string_val(head(p)),' ') ;
        variable  : print_variable(string_val(head(p))) ;
        cons_node : print_functor(head(p)) ;
      End ;
      If tail(p) <> Nil
        Then
        Begin
          write(',') ;
          print_components(tail(p)) ;
        End ;
    End ;
End ;
(* print_components *)

Begin
  If l <> Nil
    Then
    Begin
      write(string_val(head(l))) ;
      If tail(l) <> Nil
        Then
        Begin
          write('(') ;
          print_components(tail(l)) ;
          write(')') ;
        End ;
    End ;
End ;
(* print_functor *)

Begin
  If list <> Nil
    Then
    Begin
      print_bindings(tail(list)) ;
      If pos('#',string_val(head(head(list)))) = 0
        Then
        Begin
          printed := true ;
          writeln ;
          write(string_val(head(head(list))),' = ') ;
          Case tag_value(head(tail(head(list)))) Of 
            constant  : write(string_val(head(tail(head(list)))),' ') ;
            variable  : print_variable(string_val(head(tail(head(list))))) ;
            cons_node : print_functor(head(tail(head(list)))) ;
          End ;
        End ;
    End ;
End ;
(* print_bindings *)

Begin
  printed := false ;
  print_bindings(env) ;
  If Not printed
    Then
    Begin
      writeln ;
      write('Yes ') ;
    End ;
  Repeat
    read(Input,ch) ;
  Until ch In [return,';'] ;
  solved := (ch = return) ;
  writeln ;
End ;
(* check_continue *)

Function copy_list(list : node_ptr ; copy_level : counter) : node_ptr ;
     (* Copy a list and append the copy_level (recursion level) to all
        variables. *)

Var 
  temp_list,p : node_ptr ;
  level_str : string[6] ;

Procedure list_copy(from_list : node_ptr ; Var to_list : node_ptr) ;
Begin
  If from_list <> Nil
    Then
    Case from_list^.tag Of 
      variable : to_list := alloc_str(variable,
                            concat(from_list^.string_data,
                            level_str)) ;
      func,
      constant  : to_list := from_list ;
      cons_node :
                  Begin
                    list_copy(tail(from_list),to_list) ;
                    to_list := cons(copy_list(head(from_list),copy_level),
                               to_list) ;
                  End ;
    End ;
End ;
(* list_copy *)

Begin
  str(copy_level,level_str) ;
  level_str := concat('#',level_str) ;
  temp_list := Nil ;
  list_copy(list,temp_list) ;
  copy_list := temp_list ;
End ;
(* copy_list *)

Function unify(list1,list2,environ : node_ptr ; Var new_environ : node_ptr) :
                                                                              boolean ;

(* Unify two lists and return any new bindings at the front of the
        environment list. Returns true if the lists could be unified. This
        routine implements the unification table described in the article.
        Unification is straight forward, but the details of matching the
        lists get a little messy in this routine. There are better ways to
        do all of this, we just haven't gotten around to trying them. If
        you implement any other unification methods, we would be glad to
        hear about it.
        Unify checks to see if both lists are NIL, this is a successful
        unification. If one list is NIL, unification fails. Otherwise check
        what kind on node the head of list1 is and call the appropriate
        routine to perform the unification. Variables are unified by
        looking up the binding of the variable. If none is found, make
        a binding for the variable, otherwise try to unify the binding
        with list2. *)

Var 
  var_ptr : node_ptr ;

Procedure make_binding(l1,l2 : node_ptr) ;

(* Bind a variable to the environment. Anonymous variables are not bound.
         l1 points to the variable and l2 points to its binding. *)
Begin
  If copy(string_val(head(l1)),1,1) <> '_'
    Then new_environ := cons(cons(head(l1),l2),environ)
  Else new_environ := environ ;
  unify := true ;
End ;
(* make_binding *)

Procedure fail ;
      (* Unification failed. *)
Begin
  unify := false ;
  new_environ := environ ;
End ;
(* fail *)

Procedure unify_constant ;

(* List1 contains a constant. Try to unify it with list2. The 4 cases
         are:
          list2 contains
           constant - unify if constants match
           variable - look up binding, if no current binding bind the
                      constant to the variable, otherwise unify list1
                      with the binding.
           cons_node,
           func     - these can't be unified with a constant. A cons_node
                      indicates an expression. *)
Begin
  Case tag_value(head(list2)) Of 
    constant  : If string_val(head(list1)) = string_val(head(list2))
                  Then
                  Begin
                    unify := true ;
                    new_environ := environ ;
                  End
                Else fail ;
    variable  :
                Begin
                  var_ptr := look_up(string_val(head(list2)),environ) ;
                  If var_ptr = Nil
                    Then make_binding(list2,list1)
                  Else unify := unify(list1,var_ptr,environ,new_environ) ;
                End ;
    cons_node,
    func      : fail ;
  End ;
End ;
(* unify_constant *)

Procedure unify_func ;

(* List1 contains a functor. Try to unify it with list2. The 4 cases
         are:
          list2 contains
           constant  - can't be unified.
           variable  - look up binding, if no current binding bind the
                       functor to the variable, otherwise unify list1
                       with the binding.
           cons_node - fail
           func      - if the functors match, then true to unify the component
                       lists (tail of the list) term by term. *)

Procedure unify_tail ;
       (* This routine does the term by term unification of the component
          lists *)

Var 
  p,q : node_ptr ;
  unified : boolean ;
Begin
  p := tail(list1) ;
  q := tail(list2) ;
  unified := true ;
  new_environ := environ ;
  While (p <> Nil) And unified Do
    Begin
      unified := unified And unify(cons(head(p),Nil),cons(head(q),Nil),
                 new_environ,new_environ) ;
      p := tail(p) ;
      q := tail(q) ;
    End ;
  If Not unified
    Then fail ;
End ;
(* unify_tail *)

Begin
  Case tag_value(head(list2)) Of 
    constant  : fail ;
    variable  :
                Begin
                  var_ptr := look_up(string_val(head(list2)),environ) ;
                  If var_ptr = Nil
                    Then make_binding(list2,list1)
                  Else unify := unify(list1,var_ptr,environ,new_environ) ;
                End ;
    func      : If string_val(head(list1)) = string_val(head(list2))
                  Then
                  If list_length(tail(list1)) = list_length(tail(list2))
                    Then unify_tail
                Else fail
                Else fail ;
    cons_node : fail ;
  End ;
End ;
(* unify_func *)

Procedure unify_expr ;

(* List1 contains an expression. Try to unify it with list2. The 4 cases
         are:
          list2 contains
           constant  - can't be unified.
           variable  - look up binding, if no current binding bind the
                       functor to the variable, otherwise unify list1
                       with the binding.
           cons_node - If the heads can be unified, the unify the tails.
           func      - fail *)
Begin
  Case tag_value(head(list2)) Of 
    constant  : fail ;
    variable  :
                Begin
                  var_ptr := look_up(string_val(head(list2)),environ) ;
                  If var_ptr = Nil
                    Then make_binding(list2,list1)
                  Else unify := unify(list1,var_ptr,environ,new_environ) ;
                End ;
    func      : fail ;
    cons_node : If unify(head(list1),head(list2),environ,new_environ)
                  Then unify := unify(tail(list1),tail(list2),new_environ,
                                new_environ)
                Else fail ;
  End ;
End ;
(* unify_expr *)

Begin
  If (list1 = Nil) And (list2 = Nil)
    Then
    Begin
      unify := true ;
      new_environ := environ ;
    End
  Else If list1 = Nil
         Then fail
  Else If list2 = Nil
         Then fail
  Else
    Case tag_value(head(list1)) Of 
      constant  : unify_constant ;
      variable  :
                  Begin
                    var_ptr := look_up(string_val(head(list1)),environ) ;
                    If var_ptr = Nil
                      Then make_binding(list1,list2)
                    Else unify := unify(var_ptr,list2,environ,new_environ) ;
                  End ;
      func      : unify_func ;
      cons_node : unify_expr ;
    End ;
End ;
(* unify *)

Begin
  saved_list := cons(list,cons(env,saved_list)) ;
  If list = Nil
    Then check_continue
  Else
    Begin
      p := data_base ;
      While (p <> Nil) And (Not solved) Do
        Begin
          test_memory ;
          If unify(copy_list(head(head(p)),level),head(list),env,new_env)
            Then solve(append_list(copy_list(tail(head(p)),level),tail(list)),
            new_env,level + 1) ;
          p := tail(p) ;
        End ;
    End ;
  saved_list := tail(tail(saved_list)) ;
End ;
(* solve *)

Begin
  q_ptr := Nil ;
  tail_list(q_ptr) ;
  If token <> '.'
    Then error('''.'' expected.')
  Else If Not error_flag
         Then
         Begin
           solved := false ;
           saved_list := cons(data_base,Nil) ;
           solve(q_ptr,Nil,0) ;
           If Not solved
             Then writeln('No') ;
         End ;
End ;
(* query *)

Procedure read_new_file ;

(* Read source statements from a new file. When all done, close file
      and continue reading from the old file. Files may be nested, but you
      will run into trouble if you nest them deaper than 15 levels. This
      is Turbo's default for open files. *)

Var 
  new_file : text_file ;
  old_line,old_save : string132 ;
  f_name : string80 ;
Begin
  If token[1] = quote_char
    Then delete(token,1,1) ;
  If pos('.',token) = 0
    Then f_name := concat(token,'.PRO')
  Else f_name := token ;
  If open(new_file,f_name)
    Then
    Begin
      old_line := line ;
      old_save := saved_line ;
      line := '' ;
      compile(new_file) ;
      close(new_file) ;
      line := old_line ;
      saved_line := old_save ;
      scan(source,token) ;
      If token <> '.'
        Then error('''.'' expected.') ;
    End
  Else error(concat('Unable to open ',f_name)) ;
End ;
(* read_new_file *)

Procedure do_exit ;

(* Exit the program. This really should be a built-in function and handled
      in solve, but this does the trick. *)
Begin
  scan(source,token) ;
  If token <> '.'
    Then error('''.'' expected.')
  Else halt
End ;
(* do_exit *)

Begin
  scan(source,token) ;
  While token <> eof_mark Do
    Begin
      error_flag := false ;
      If token = '?-'
        Then
        Begin
          scan(source,token) ;
          query ;
        End
      Else If token = '@'
             Then
             Begin
               scan(source,token) ;
               read_new_file ;
             End
      Else 
      Begin
        toupper(token) ;
        If token = 'EXIT'
        Then do_exit
        Else rule ;
        End ;
      End ;
      scan(source,token) ;
    End ;
End ;
(* compile *)


Procedure initialize ;

Begin
void initialize() 
  // Write a heading line and initialize the global variables
{
  // clrscr();
  printf("\n");
  printf("Very Tiny Prolog - Version 1.0     [c] 1986 MicroExpert Systems\n");
  printf("\n");
  in_comment= false;

  // delim_set := [' ',')','(',',','[',']',eof_mark,tab,quote_char,':',
  // '@','.','?'] ;
  // text_chars := [' ' .. '~'] ;

  line= "";
  data_base= NULL;
  free= NULL;
  saved_list= NULL;
  total_free= 0.0;
  initial_heap= HeapPtr; // TODO(johnicholas.hines@gmail.com): hrr?
}
// initialize


*/

int main() {
  // initialize(); // TODO(johnicholas.hines@gmail.com): implement this
  // compile(stdin); // TODO(johnicholas.hines@gmail.com): implement this
  return 0;
}

