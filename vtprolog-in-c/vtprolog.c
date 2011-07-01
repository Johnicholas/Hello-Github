#include <assert.h>
#include <ctype.h> // for isalpha
#include <stdio.h>
#include <stdlib.h> // for malloc, free
#include <string.h> // for strlen
#include <unistd.h> // for isatty

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
// However, the translation to C has not been tested at all,
// and should NOT be trusted.
//
// Johnicholas Hines(johnicholas.hines@gmail.com)

typedef int boolean;
#define TRUE 1
#define FALSE 0

const boolean debug= 0;
const char back_space= 0x08; // in ascii
const char tab= '\t';
const char* eof_mark= "EOF"; // This was a character, but EOF isn't really an character in C/Unix.
const char esc= 0x1B; // in ascii
const char quote_char= '\'';
const char bell= '\b';

typedef int counter;
typedef char string80[80];
typedef char string132[132];
typedef char string255[255];

typedef FILE* text_file;

typedef char* char_set;

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
      string80 string_data;
    } func;
    struct {
      string80 string_data;
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
text_file source_file;
boolean error_flag, in_comment;
char_set delim_set, text_chars;

node_ptr data_base, initial_heap, vtprolog_free, saved_list, HeapPtr;
float total_free;

// The important globals are:
// source_file  - text file containing PROLOG statements.
// line         - line buffer for reading in the text file
// saved_list   - list of all items that absolutely must be saved if garbage
//                collection occurs. Usually has at least the data_base and
//                the currents query attached to it.
// initial_heap - the value of the heap pointer at the start of the program.
//                used by the garbage collector
// vtprolog_free         - the list of free nodes.
// total_free   - total number of free blocks on the free list.
// data_base    - a pointer to the start of the data base. It points to a
//                node pointing to the first sentence in the data base. Nodes
//                pointing to sentences are linked together to form the data
//                base.
// delim_set    - set of characters which delimit tokens.



// ----------------------------------------------------------------------
//      Utility Routines
// ----------------------------------------------------------------------

int vtprolog_open(text_file* f, const char* f_name)
// open a file - returns true if the file exists and was opened properly
//   f      - a text_file, passed by reference
//   f_name - external name of the file
{
  int retval;

  assert(f);
  *f= fopen(f_name, "r");
  return (*f != NULL);
}
// vtprolog_open

boolean is_console(text_file f)
// return true if f is open on the system console
{
  return isatty(fileno(f));
}

// Assumes the input is a null-terminated string, allocated with malloc, passed by reference.
// Returns (by modifying the input) a string that differs only by any initial spaces or tabs stripped off.
void strip_leading_blanks(char** s)
{
  int i;
  char* temp;

  i= 0;
  while ((*s)[i] == ' ' || (*s)[i] == tab) {
    i++;
  }
  temp= strdup((*s)+i); 
  free(*s);
  *s= temp;
}
// strip_leading_blanks

void vtprolog_toupper(char* s)
// converts s to upper case
{
  int i;

  for (i= 0; s[i] != 0; ++i) {
    s[i]= toupper(s[i]);
  }
}
// vtprolog_toupper

boolean is_number(char* s)
  // checks to see if s contains a legitimate numerical string.
  // It ignores leading blanks. 
{
  float num;

  return sscanf(s, " %f", &num) == 1;
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

const char* string_val(node_ptr list)
// returns the string pointed to by list. If list points to a number
//   node, it returns a string representing that number. 
{
  if (list == NULL)
    return "";
  else 
    switch (list->tag) {
    case CONSTANT: return list->u.constant.string_data; break;
    case VARIABLE: return list->u.variable.string_data; break;
    case FUNC: return list->u.variable.string_data; break;
    default: return ""; break;
    }
}
// string_val


node_type tag_value(node_ptr list)
// returns the value of the tag for a node.
{
  if (list == NULL)
    return FREE_NODE;
  else return list->tag;
}
// tag_value


void print_list(node_ptr list)
// recursively traverses the list and prints its elements. This is
//   not a pretty printer, so the lists may look a bit messy.
{
  node_ptr p;

  if (list != NULL)
    {
      switch (list->tag) {
      case CONSTANT: // fall through
      case FUNC: // fall through
      case VARIABLE: printf("%s ", string_val(list)); break;
      case CONS_NODE:
	printf("(");
	p= list;
	while (p != NULL)
	  {
	    print_list(head(p));
	    p= tail(p);
	  }
	printf(") ");
      }
    }
}
// print_list


// TODO(johnicholas.hines@gmail.com): p should be pass-by-reference
void get_memory(node_ptr* p, counter size)
// On exit p contains a pointer to a block of size bytes.
//   If possible this routine tries to get memory from the free list before
//   requesting it from the heap.
{
  counter blks;
  boolean allocated;

  // TODO(johnicholas.hines@gmail.com): list should be pass by reference
  void get_from_free(node_ptr list)
  // Try and get need memory from the free list. This routine uses a
  //  first-fit algorithm to get the space. It takes the first free block it
  //  finds with enough storage. If the free block has more storage than was
  //  requested, the block is shrunk by the requested amount.
  {
    if (list != NULL) {
      if (list->u.free_node.block_cnt >= (blks - 1)) {
	*p= list;
	if (list->u.free_node.block_cnt == blks - 1)
	  list= list->u.free_node.next_free;
	else list->u.free_node.block_cnt= list->u.free_node.block_cnt - blks;
	allocated= TRUE;
	total_free= total_free - (blks * 8.0);
      } else get_from_free(list->u.free_node.next_free);
    }
  }
  // get_from_free
  
  blks= ((size - 1) / 8) + 1; // TODO(johnicholas.hines@gmail.com): Duplication?
  allocated= FALSE;
  get_from_free(vtprolog_free);
  if (!allocated)
    getmem(p, blks * 8);
}
// get_memory

node_ptr alloc_str(node_type typ, string80 s)

// Allocate storage for a string and return a pointer to the new node.
//   This routine only allocates enough storage for the actual number of
//   characters in the string plus one for the length. Because of this,
//   concatenating anything to the end of a string stored in a symbol node
//   will lead to disaster. Copy the string to a new string do the
//   concatenation and then allocate a new node.

{
  node_ptr pt;

  get_memory(pt, sizeof(node_type) + sizeof(boolean) + sizeof(s) + 1); // TODO(johnicholas.hines@gmail.com): duplicate?
  pt->tag= typ;
  strncpy(pt->u.constant.string_data, s, 80); // TODO(johnicholas.hines@gmail.com): I think this requires that the string data is stored at the same spot in all the nodes that have string data
  // TODO(johnicholas.hines@gmail.com): This 80 magic number is no good, and strncpy isn't a great way to deal with strings anyway
  return pt;
}
// alloc_str


node_ptr cons(node_ptr new_node, node_ptr list)

// Construct a list. This routine allocates storage for a new cons node.
//   new_node points to the new head of the list. The tail pointer of the
//   new node points to list. This routine adds the new cons node to the
//   beginning of the list and returns a pointer to it. The list described
//   in the comments at the beginning of the program could be constructed
//   as cons(alloc_str('A'),cons(alloc_str('B'),cons(alloc_str('C'),NIL))). *)

{
  node_ptr p;

  get_memory(p, sizeof(struct node));
  p->tag= CONS_NODE;
  p->u.cons_node.head_ptr= new_node;
  p->u.cons_node.tail_ptr= list;
  return p;
}
// cons

node_ptr append_list(node_ptr list1, node_ptr list2)
// Append list2 to list1. This routine returns a pointer to the
//   combined list. Appending is done by consing each item on the first
//   list to the second list. This routine is one of the major sources of
//   garbage so if garbage collection becomes a problem, you may want to
//   rewrite it.
{
  if (list1 == NULL)
    return list2;
  else
    return cons(head(list1), append_list(tail(list1), list2));
}
// append_list

counter list_length(node_ptr list)
// returns the length of a list.
//   Note - both (A B C) and ( (A B) C D) have length 3.   *)
{
  if (list == NULL)
    return 0;
  else
    return 1 + list_length(list->u.cons_node.tail_ptr);
}
// list_length


void collect_garbage()
// This routine is specific to Turbo Pascal Ver 3.01
//   It depends upon the fact that Turbo allocates memory in 8 byte blocks
//   on the PC. If you recompile this program on another system be very
//   careful with this routine.
//   Garbage collection proceeds in three phases:
//    unmark  - free all memory between the initial_heap^ and the current
//              top of the heap.
//    mark    - mark everything on the saved_list as being in ues.
//    release - gather all unmarked blocks and put them on the free list.
//   The collector displays a '*' on the screen to let you know it is
//    operating.
{

  boolean lower(node_ptr p1, node_ptr p2)
  // returns true if p1 points to a lower memory address than p2
  {
    return p1 < p2;
  }
  // lower
  
  
  void mark(node_ptr list)
  // Mark the blocks on list as being in use. Since a node may be on several
  //    lists at one time, if it is already marked we don't continue processing
  //    the tail of the list.
  {
    if (list != NULL) {
      if (! list->in_use) {
	list->in_use= TRUE;
	if (list->tag == CONS_NODE) {
	  mark(head(list));
	  mark(tail(list));
	}
      }
    }
  }
  // mark
  
  
  // TODO(johnicholas.hines@gmail.com): This function is acting as if different kinds of nodes take different amounts of memory, which is not true in the current C implementation.
  void unmark_mem()
  // Go through memory from initial_heap^ to HeapPtr^ and mark each node
  //    as not in use. The tricky part here is updating the pointer p to point
  //    to the next cell.
  {
    node_ptr p;
    counter string_base, node_allocation;
    
    string_base= sizeof(node_type) + sizeof(boolean); // Johnicholas says: I think I've seen this somewhere - duplication?
    p= initial_heap;
    node_allocation= sizeof(struct node);
    
    while (lower(p, HeapPtr)) {
      p->in_use= FALSE;
      switch (p->tag) {
      case CONS_NODE: 
	p= p + node_allocation;
	break;
      case FREE_NODE: 
	p= p + (p->u.free_node.block_cnt + 1) * 8;
	break;
      case FUNC: // fall through
      case CONSTANT: // fall through
      case VARIABLE:
	p= p + string_base + sizeof(p->u.constant.string_data) + 1;
	break;
      }
    }
  }
  // unmark_mem
  
  void release_mem()
  // This procedure does the actual collection and compaction of nodes.
  //    This is the slow phase of garbage collection because of all the pointer
  //    manipulation.
  {
    node_ptr heap_top;
    counter string_base, node_allocation, string_allocation, block_allocation;
    
    void free_memory(node_ptr pt, counter size)
    // return size bytes pointed to by pt to the free list. If pt points to
    //     a block next to the head of the free list combine it with the top
    //     free node. total_free keeps track of the total number of free bytes.
    {
      counter blks;
      
      blks= ((size - 1) / 8) + 1; 
      pt->tag= FREE_NODE;
      if (pt + 8 * blks == vtprolog_free) {
	pt->u.free_node.next_free= vtprolog_free->u.free_node.next_free;
	pt->u.free_node.block_cnt= vtprolog_free->u.free_node.block_cnt + blks;
	vtprolog_free= pt;
      }
      else if (vtprolog_free + 8 * (vtprolog_free->u.free_node.block_cnt + 1) == pt)
	vtprolog_free->u.free_node.block_cnt= vtprolog_free->u.free_node.block_cnt + blks;
      else {
	pt->u.free_node.next_free= vtprolog_free;
	pt->u.free_node.block_cnt= blks - 1;
	vtprolog_free= pt;
      }
      total_free= total_free + (blks * 8.0);
    }
    // free_memory
    
    // TODO(johnicholas.hines@gmail.com): Again, this is sweeping through memory, assuming these nodes have variable sizes,
    // which they don't in the C version at the moment; there's duplication too.
    void do_release()
    // This routine sweeps through memory and checks for nodes with in_use = false.
    {
      node_ptr p;
      
      p= initial_heap;
      while (lower(p, heap_top)) {
	switch (p->tag) {
	case CONS_NODE: 
	  if (!p->in_use) {
	    free_memory(p, sizeof(struct node));
	  }
	  p= p + node_allocation;
	  break;
	case FREE_NODE:
	  block_allocation= (p->u.free_node.block_cnt + 1) * 8;
	  free_memory(p, block_allocation);
	  p= p + block_allocation;
	  break;
	case FUNC: // fall through
	case CONSTANT: // fall through
	case VARIABLE: 
	  string_allocation= string_base + sizeof(p->u.constant.string_data) + 1;
	  if (! p->in_use)
	    free_memory(p, string_base + sizeof(p->u.constant.string_data) + 1);
	  p= p + string_allocation;
	  break;
	}
      }
    }
    // do_release
    
    vtprolog_free= NULL;
    total_free= 0.0;
    heap_top= HeapPtr;
    string_base= sizeof(node_type) + sizeof(boolean); // TODO(johnicholas.hines@gmail.com): What is this trying to do?
    node_allocation= sizeof(struct node);
    do_release();
  }
  // release_mem
  
  printf("*");
  unmark_mem();
  mark(saved_list);
  release_mem();
  // printf("%c", back_space); // TODO(johnicholas.hines@gmail.com): What is the goal here?
  
  // ClrEol ; // TODO(johnicholas.hines@gmail.com): Maybe an ansi escape sequence would be appropriate here?

}
// collect_garbage

void test_memory()
// This routine activates the garbage collector, if the the total available
//   memory (free_list + heap) is less than a specified amount. Lowering the
//   minimum causes garbage collection to be called less often, but if you
//   make it too small you may not have enough room left for recursion or any
//   temporary lists you need. Using 10000 is probably being overly
//   cautious.
{
  // Johnicholas says: This function is probably going to have to be completely rewritten.
  // memavail was a function that returned the amount of available free memory
  if ((memavail() * 16.0) + total_free < 10000) 
    collect_garbage();
}
// test_memory

void wait()
// Just like it says. It waits for the user to press a key before
//   continuing.
{
  char ch;

  printf("\n");
  printf("\n");
  printf("Press any key to continue. ");
  scanf("%c", &ch);
  printf("\n");

  // ClrEol ; // TODO(johnicholas.hines@gmail.com): Maybe an ansi escape sequence would be appropriate here? or curses? or nothing?
}
// wait


// ------------------------------------------------------------------------
//      End of utility routines
// ------------------------------------------------------------------------


// TODO(johnicholas.hines@gmail.com): s should be pass-by-reference
void read_kbd(string80 s)
// Read a line from the keyboard
{
  printf("-> ");
  getline(NULL, 0, stdin); // TODO(johnicholas.hines@gmail.com): The line should go into s, not the bit bucket.
}
// read_kbd

// TODO(johnicholas.hines@gmail.com): f should be pass-by-reference
void read_from_file(text_file f)
// Read a line from file f and store it in the global variable line.
//   It ignores blank lines and when the end of file is reached an
//   eof_mark is returned.
{

  // TODO(johnicholas.hines@gmail.com): Almost certainly, this, and eof_mark, will need to be completely rewritten.
  void read_a_line()
  {
    // $I- // TODO(johnicholas.hines@gmail.com): What does this mean?
    // readln(f, line);
    // $I+ // TODO(johnicholas.hines@gmail.com): What does this mean?
    // if (ioresult != 0)
    //   line= eof_mark;
    // else if eof(f);
    // line= concat(line, eof_mark);
  }
  // read_a_line
  
  line[0]= '\0';
  if (is_console(f))
    read_kbd(line);
  else read_a_line();
  if (in_comment) {
    if (pos("*)", line) > 0) // TODO(johnicholas.hines@gmail.com): if there is a end-of-comment sequence on this line?
      {
	delete(line, 1, pos("*)", line) + 1);
	in_comment= FALSE;
      }
  }
  else read_from_file(f);
  strncpy(saved_line, line, sizeof(saved_line));
}
// read_from_file

// TODO(johnicholas.hines@gmail.com): both t_line and token should be passed by reference.
void get_token(string132 t_line, string80 token)
// Extract a token from t_line. Comments are ignored. A token is
//   a string surrounded by delimiters or an end of line. Tokens may
//   contain embedded spaces if they are surrounded by quote marks.
{

  void get_word() {
    boolean done;
    int cn;
    int len;
    
    cn= 1;
    len= strlen(t_line);
    done= FALSE;
    while (!done) {
      if (cn > len)
	done= TRUE;
      else if (in(t_line[cn], delim_set))
	done= TRUE;
      else cn= cn + 1;
    }
    token= copy(t_line, 1, cn-1);
    delete(t_line, 1, cn-1);
  }
  // get_word
  
  void comment()
  {
    if (pos("*)", t_line) > 0) // Johnicholas says: does this mean "if there is an end-comment sequence on this line?"
      {
	delete(t_line, 1, pos("*)", t_line) + 1);
	get_token(line, token);
      }
    else
      {
	t_line= "";
	token= "";
	in_comment= TRUE;
      }
  }
  // comment
  
  void get_quote()
  {
    delete(t_line, 1, 1);
    if (pos(quote_char, t_line) > 0) // Johnicholas says: does this mean "if there is a quote character on the line?"
      {
	token= concat(quote_char, copy(t_line, 1, pos(quote_char, t_line) - 1));
	delete(t_line, 1, pos(quote_char, t_line));
      }
    else
      {
	token= t_line;
	t_line= "";
      }
  }
  // get_quote
  
  strip_leading_blanks(&t_line);
  if (strlen(t_line) > 0)
    {
      if (strcmp(copy(t_line, 1, 2), "(*") == 0)  // TODO(johnicholas.hines@gmail.com): What is copy? Should I be using it?
	comment();
      else if (strcmp(copy(t_line, 1, 2), ":-") == 0 || strcmp(copy(t_line, 1, 2), "?-") == 0)
	{
	  token= copy(t_line, 1, 2);
	  delete(t_line, 1, 2);
	}
      else if (t_line[0] == quote_char)
	get_quote();
      else if (in(t_line[0], delim_set))
	{
	  token= t_line[0];
	  delete(t_line, 1, 1);
	}
      else get_word();
    }
  else token= "";
}
// get_token

// TODO(johnicholas.hines@gmail.com): both f and token should be passed by reference
void scan(text_file f, string80 token)
// Scan repeatedly calls get_token to retreive tokens. When the
//   end of a line has been reached, read_from_file is called to
//   get a new line.
{
  if (strlen(line) > 0)
    {
      get_token(line, token);
      if (strcmp(token, "") == 0)
	scan(f, token);
    }
  else
    {
      read_from_file(f);
      scan(f, token);
    }
}
// scan

node_ptr look_up(string80 var_str, node_ptr environ)
// Search the environment list pointed to by environ for the variable,
//      var_str. If found return a pointer to var_str's binding, otherwise
//      return NIL
{
  boolean found;
  node_ptr p;
  
  p= environ;
  found= FALSE;
  while (p != NULL && ! found)
    {
      if (strcmp(var_str, string_val(head(head(p)))) == 0) {
	{
	  found= TRUE;
	  return tail(head(p));
	}
      } else p= tail(p);
    }
  if (!found) // TODO(johnicholas.hines@gmail.com): Is this necessary?
    return NULL; 
}
// look_up

void print_components(node_ptr p, node_ptr env)
// Print the components of a functor. These may be variables or
// other functors, so call the approriate routines to print them.
{
  if (p != NULL)
    {
      switch (tag_value(head(p))) {
      case CONSTANT: printf("%s ", string_val(head(p))); break;
      case VARIABLE: print_variable(string_val(head(p)), env); break;
      case CONS_NODE: print_functor(head(p), env); break;
      }
      if (tail(p) != NULL) 
	{
	  printf(",");
	  print_components(tail(p), env);
	}
    }
}
// print_components
	    
void print_functor(node_ptr l, node_ptr env)
// The variable was bound to a functor. Print the functor and its
// components.
{	    
  if (l != NULL) {
    printf("%s", string_val(head(l)));
    if (tail(l) != NULL) {
      printf("(");
      print_components(tail(l), env);
      printf(")");
    }
  }
}
// print_functor
	  
void print_variable(string80 var_str, node_ptr env)
// The varaible in question was bound to another varaible, so look
//        up that variable's binding and print it. If a match can't be found
//        print '_' to tell the user that the variable is anonymous.
{
  node_ptr var_ptr;
	    
  var_ptr= look_up(var_str, env);
  if (var_ptr != NULL) {
    switch (tag_value(head(var_ptr))) {
    case CONSTANT: printf("%s ", string_val(head(var_ptr))); break;
    case VARIABLE: print_variable(string_val(head(var_ptr)), env); break;
    case CONS_NODE: print_functor(head(var_ptr), env); break;
    }
  }
  else printf("_ ");
}
// print_variable
	  
// TODO(johnicholas.hines@gmail.com): printed should be passed by reference.
void print_bindings(node_ptr list, node_ptr env, boolean printed)
// Print the bindings for level 0 variables only, intermediate variables
//     aren't of interest. The routine recursivley searches for the
//     end of the environments list and then prints the binding. This
//     is so that variables bound first are printed first. *)
{	  
  if (list != NULL) {
    print_bindings(tail(list), env, printed);
    if (pos('#', string_val(head(head(list)))) == 0) {
      printed= TRUE;
      printf("\n");
      printf("%s = ", string_val(head(head(list))));
      switch (tag_value(head(tail(head(list))))) {
      case CONSTANT: printf("%s ", string_val(head(tail(head(list))))); break;
      case VARIABLE: print_variable(string_val(head(tail(head(list)))), env); break;
      case CONS_NODE: print_functor(head(tail(head(list))), env); break;
      }
    }
  }
}
// print_bindings
	
// TODO(johnicholas.hines@gmail.com): source should be passed by reference
void compile(text_file source)
// The recursive descent compiler. It reads tokens until the token
//   'EXIT' is found. If the token is '?-', a query is performed, a '@' token
//   is the command to read a new file and source statements are read form that
//   file, otherwise the token is assumed to be part of a sentence and the rest
//   of the sentence is parsed.
{
  void error(string80 error_msg)
  // Signal an error. Prints saved_line to show where the error is located.
  //    saved_line contains the current line being parsed. it is required,
  //    because get_token chews up line as it reads tokens.
  {
    void runout()
    {
      while (strcmp(token, ".") != 0 && strcmp(token, eof_mark) != 0) 
	scan(source, token);
    }
    // runout
    
    error_flag= TRUE;
    printf("\n");
    printf("%s", error_msg);
    printf("\n");
    printf("%s\n", saved_line);
    // writeln('' : length(saved_line) - length(line) - 1,'^') ; ; // TODO(johnicholas.hines@gmail.com): What was this doing?
    runout();
    wait();
  }
  // error

  // TODO(johnicholas.hines@gmail.com): l_ptr ought to be passed by reference
  void goal(node_ptr l_ptr)
  // Read a goal. The new goal is appended to l_ptr. Each goal is appended
  //    to l_ptr as a list. Thus, the sentence 'likes(john,X) :- likes(X,wine) .'
  //    becomes the list ( (likes john X) (likes X wine) ) *)
  {
    string80 goal_token;
    
    // TODO(johnicholas.hines@gmail.com): f_ptr ought to be passed by reference
    void functor(node_ptr f_ptr, string80 func_token)
    // The current goal is a functor. This routine allocates a node
    //   to store the functor and then processes the components of the
    //   functor. On exit, f_ptr points to the list containing the functor
    //   and its components. func_token contains the functor name. *)
    {
      node_ptr c_ptr;
      
      // TODO(johnicholas.hines@gmail.com): cm_ptr ought to be passed by reference
      void components(node_ptr cm_ptr)
      // Process the components of the functor. The components are terms
      //      seperated by commas. On exit, cm_ptr points to the list of
      //      components.
      {
	
	// TODO(johnicholas.hines@gmail.com): t_ptr ought to be passed by reference
	void term(node_ptr t_ptr)
	// Process a single term. The new term is appended to t_ptr.
	{
	  string80 t_token;
	  
	  // TODO(johnicholas.hines@gmail.com): q_ptr ought to be passed by reference
	  void quoted_str(node_ptr q_ptr)
	  // Process a quote
	  {
	    q_ptr= append_list(q_ptr, cons(alloc_str(CONSTANT,
						     copy(token, 2, sizeof(token) - 1)),
					   NULL));
	    scan(source, token);
	  }
	  // quoted_str
	  
	  void varbl(node_ptr v_ptr)
	  // The current token is a varaible, allocate a node and return
	  //        a pointer to it.
	  {
	    v_ptr= append_list(v_ptr, cons(alloc_str(VARIABLE, token), NULL));
	    scan(source, token);
	  }
	  // varbl
	  
	  // TODO(johnicholas.hines@gmail.com): n_ptr ought to be passed by reference
	  void number(node_ptr n_ptr)
	  // Numbers are treated as string constants. This isn't particularly
	  //        efficent, but it is easy.
	  {
	    n_ptr= append_list(n_ptr, cons(alloc_str(CONSTANT, token), NULL));
	    scan(source, token);
	  }
	  // handle_number
	  
	  if (isupper(token[0]) || token[0] == '_')
	    varbl(t_ptr);
	  else if (token[0] == quote_char)
	    quoted_str(t_ptr);
	  else if (is_number(token))
	    number(t_ptr);
	  else if (isalpha(token[0]))
	    {
	      strncpy(t_token, token, sizeof(t_token));
	      scan(source, token);
	      if (strcmp(token, "(") == 0)
		functor(t_ptr, t_token);
	      else 
		t_ptr= append_list(t_ptr,
				   cons(alloc_str(CONSTANT, t_token), NULL));
	    }
	  else error("Illegal Symbol.");
	}
	// term
	
	term(cm_ptr);
	if (strcmp(token, ",") == 0)
	  {
	    scan(source, token);
	    components(cm_ptr);
	  }
      }
      // components
      
      c_ptr= cons(alloc_str(FUNC, func_token), NULL);
      scan(source, token);
      components(c_ptr);
      if (strcmp(token, ")") == 0)
	{
	  f_ptr= append_list(f_ptr, cons(c_ptr, NULL));
	  scan(source, token);
	}
      else error("Missing ')'.");
    }
    // functor
    
    if (in(token[0], "[a-z]'")) // TODO(johnicholas.hines@gmail.com): Was token[1] In ['a' .. 'z',quote_char]
      {
	if (token[0] == quote_char)
	  {
	    l_ptr= append_list(l_ptr,
			       cons(cons(alloc_str(CONSTANT,
						   copy(token, 2, sizeof(token) - 1)), NULL), NULL));
	    scan(source, token);
	  }
	else
	  {
	    strncpy(goal_token, token, sizeof(goal_token));
	    scan(source, token);
	    if (strcmp(token, "(") == 0) {
	      functor(l_ptr, goal_token);
	    } else 
	      l_ptr= append_list(l_ptr,
				 cons(cons(alloc_str(CONSTANT, goal_token),
					   NULL), NULL));
	  }
      }
    else error("A goal must begin with 'a .. z' or be a quoted string.");
  }
  // goal
  
  // TODO(johnicholas.hines@gmail.com): t_ptr should be passed by reference
  void tail_list(node_ptr t_ptr)
  // Process the tail of a rule. Since the a query is syntactically identical
  //    to the tail of a rule, this routine is used to compile queries.
  //    On exit, t_ptr points to the list containing the tail.
  {
    goal(t_ptr);
    if (strcmp(token, ",") == 0)
      {
	scan(source, token);
	tail_list(t_ptr);
      }
  }
  // tail
  
  void rule()
  // Procees a rule, actually any sentence. If no error occurs the
  //    new sentence is appended to the data base.
  {
    node_ptr r_ptr;
    
    // TODO(johnicholas.hines@gmail.com): h_ptr should be passed by reference
    void head_list(node_ptr h_ptr)
    {
      goal(h_ptr);
    }
    // head
    
    saved_list= cons(data_base, NULL);
    test_memory();
    r_ptr= NULL;
    head_list(r_ptr);
    if (strcmp(token, ":-") == 0)
      {
	scan(source, token);
	tail_list(r_ptr);
      }
    if (strcmp(token, ".") != 0)
      error("'.' expected.");
    if (!error_flag)
      data_base= append_list(data_base, cons(r_ptr, NULL));
  }
  // rule
  
  void query()
  // Process a query. Compile the query, and then call solve to search the
  //    data base. q_ptr points to the compiled query and solved is a boolean
  //    indicating whether the query was successfully solved.
  {
    node_ptr q_ptr;
    boolean solved;
    
    void solve(node_ptr list, node_ptr env, counter level)
    //   This is where all the hard work is done. This routine follows the
    //     steps outlined in the article. list is the query to be soved, env is
    //     the current environment and level is the recursion level. level can
    //     only get to 32767, but you'll run out of stack space long before you
    //     get that far.
    //     solve saves list and env on the saved list so that they won't be
    //     destroyed by garbage collection. The data base is always on the
    //     saved list. At the end of solve, list and env are removed from
    //     saved_list.
    {
      node_ptr new_env;
      node_ptr p;
      
      void check_continue()
      // Print the bindings and see if the user is satisfied. If nothing
      //      is printed from the environment, then print 'Yes' to indicate
      //      that the query was successfully satisfied. *)
      {
	boolean printed;
	char ch;
	
	printed= FALSE;
	print_bindings(env, env, printed);
	if (!printed) {
	  printf("\n");
	  printf("Yes ");
	}
	do {
	  ch= getchar(); // TODO(johnicholas.hines@gmail.com): getchar actually returns an int, and it might be EOF, which is not a char.
	} while (!in(ch, "\n;")); // TODO(johnicholas.hines@gmail.com): Write a new function, 'in'? or is there a C version?
	solved= (ch == '\n');
	printf("\n");
      }
      // check_continue
      
      node_ptr copy_list(node_ptr list, counter copy_level)
      // Copy a list and append the copy_level (recursion level) to all
      // variables.
      {
	node_ptr temp_list, p;
	const char level_str[6];
	
	// TODO(johnicholas.hines@gmail.com): to_list should be passed by reference
	void list_copy(node_ptr from_list, node_ptr to_list)
	{
	  if (from_list != NULL) 
	    {
	      switch (from_list->tag) {
	      case VARIABLE:
		to_list= alloc_str(VARIABLE, concat(from_list->u.constant.string_data, level_str));
		break;
	      case FUNC: // fall through
	      case CONSTANT:
		to_list= from_list;
		break;
	      case CONS_NODE:
		list_copy(tail(from_list), to_list);
		to_list= cons(copy_list(head(from_list), copy_level), to_list);
		break;
	      }
	    }
	}
	// list_copy
	
	// str(copy_level, level_str); // TODO(johnicholas.hines@gmail.com): What did this mean?
	strncpy(level_str, concat("#", level_str), sizeof(level_str));
	temp_list= NULL;
	list_copy(list, temp_list);
	return temp_list;
      }
      // copy_list
      
      // TODO(johnicholas.hines@gmail.com): new_environ should be passed by reference, I think.
      boolean unify(node_ptr list1, node_ptr list2, node_ptr environ, node_ptr new_environ)
      // Unify two lists and return any new bindings at the front of the
      //      environment list. Returns true if the lists could be unified. This
      //      routine implements the unification table described in the article.
      //      Unification is straight forward, but the details of matching the
      //      lists get a little messy in this routine. There are better ways to
      //      do all of this, we just haven't gotten around to trying them. If
      //      you implement any other unification methods, we would be glad to
      //      hear about it.
      //      Unify checks to see if both lists are NIL, this is a successful
      //      unification. If one list is NIL, unification fails. Otherwise check
      //      what kind on node the head of list1 is and call the appropriate
      //      routine to perform the unification. Variables are unified by
      //      looking up the binding of the variable. If none is found, make
      //      a binding for the variable, otherwise try to unify the binding
      //      with list2.
      {
	node_ptr var_ptr;
	boolean unify_return_value;
	
	void make_binding(node_ptr l1, node_ptr l2)
	// Bind a variable to the environment. Anonymous variables are not bound.
	//       l1 points to the variable and l2 points to its binding.
	{
	  if (strcmp(copy(string_val(head(l1)), 1, 1), "_") != 0) {
	    new_environ= cons(cons(head(l1), l2), environ);
	  } else
	    new_environ= environ;
	  return TRUE;
	}
	// make_binding
	
	void fail()
	// Unification failed.
	{
	  unify_return_value= FALSE;
	  new_environ= environ;
	}
	// fail
	
	void unify_constant()
	// List1 contains a constant. Try to unify it with list2. The 4 cases
	//       are:
	//        list2 contains
	//         constant - unify if constants match
	//         variable - look up binding, if no current binding bind the
	//                    constant to the variable, otherwise unify list1
	//                    with the binding.
	//         cons_node,
	//         func     - these can't be unified with a constant. A cons_node
	//                    indicates an expression.
	{
	  switch (tag_value(head(list2))) {
	  case CONSTANT:
	    if (strcmp(string_val(head(list1)), string_val(head(list2))) == 0) {
	      unify_return_value= TRUE;
	      new_environ= environ;
	    } else fail();
	    break;
	  case VARIABLE:
	    var_ptr= look_up(string_val(head(list2)), environ);
	    if (var_ptr == NULL) {
	      make_binding(list2, list1);
	    } else {
	      unify_return_value= unify(list1, var_ptr, environ, new_environ);
	    }
	    break;
	  case CONS_NODE: // fall through
	  case FUNC: fail(); break;
	  }
	}
	// unify_constant
	
	void unify_func()
	// List1 contains a functor. Try to unify it with list2. The 4 cases
	//       are:
	//        list2 contains
	//         constant  - can't be unified.
	//         variable  - look up binding, if no current binding bind the
	//                     functor to the variable, otherwise unify list1
	//                     with the binding.
	//         cons_node - fail
	//         func      - if the functors match, then true to unify the component
	//                     lists (tail of the list) term by term.
	{
	  
	  void unify_tail()
	  // This routine does the term by term unification of the component
	  //   lists
	  {
	    node_ptr p;
	    node_ptr q;
	    boolean unified;
	    
	    p= tail(list1);
	    q= tail(list2);
	    unified= TRUE;
	    new_environ= environ;
	    while (p != NULL && unified)
	      {
		unified= unified && unify(cons(head(p), NULL), cons(head(q), NULL), new_environ, new_environ);
		p= tail(p);
		q= tail(q);
	      }
	    if (!unified)
	      fail();
	  }
	  // unify_tail
	  
	  switch (tag_value(head(list2))) {
	  case CONSTANT: fail(); break;
	  case VARIABLE: 
	    var_ptr= look_up(string_val(head(list2)), environ);
	    if (var_ptr == NULL) {
	      make_binding(list2, list1);
	    } else
	      return unify(list1, var_ptr, environ, new_environ);
	    break;
	  case FUNC:
	    if (strcmp(string_val(head(list1)), string_val(head(list2))) == 0) {
	      if (list_length(tail(list1)) == list_length(tail(list2))) {
		unify_tail();
	      } else fail();
	    } else fail();
	    break;
	  case CONS_NODE: fail(); break;
	  }
	}
	// unify_func
	
	void unify_expr()
	// List1 contains an expression. Try to unify it with list2. The 4 cases
	//       are:
	//        list2 contains
	//         constant  - can't be unified.
	//         variable  - look up binding, if no current binding bind the
	//                     functor to the variable, otherwise unify list1
	//                     with the binding.
	//         cons_node - If the heads can be unified, the unify the tails.
	//         func      - fail
	{
	  switch (tag_value(head(list2))) {
	  case CONSTANT: fail(); break;
	  case VARIABLE: 
	    var_ptr= look_up(string_val(head(list2)), environ);
	    if (var_ptr == NULL) {
	      make_binding(list2, list1);
	    } else
	      return unify(list1, var_ptr, environ, new_environ);
	    break;
	  case FUNC: fail(); break;
	  case CONS_NODE: 
	    if (unify(head(list1), head(list2), environ, new_environ)) {
	      return unify(tail(list1), tail(list2), new_environ, new_environ);
	    } else fail();
	    break;
	  }
	}
	// unify_expr
	
	if (list1 == NULL && list2 == NULL) {
	  unify_return_value= TRUE;
	  new_environ= environ;
	} else if (list1 == NULL) {
	  fail();
	} else if (list2 == NULL) {
	  fail();
	} else {
	  switch (tag_value(head(list1))) {
	  case CONSTANT: unify_constant(); break;
	  case VARIABLE:
	    var_ptr= look_up(string_val(head(list1)), environ);
	    if (var_ptr == NULL) {
	      make_binding(list1, list2);
	    } else
	      return unify(var_ptr, list2, environ, new_environ);
	    break;
	  case FUNC: unify_func(); break;
	  case CONS_NODE: unify_expr(); break;
	  }
	}
	return unify_return_value;
      }
      // unify
      
      saved_list= cons(list, cons(env, saved_list));
      if (list == NULL) {
	check_continue();
      } else {
	p= data_base;
	while (p != NULL && !solved) {
	  test_memory();
	  if (unify(copy_list(head(head(p)), level), head(list), env, new_env)) {
	    solve(append_list(copy_list(tail(head(p)), level), tail(list)), new_env, level + 1);
	    p= tail(p);
	  }
	}
	saved_list= tail(tail(saved_list));
      }
    }
    // solve
    
    q_ptr= NULL;
    tail_list(q_ptr);
    if (strcmp(token, ".") != 0) {
      error("'.' expected.");
    } else if (!error_flag) {
      solved= FALSE;
      saved_list= cons(data_base, NULL);
      solve(q_ptr, NULL, 0);
      if (!solved) {
	printf("No\n");
      }
    }
  }
  // query
  
  void read_new_file()
  // Read source statements from a new file. When all done, close file
  //    and continue reading from the old file. Files may be nested, but you
  //    will run into trouble if you nest them deaper than 15 levels. This
  //    is Turbo's default for open files. 
  {
    text_file new_file;
    string132 old_line, old_save;
    string80 f_name;
    
    if (token[0] == quote_char) {
      delete(token, 1, 1); // Does this mean "delete one character from the front of token"?
    }
    if (pos('.', token) == 0) { // Does this mean "if there are no occurrences of period character within token"?
      strncpy(f_name, concat(token, ".pl"), sizeof(f_name)); // was .PRO, but I think .pl is more common for prolog source
    } else {
      strncpy(f_name, token, sizeof(f_name));
    }
    if (vtprolog_open(new_file, f_name)) {
      strncpy(old_line, line, sizeof(old_line));
      strncpy(old_save, saved_line, sizeof(old_save));
      line[0]= '\0';
      compile(new_file);
      close(new_file);
      strncpy(line, old_line, sizeof(line));
      strncpy(saved_line, old_save, sizeof(saved_line));
      scan(source, token);
      if (strcmp(token, ".") != 0) {
	error("'.' expected.");
      }
    } else {
      error(concat("Unable to open ", f_name));
    }
  }
  // read_new_file
    
  void do_exit() 
  // Exit the program. This really should be a built-in function and handled
  //   in solve, but this does the trick.
  {
    scan(source,token);
    if (strcmp(token, ".") != 0) {
      error("'.' expected.");
    } else {
      exit(0);
    }
  }
  // do_exit
  
  scan(source, token);
  while (token != eof_mark) {
    error_flag= FALSE;
    if (strcmp(token, "?-") == 0) {
      scan(source, token);
      query();
    } else if (strcmp(token, "@") == 0) {
      scan(source, token);
      read_new_file();
    } else {
      toupper(token);
      if (strcmp(token, "EXIT") == 0) {
	do_exit(); 
      } else {
	rule();
      }
    }
    scan(source,token);
  }
}
// compile

void initialize() 
// Write a heading line and initialize the global variables
{
  // clrscr();
  printf("\n");
  printf("Very Tiny Prolog - Version 1.0     [c] 1986 MicroExpert Systems\n");
  printf("\n");
  in_comment= FALSE;
  
  // delim_set := [' ',')','(',',','[',']',eof_mark,tab,quote_char,':',
  // '@','.','?'] ;
  // text_chars := [' ' .. '~'] ;
  
  line[0]= '\0';
  data_base= NULL;
  vtprolog_free= NULL;
  saved_list= NULL;
  total_free= 0.0;
  initial_heap= HeapPtr;
}
// initialize

#ifndef BUILD_AS_LIBRARY
int main() {
  initialize();
  compile(stdin);
  return 0;
}
#endif // BUILD_AS_LIBRARY
