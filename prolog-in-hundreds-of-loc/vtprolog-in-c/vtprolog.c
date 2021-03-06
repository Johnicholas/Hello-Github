/*
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
*/

/* This is necessary for fileno, which is conditionally defined in stdio.h */
#define _POSIX_C_SOURCE 1
/* This is necessary for strdup, which is conditionally defined in string.h */
#define _GNU_SOURCE

#include <assert.h>
/* for isalpha */
#include <ctype.h>
#include <stdio.h>
/* for malloc, free */
#include <stdlib.h>
/* for strlen */
#include <string.h>
/* for isatty */
#include <unistd.h>

typedef int boolean;
#define TRUE 1
#define FALSE 0

const boolean debug= 0;
/* const char back_space= 0x08; in ascii */
const char tab= '\t';
/* This was a character, but EOF isn't really an character in C/Unix. */
const char* eof_mark= "EOF";
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
/*
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
*/

string132 line, saved_line;
string80 token;
text_file source_file;
boolean error_flag, in_comment;
char_set delim_set, text_chars;

node_ptr data_base, initial_heap, vtprolog_free, saved_list, HeapPtr;
float total_free;

/*
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
*/


/*
// ----------------------------------------------------------------------
//      Utility Routines
// ----------------------------------------------------------------------
*/

node_ptr head(node_ptr list)
/*
// returns a pointer to the first item in the list
// if the list is empty, returns NULL.
*/
{
    return list ? list->u.cons_node.head_ptr : NULL;
}
/* head */

node_ptr tail(node_ptr list)
/*
// returns a pointer to a list starting at the second item in the list.
// Note - tail( (a b c) ) points to the list (b c), but
//        tail( ((a b) c d) ) points to the list (c d) .
*/
{
    if (list == NULL) return NULL;
    switch (list->tag) {
    case CONS_NODE:
        return list->u.cons_node.tail_ptr;
    case FREE_NODE:
        return list->u.free_node.next_free;
    default:
        return NULL;
    }
}
/* tail */

char* string_val(node_ptr list)
/*
// returns the string pointed to by list. If list points to a number
//   node, it returns a string representing that number.
*/
{
    if (list == NULL) return "";
    switch (list->tag) {
    case CONSTANT:
        return list->u.constant.string_data;
        break;
    case VARIABLE:
        return list->u.variable.string_data;
        break;
    case FUNC:
        return list->u.func.string_data;
        break;
    default:
        return "";
        break;
    }
}
/* string_val */

node_type tag_value(node_ptr list)
/* returns the value of the tag for a node. */
{
    return list ? list->tag : FREE_NODE;
}
/* tag_value */

void print_list(node_ptr list)
/*
// recursively traverses the list and prints its elements. This is
//   not a pretty printer, so the lists may look a bit messy.
*/
{
    node_ptr p;

    if (list == NULL) return;
    switch (list->tag) {
    case CONSTANT: /* fall through */
    case FUNC: /* fall through */
    case VARIABLE:
        printf("%s ", string_val(list));
        break;
    case CONS_NODE:
        printf("(");
        for (p= list; p; p= tail(p)) print_list(head(p));
        printf(") ");
        break;
    case FREE_NODE:
        assert(0);
        break;
    }
}
/* print_list */


/* TODO(johnicholas.hines@gmail.com): list, blks, allocated, p should be pass by reference */
void get_from_free(node_ptr list, counter blks, boolean allocated, node_ptr* p)
/*
// Try and get need memory from the free list. This routine uses a
//  first-fit algorithm to get the space. It takes the first free block it
//  finds with enough storage. If the free block has more storage than was
//  requested, the block is shrunk by the requested amount.
*/
{
    if (list == NULL) return;

    if (list->u.free_node.block_cnt >= (blks - 1)) {
        *p= list;
        if (list->u.free_node.block_cnt == blks - 1)
            list= list->u.free_node.next_free;
        else list->u.free_node.block_cnt= list->u.free_node.block_cnt - blks;
        allocated= TRUE;
        total_free= total_free - (blks * 8.0);
    } else get_from_free(list->u.free_node.next_free, blks, allocated, p);
}
/* get_from_free */

void get_memory(node_ptr* p, counter size)
/*
// On exit p contains a pointer to a block of size bytes.
//   If possible this routine tries to get memory from the free list before
//   requesting it from the heap.
*/
{
    counter blks;
    boolean allocated;

    blks= ((size - 1) / 8) + 1; /* TODO(johnicholas.hines@gmail.com): Duplication? */
    allocated= FALSE;
    get_from_free(vtprolog_free, blks, allocated, p);
    if (!allocated)
        *p= malloc(blks * 8);
}
/* get_memory */

node_ptr alloc_str(node_type typ, string80 s)
/*
// Allocate storage for a string and return a pointer to the new node.
//   This routine only allocates enough storage for the actual number of
//   characters in the string plus one for the length. Because of this,
//   concatenating anything to the end of a string stored in a symbol node
//   will lead to disaster. Copy the string to a new string do the
//   concatenation and then allocate a new node.
*/
{
    node_ptr pt;

    get_memory(&pt, sizeof(node_type) + sizeof(boolean) + sizeof(s) + 1); /* TODO(johnicholas.hines@gmail.com): duplicate? */
    pt->tag= typ;
    strncpy(pt->u.constant.string_data, s, 80); /* TODO(johnicholas.hines@gmail.com): I think this requires that the string data is stored at the same spot in all the nodes that have string data */
    /* TODO(johnicholas.hines@gmail.com): This 80 magic number is no good, and strncpy isn't a great way to deal with strings anyway */
    return pt;
}
/* alloc_str */


node_ptr cons(node_ptr new_node, node_ptr list)
/*
// Construct a list. This routine allocates storage for a new cons node.
//   new_node points to the new head of the list. The tail pointer of the
//   new node points to list. This routine adds the new cons node to the
//   beginning of the list and returns a pointer to it. The list described
//   in the comments at the beginning of the program could be constructed
//   as cons(alloc_str('A'),cons(alloc_str('B'),cons(alloc_str('C'),NIL))).
*/
{
    node_ptr p;

    get_memory(&p, sizeof(struct node));
    p->tag= CONS_NODE;
    p->u.cons_node.head_ptr= new_node;
    p->u.cons_node.tail_ptr= list;
    return p;
}
/* cons */

node_ptr append_list(node_ptr list1, node_ptr list2)
/*
// Append list2 to list1. This routine returns a pointer to the
//   combined list. Appending is done by consing each item on the first
//   list to the second list. This routine is one of the major sources of
//   garbage so if garbage collection becomes a problem, you may want to
//   rewrite it.
*/
{
    return list1 ? cons(head(list1), append_list(tail(list1), list2)) : list2;
}
/* append_list */

counter list_length(node_ptr list)
/*
// returns the length of a list.
//   Note - both (A B C) and ( (A B) C D) have length 3.
*/
{
    return list ? 1 + list_length(list->u.cons_node.tail_ptr) : 0;
}
/* list_length */

void mark(node_ptr list)
/*
// Mark the blocks on list as being in use. Since a node may be on several
//    lists at one time, if it is already marked we don't continue processing
//    the tail of the list.
*/
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
/* mark */

/* TODO(johnicholas.hines@gmail.com): This function is acting as if different kinds of nodes take different amounts of memory, which is not true in the current C implementation. */
void unmark_mem()
/*
// Go through memory from initial_heap^ to HeapPtr^ and mark each node
//    as not in use. The tricky part here is updating the pointer p to point
//    to the next cell.
*/
{
    node_ptr p;
    counter string_base, node_allocation;

    string_base= sizeof(node_type) + sizeof(boolean); /* Johnicholas says: I think I've seen this somewhere - duplication? */
    node_allocation= sizeof(struct node);
    p= initial_heap;

    while (p < HeapPtr) {
        p->in_use= FALSE;
        switch (p->tag) {
        case CONS_NODE:
            p= p + node_allocation;
            break;
        case FREE_NODE:
            p= p + (p->u.free_node.block_cnt + 1) * 8;
            break;
        case FUNC: /* fall through */
        case CONSTANT: /* fall through */
        case VARIABLE:
            p= p + string_base + sizeof(p->u.constant.string_data) + 1;
            break;
        }
    }
}
/* unmark_mem */

void free_memory(node_ptr pt, counter size)
/*
// return size bytes pointed to by pt to the free list. If pt points to
//     a block next to the head of the free list combine it with the top
//     free node. total_free keeps track of the total number of free bytes.
*/
{
    counter blks;

    blks= ((size - 1) / 8) + 1;
    pt->tag= FREE_NODE;
    if (pt + 8 * blks == vtprolog_free) {
        pt->u.free_node.next_free= vtprolog_free->u.free_node.next_free;
        pt->u.free_node.block_cnt= vtprolog_free->u.free_node.block_cnt + blks;
        vtprolog_free= pt;
    } else if (vtprolog_free + 8 * (vtprolog_free->u.free_node.block_cnt + 1) == pt)
        vtprolog_free->u.free_node.block_cnt= vtprolog_free->u.free_node.block_cnt + blks;
    else {
        pt->u.free_node.next_free= vtprolog_free;
        pt->u.free_node.block_cnt= blks - 1;
        vtprolog_free= pt;
    }
    total_free= total_free + (blks * 8.0);
}
/* free_memory */

/* TODO(johnicholas.hines@gmail.com): Again, this is sweeping through memory, assuming these nodes have variable sizes,
// which they don't in the C version at the moment; there's duplication too.  */
void do_release(node_ptr heap_top)
/* This routine sweeps through memory and checks for nodes with in_use = false. */
{
    node_ptr p;
    counter string_allocation, block_allocation;

    p= initial_heap;
    while (p < heap_top) {
        switch (p->tag) {
        case CONS_NODE:
            if (!p->in_use) {
                free_memory(p, sizeof(struct node));
            }
            p= p + sizeof(struct node);
            break;
        case FREE_NODE:
            block_allocation= (p->u.free_node.block_cnt + 1) * 8;
            free_memory(p, block_allocation);
            p= p + block_allocation;
            break;
        case FUNC: /* fall through */
        case CONSTANT: /* fall through */
        case VARIABLE:
            string_allocation= sizeof(node_type) + sizeof(boolean) + sizeof(p->u.constant.string_data) + 1; /* TODO(johnicholas.hines@gmail.com): These computations are almost certainly completely irrelevant now */
            if (! p->in_use)
                free_memory(p, sizeof(node_type) + sizeof(boolean) + sizeof(p->u.constant.string_data) + 1);
            p= p + string_allocation;
            break;
        }
    }
}
/* do_release */

void release_mem()
/*
// This procedure does the actual collection and compaction of nodes.
//    This is the slow phase of garbage collection because of all the pointer
//    manipulation.
*/
{
    node_ptr heap_top;

    vtprolog_free= NULL;
    total_free= 0.0;
    heap_top= HeapPtr;
    do_release(heap_top);
}
/* release_mem */

void collect_garbage()
/*
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
*/
{
    printf("*");
    unmark_mem();
    mark(saved_list);
    release_mem();
    /* printf("%c", back_space); Maybe an ansi escape sequence would be appropriate here? */

    /* ClrEol ; // TODO(johnicholas.hines@gmail.com): Maybe an ansi escape sequence would be appropriate here? */

}
/* collect_garbage */

void test_memory()
/*
// This routine activates the garbage collector, if the the total available
//   memory (free_list + heap) is less than a specified amount. Lowering the
//   minimum causes garbage collection to be called less often, but if you
//   make it too small you may not have enough room left for recursion or any
//   temporary lists you need. Using 10000 is probably being overly
//   cautious.
*/
{
    /* Johnicholas says: This function is probably going to have to be completely rewritten. */
    /* memavail was a function that returned the amount of available free memory
       if ((memavail() * 16.0) + total_free < 10000) */
    collect_garbage();
}
/* test_memory */

void wait()
/*
// Just like it says. It waits for the user to press a key before
//   continuing.
*/
{
    char ch;

    printf("\n");
    printf("\n");
    printf("Press any key to continue. ");
    scanf("%c", &ch);
    printf("\n");

    /* ClrEol ; // TODO(johnicholas.hines@gmail.com): Maybe an ansi escape sequence would be appropriate here? or curses? or nothing? */
}
/* wait */


/*
// ------------------------------------------------------------------------
//      End of utility routines
// ------------------------------------------------------------------------
*/

/* TODO(johnicholas.hines@gmail.com): s should be pass-by-reference */
void read_kbd(string80 s)
/* Read a line from the keyboard */
{
    printf("-> ");
    getline(NULL, 0, stdin); /* TODO(johnicholas.hines@gmail.com): The line should go into s, not the bit bucket. */
}
/* read_kbd */

/* TODO(johnicholas.hines@gmail.com): Almost certainly, this, and eof_mark, will need to be completely rewritten. */
void read_a_line()
{
    /*
    // $I- // TODO(johnicholas.hines@gmail.com): What does this mean?
    // readln(f, line);
    // $I+ // TODO(johnicholas.hines@gmail.com): What does this mean?
    // if (ioresult != 0)
    //   line= eof_mark;
    // else if eof(f);
    // line= concat(line, eof_mark);
    */
}
/* read_a_line */

/* TODO(johnicholas.hines@gmail.com): f should be pass-by-reference */
void read_from_file(text_file f)
/*
// Read a line from file f and store it in the global variable line.
//   It ignores blank lines and when the end of file is reached an
//   eof_mark is returned.
*/
{
    line[0]= '\0';
    if (isatty(fileno(f)))
        read_kbd(line);
    else read_a_line();
    if (in_comment) {
        if (strstr(line, "*)") != NULL) {
            memmove(line, strstr("*)", line) + sizeof("*)"), line + sizeof(line) - strstr("*)", line)); /* Was: delete(line, 1, pos("*)", line) + 1); The intent here was to chop line to be shorter */
            in_comment= FALSE;
        }
    } else read_from_file(f);
    strncpy(saved_line, line, sizeof(saved_line));
}
/* read_from_file */

/* forward declaration */
void get_token(string132 t_line);

/* TODO(johnicholas.hines@gmail.com): t_line should be passed by ref */
void get_word(string132 t_line) {
    boolean done;
    int cn;
    int len;

    done= FALSE;
    len= strlen(t_line);
    /* TODO(johnicholas.hines@gmail.com): This is a bit crazy; replace with
    // strtok or getdelim if we can't just put in a new lexer. */
    for (cn= 0; cn < len && strchr(delim_set, t_line[cn]) == NULL; ++cn); /* deliberately empty loop body */
    strncpy(token, t_line, cn);
    memmove(t_line, t_line + cn, sizeof(t_line) - cn); /* chop t_line to be shorter */
}
/* get_word */

/* TODO(johnicholas.hines@gmail.com): t_line should be passed by ref. */
void comment(string132 t_line)
{
    if (strstr(t_line, "*)") != NULL) {
        memmove(t_line, strstr("*)", t_line), t_line + sizeof(t_line) - strstr("*)", t_line)); /* Was: delete(t_line, 1, pos("*)", t_line) + 1); The intent here was to chop t_line to be shorter. */
        get_token(line);
    } else {
        t_line[0]= '\0';
        token[0]= '\0';
        in_comment= TRUE;
    }
}
/* comment */

/* TODO(johnicholas.hines@gmail.com): t_line should be passed by ref */
void get_quote(string132 t_line)
{
    memmove(t_line, t_line + 1, sizeof(t_line) - 1);
    if (strchr(t_line, quote_char) == NULL) {
        strcpy(token, strcat("'", strndup(t_line, strchr(t_line, quote_char) - t_line)));
        memmove(t_line, t_line + 1, sizeof(t_line) - 1); /* Was delete(t_line, 1, pos(quote_char, t_line)); The intent here was to chop t_line to be shorter */
    } else {
        strcpy(token, t_line);
        t_line[0]= '\0';
    }
}
/* get_quote */

/* TODO(johnicholas.hines@gmail.com): t_line should be passed by ref */
void get_token(string132 t_line)
/*
// Extract a token from t_line. Comments are ignored. A token is
//   a string surrounded by delimiters or an end of line. Tokens may
//   contain embedded spaces if they are surrounded by quote marks.
*/
{
    {
        int i;
        char* temp;

        for (i= 0; t_line[i] == ' ' || t_line[i] == tab; ++i); /* note empty loop body */
        temp= strdup(t_line + i);
        free(t_line);
        t_line= temp;
    }
    if (strlen(t_line) > 0) {
        if (strncmp(t_line, "(*", 2) == 0) {
            comment(t_line);
        } else if (strncmp(t_line, ":-", 2) == 0 ||
                   strncmp(t_line, "?-", 2) == 0) {
            memcpy(token, t_line, 2);
            memmove(t_line, t_line + 2, sizeof(t_line) - 2);
        } else if (t_line[0] == quote_char) {
            get_quote(t_line);
        } else if (strchr(delim_set, t_line[0]) != NULL) {
            strncpy(token, t_line, 1);
            memmove(t_line, t_line + 1, sizeof(t_line) - 1);
        } else {
            get_word(t_line);
        }
    } else {
        token[0]= '\0';
    }
}
/* get_token */

/* TODO(johnicholas.hines@gmail.com): both f and token should be passed by reference */
void scan(text_file f, string80 token)
/*
// Scan repeatedly calls get_token to retreive tokens. When the
//   end of a line has been reached, read_from_file is called to
//   get a new line.
*/
{
    if (strlen(line) > 0) {
        get_token(line);
        if (strcmp(token, "") == 0)
            scan(f, token);
    } else {
        read_from_file(f);
        scan(f, token);
    }
}
/* scan */

node_ptr look_up(string80 var_str, node_ptr environ)
/*
// Search the environment list pointed to by environ for the variable,
//      var_str. If found return a pointer to var_str's binding, otherwise
//      return NIL
*/
{
    node_ptr p;

    for (p= environ; p; p= tail(p)) {
        if (strcmp(var_str, string_val(head(head(p)))) == 0)
            return tail(head(p));
    }
    return NULL;
}
/* look_up */

/* forward declarations */
void print_variable(char* to_print, node_ptr env);
void print_functor(node_ptr to_print, node_ptr env);
void print_components(node_ptr p, node_ptr env)

/*
// Print the components of a functor. These may be variables or
// other functors, so call the approriate routines to print them.
*/
{
    if (p == NULL) return;

    switch (tag_value(head(p))) {
    case CONSTANT:
        printf("%s ", string_val(head(p)));
        break;
    case VARIABLE:
        print_variable(string_val(head(p)), env);
        break;
    case CONS_NODE:
        print_functor(head(p), env);
        break;
    case FUNC: /* fall through */
    case FREE_NODE:
        assert(0);
        break;
    }
    if (tail(p) != NULL) {
        printf(",");
        print_components(tail(p), env);
    }
}
/* print_components */

void print_functor(node_ptr l, node_ptr env)
/* The variable was bound to a functor. Print the functor and its components. */
{
    if (l == NULL) return;

    printf("%s", string_val(head(l)));
    if (tail(l) != NULL) {
        printf("(");
        print_components(tail(l), env);
        printf(")");
    }
}
/* print_functor */

void print_variable(char* var_str, node_ptr env)
/*
// The varaible in question was bound to another varaible, so look
//        up that variable's binding and print it. If a match can't be found
//        print '_' to tell the user that the variable is anonymous.
*/
{
    node_ptr var_ptr;

    var_ptr= look_up(var_str, env);
    if (var_ptr == NULL) printf("_ ");

    switch (tag_value(head(var_ptr))) {
    case CONSTANT:
        printf("%s ", string_val(head(var_ptr)));
        break;
    case VARIABLE:
        print_variable(string_val(head(var_ptr)), env);
        break;
    case CONS_NODE:
        print_functor(head(var_ptr), env);
        break;
    case FUNC: /* fall through */
    case FREE_NODE:
        assert(0);
        break;
    }
}
/* print_variable */

/* TODO(johnicholas.hines@gmail.com): printed should be passed by reference. */
void print_bindings(node_ptr list, node_ptr env, boolean printed)
/*
// Print the bindings for level 0 variables only, intermediate variables
//     aren't of interest. The routine recursivley searches for the
//     end of the environments list and then prints the binding. This
//     is so that variables bound first are printed first.
*/
{
    if (list == NULL) return;

    print_bindings(tail(list), env, printed);
    if (strchr(string_val(head(head(list))), '#') == NULL) {
        printed= TRUE;
        printf("\n");
        printf("%s = ", string_val(head(head(list))));
        switch (tag_value(head(tail(head(list))))) {
        case CONSTANT:
            printf("%s ", string_val(head(tail(head(list)))));
            break;
        case VARIABLE:
            print_variable(string_val(head(tail(head(list)))), env);
            break;
        case CONS_NODE:
            print_functor(head(tail(head(list))), env);
            break;
        case FUNC: /* fall through */
        case FREE_NODE:
            assert(0);
            break;
        }
    }
}
/* print_bindings */

void runout(text_file source)
{
    while (strcmp(token, ".") != 0 && strcmp(token, eof_mark) != 0)
        scan(source, token);
}
/* runout */

void error(string80 error_msg, text_file source)
/*
// Signal an error. Prints saved_line to show where the error is located.
//    saved_line contains the current line being parsed. it is required,
//    because get_token chews up line as it reads tokens.
*/
{
    error_flag= TRUE;
    printf("\n");
    printf("%s", error_msg);
    printf("\n");
    printf("%s\n", saved_line);
    /* writeln('' : length(saved_line) - length(line) - 1,'^') ; // TODO(johnicholas.hines@gmail.com): Did this print a caret pointing at the error? */
    runout(source);
    wait();
}
/* error */

void do_exit(text_file source)
/*
// Exit the program. This really should be a built-in function and handled
//   in solve, but this does the trick.
*/
{
    if (strcmp(token, ".") != 0) {
        error("'.' expected.", source);
    } else {
        exit(0);
    }
}
/* do_exit */

void quoted_str(node_ptr* q_ptr, text_file source)
/* Process a quote */
{
    *q_ptr= append_list(*q_ptr, cons(alloc_str(CONSTANT,
                                     strndup(token+1, strlen(token) - 2)),
                                     NULL));
    scan(source, token);
}
/* quoted_str */

/* forward declaration */
void goal(node_ptr*, text_file source);

void head_list(node_ptr* h_ptr, text_file source)
{
    goal(h_ptr, source);
}
/* head */

void varbl(node_ptr* v_ptr, text_file source)
/*
// The current token is a varaible, allocate a node and return
//        a pointer to it.
*/
{
    *v_ptr= append_list(*v_ptr, cons(alloc_str(VARIABLE, token), NULL));
    scan(source, token);
}
/* varbl */

/* forward declaration */
void functor(node_ptr* t_ptr, string80 t_token, text_file source);

void term(node_ptr* t_ptr, text_file source)
/* Process a single term. The new term is appended to t_ptr.
//
// Here, the CFG production we are modeling is:
// term ::= VARBL | ' quoted_str | ID ( functor | ID .
*/
{
    string80 t_token;

    if (isupper(token[0]) || token[0] == '_') {
        varbl(t_ptr, source);
    } else if (token[0] == quote_char) {
        quoted_str(t_ptr, source);
    } else if (isalnum(token[0])) {
        strncpy(t_token, token, sizeof(t_token));
        scan(source, token);
        if (strcmp(token, "(") == 0) {
            functor(t_ptr, t_token, source);
        } else {
            *t_ptr= append_list(*t_ptr,
                                cons(alloc_str(CONSTANT, t_token), NULL));
        }
    } else {
        error("Illegal Symbol.", source);
    }
}
/* term */

void components(node_ptr* cm_ptr, text_file source)
/*
// Process the components of the functor. The components are terms
//      seperated by commas. On exit, cm_ptr points to the list of
//      components.
//
// Here, the CFG component we are modeling is:
// components ::= term | term , components .
*/
{
    term(cm_ptr, source);
    if (strcmp(token, ",") == 0) {
        scan(source, token);
        components(cm_ptr, source);
    }
}
/* components */

void functor(node_ptr* f_ptr, string80 func_token, text_file source)
/*
// The current goal is a functor. This routine allocates a node
//   to store the functor and then processes the components of the
//   functor. On exit, f_ptr points to the list containing the functor
//   and its components. func_token contains the functor name.
//
// Here, the CFG production that we are modeling is:
// functor ::= components ) .
*/
{
    node_ptr c_ptr;

    c_ptr= cons(alloc_str(FUNC, func_token), NULL);
    scan(source, token);
    components(&c_ptr, source);
    if (strcmp(token, ")") == 0) {
        *f_ptr= append_list(*f_ptr, cons(c_ptr, NULL));
        scan(source, token);
    } else {
        error("Missing ')'.", source);
    }
}
/* functor */

/* TODO(johnicholas.hines@gmail.com): l_ptr ought to be passed by reference */
void goal(node_ptr* l_ptr, text_file source)
/*
// Read a goal. The new goal is appended to l_ptr. Each goal is appended
//    to l_ptr as a list. Thus, the sentence 'likes(john,X) :- likes(X,wine) .'
//    becomes the list ( (likes john X) (likes X wine) )
//
// goal ::= quoted_str | ID ( functor | ID .
*/
{
    string80 goal_token;

    if (!islower(token[0]) && token[0] != quote_char) {
        error("A goal must begin with 'a .. z' or be a quoted string.", source);
        return;
    }
    if (token[0] == quote_char) {
        *l_ptr= append_list(*l_ptr,
                            cons(cons(alloc_str(CONSTANT,
                                                strndup(token+1, sizeof(token) - 2)), NULL), NULL));
        scan(source, token);
    } else {
        strncpy(goal_token, token, sizeof(goal_token));
        scan(source, token);
        if (strcmp(token, "(") == 0) {
            functor(l_ptr, goal_token, source);
        } else
            *l_ptr= append_list(*l_ptr,
                                cons(cons(alloc_str(CONSTANT, goal_token),
                                          NULL), NULL));
    }
}
/* goal */

/* TODO(johnicholas.hines@gmail.com): t_ptr should be passed by reference */
void tail_list(node_ptr* t_ptr, text_file source)
/*
// Process the tail of a rule. Since the a query is syntactically identical
//    to the tail of a rule, this routine is used to compile queries.
//    On exit, t_ptr points to the list containing the tail.
//
// tail_list ::= goal | goal , tail_list .
*/
{
    goal(t_ptr, source);
    if (strcmp(token, ",") == 0) {
        scan(source, token);
        tail_list(t_ptr, source);
    }
}
/* tail */

void rule(text_file source)
/*
// Procees a rule, actually any sentence. If no error occurs the
//    new sentence is appended to the data base.
//
// rule ::= head_list | head_list :- tail_list .
*/
{
    node_ptr r_ptr;

    saved_list= cons(data_base, NULL);
    test_memory();
    r_ptr= NULL;
    head_list(&r_ptr, source);
    if (strcmp(token, ":-") == 0) {
        scan(source, token);
        tail_list(&r_ptr, source);
    }
    if (strcmp(token, ".") != 0) error("'.' expected.", source);
    if (!error_flag) data_base= append_list(data_base, cons(r_ptr, NULL));
}
/* rule */

/* TODO(johnicholas.hines@gmail.com): solved should be passed by ref */
void check_continue(node_ptr env, boolean* solved)
/*
// Print the bindings and see if the user is satisfied. If nothing
//      is printed from the environment, then print 'Yes' to indicate
//      that the query was successfully satisfied.
*/
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
        ch= getchar(); /* TODO(johnicholas.hines@gmail.com): getchar actually returns an int, and it might be EOF, which is not a char. */
    } while (strchr("\n;", ch) == NULL);
    *solved= (ch == '\n');
    printf("\n");
}
/* check_continue */

/* forward declaration */
node_ptr copy_list(node_ptr to_copy, counter copy_level);

/* TODO(johnicholas.hines@gmail.com): to_list should be passed by reference */
void list_copy(node_ptr from_list, node_ptr to_list, char* level_str, counter copy_level)
{
    if (from_list == NULL) return;

    switch (from_list->tag) {
    case VARIABLE:
        to_list= alloc_str(VARIABLE, strcat(from_list->u.constant.string_data, level_str));
        break;
    case FUNC: /* fall through */
    case CONSTANT:
        to_list= from_list;
        break;
    case CONS_NODE:
        list_copy(tail(from_list), to_list, level_str, copy_level);
        to_list= cons(copy_list(head(from_list), copy_level), to_list);
        break;
    case FREE_NODE:
        assert(0);
        break;
    }
}
/* list_copy */

node_ptr copy_list(node_ptr list, counter copy_level)
/* Copy a list and append the copy_level (recursion level) to all variables. */
{
    node_ptr temp_list;
    char level_str[6];

    snprintf(level_str, sizeof(level_str), "#%d", copy_level);
    temp_list= NULL;
    list_copy(list, temp_list, level_str, copy_level);
    return temp_list;
}
/* copy_list */

void make_binding(node_ptr l1, node_ptr l2, node_ptr* env)
/*
// Bind a variable to the environment. Anonymous variables are not bound.
//       l1 points to the variable and l2 points to its binding.
*/
{
  if (string_val(head(l1))[0] != '_') {
    *env= cons(cons(head(l1), l2), *env);
  }
}
/* make_binding */

void fail(boolean* unify_return_value, node_ptr* new_environ, node_ptr environ)
/* Unification failed. */
{
    *unify_return_value= FALSE;
    *new_environ= environ;
}
/* fail */

void unify_constant(node_ptr list1, node_ptr list2, boolean* unify_return_value)
/*
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
*/
{
    switch (tag_value(head(list2))) {
    case CONSTANT:
        if (strcmp(string_val(head(list1)), string_val(head(list2))) == 0) *unify_return_value= TRUE;
	else fail(unify_return_value, new_environ, environ);
        break;
    case VARIABLE:
        var_ptr= look_up(string_val(head(list2)), environ);
        if (var_ptr == NULL) make_binding(list2, list1, &new_environ);
        else *unify_return_value= unify(list1, var_ptr, environ, new_environ);
        break;
    case CONS_NODE: /* fall through */
    case FUNC:
        fail(unify_return_value, new_environ, environ);
        break;
    case FREE_NODE:
        assert(0);
        break;
    }
}
/* unify_constant */

void unify_tail()
/* This routine does the term by term unification of the component lists */
{
    node_ptr p;
    node_ptr q;
    boolean unified;

    p= tail(list1);
    q= tail(list2);
    unified= TRUE;
    new_environ= environ;
    while (p != NULL && unified) {
        unified= unified && unify(cons(head(p), NULL), cons(head(q), NULL), new_environ, new_environ);
        p= tail(p);
        q= tail(q);
    }
    if (!unified)
        fail(unify_return_value, new_environ, environ);
}
/* unify_tail */

void unify_func()
/*
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
*/
{
    switch (tag_value(head(list2))) {
    case CONSTANT:
        fail(unify_return_value, new_environ, environ);
        break;
    case VARIABLE:
        var_ptr= look_up(string_val(head(list2)), environ);
        if (var_ptr == NULL) {
            make_binding(list2, list1, &environ);
        } else
            return unify(list1, var_ptr, environ, new_environ);
        break;
    case FUNC:
        if (strcmp(string_val(head(list1)), string_val(head(list2))) == 0) {
            if (list_length(tail(list1)) == list_length(tail(list2))) {
                unify_tail();
            } else fail(unify_return_value, new_environ, environ);
        } else fail(unify_return_value, new_environ, environ);
        break;
    case CONS_NODE:
        fail(unify_return_value, new_environ, environ);
        break;
    case FREE_NODE:
        assert(0);
        break;
    }
}
/* unify_func */

void unify_expr(boolean* unify_return_value, node_ptr* new_environ, node_ptr* environ)
/*
// List1 contains an expression. Try to unify it with list2. The 4 cases
//       are:
//        list2 contains
//         constant  - can't be unified.
//         variable  - look up binding, if no current binding bind the
//                     functor to the variable, otherwise unify list1
//                     with the binding.
//         cons_node - If the heads can be unified, the unify the tails.
//         func      - fail
*/
{
    switch (tag_value(head(list2))) {
    case CONSTANT:
        fail(unify_return_value, new_environ, environ);
        break;
    case VARIABLE:
        var_ptr= look_up(string_val(head(list2)), environ);
        if (var_ptr == NULL) {
            make_binding(&list2, list1, environ);
        } else {
            unify(list1, var_ptr, environ, new_environ);
        }
        break;
    case FUNC:
        fail(unify_return_value, new_environ, environ);
        break;
    case CONS_NODE:
        if (unify(head(list1), head(list2), environ, new_environ)) {
            unify(tail(list1), tail(list2), new_environ, new_environ);
        } else {
            fail(unify_return_value, new_environ, environ);
        }
        break;
    case FREE_NODE:
        assert(0);
        break;
    }
}
/* unify_expr */

boolean unify(node_ptr list1, node_ptr list2, node_ptr environ, node_ptr* new_environ)
/*
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
*/
{
    node_ptr var_ptr;
    boolean unify_return_value;

    if (list1 == NULL && list2 == NULL) {
        unify_return_value= TRUE;
        new_environ= environ;
    } else if (list1 == NULL) {
        fail(&unify_return_value, new_environ, environ);
    } else if (list2 == NULL) {
        fail(&unify_return_value, new_environ, environ);
    } else {
        switch (tag_value(head(list1))) {
        case CONSTANT:
            unify_constant();
            break;
        case VARIABLE:
            var_ptr= look_up(string_val(head(list1)), environ);
            if (var_ptr == NULL) {
                make_binding(list1, list2, environ);
            } else {
                return unify(var_ptr, list2, environ, new_environ);
            }
            break;
        case FUNC: unify_func(&unify_return_value); break;
        case CONS_NODE: unify_expr(&unify_return_value); break;
        case FREE_NODE: assert(0); break;
        }
    }
    return unify_return_value;
}
/* unify */

void solve(node_ptr list, node_ptr env, counter level, boolean* solved)
/*
//   This is where all the hard work is done. This routine follows the
//     steps outlined in the article. list is the query to be soved, env is
//     the current environment and level is the recursion level. level can
//     only get to 32767, but you'll run out of stack space long before you
//     get that far.
//     solve saves list and env on the saved list so that they won't be
//     destroyed by garbage collection. The data base is always on the
//     saved list. At the end of solve, list and env are removed from
//     saved_list.
*/
{
    node_ptr new_env;
    node_ptr p;

    saved_list= cons(list, cons(env, saved_list));
    if (list == NULL) {
        check_continue(env, solved);
        return;
    }
    for (p= data_base; p != NULL && !solved; ) {
        test_memory();
        if (unify(copy_list(head(head(p)), level), head(list), env, new_env)) {
            solve(append_list(copy_list(tail(head(p)), level), tail(list)), new_env, level + 1, solved);
            p= tail(p);
        }
    }
    saved_list= tail(tail(saved_list));
}
/* solve */

void query(text_file source)
/*
// Process a query. Compile the query, and then call solve to search the
//    data base. q_ptr points to the compiled query and solved is a boolean
//    indicating whether the query was successfully solved.
*/
{
    node_ptr q_ptr;
    boolean solved;

    q_ptr= NULL;
    tail_list(q_ptr, source);
    if (strcmp(token, ".") != 0) {
        error("'.' expected.", source);
    } else if (!error_flag) {
        solved= FALSE;
        saved_list= cons(data_base, NULL);
        solve(q_ptr, NULL, 0, &solved);
        if (!solved) {
            printf("No\n");
        }
    }
}
/* query */

/* forward declaration */
void compile(text_file);

void read_new_file(text_file source)
/*
// Read source statements from a new file. When all done, close file
//    and continue reading from the old file. Files may be nested, but you
//    will run into trouble if you nest them deaper than 15 levels. This
//    is Turbo's default for open files.
*/
{
    text_file new_file;
    string132 old_line, old_save;
    string80 f_name;

    if (token[0] == quote_char) {
        memmove(token, token + 1, sizeof(token) - 1);
    }
    if (strchr(token, '.') == NULL) {
        snprintf(f_name, sizeof(f_name), "%s.pro", token);
    } else {
        strncpy(f_name, token, sizeof(f_name));
    }
    new_file= fopen(f_name, "r");
    if (new_file) {
        strncpy(old_line, line, sizeof(old_line));
        strncpy(old_save, saved_line, sizeof(old_save));
        line[0]= '\0';
        compile(new_file);
        /* Was close(new_file); TODO(johnicholas.hines@gmail.com): replace this */
        strncpy(line, old_line, sizeof(line));
        strncpy(saved_line, old_save, sizeof(saved_line));
        scan(source, token);
        if (strcmp(token, ".") != 0) {
            error("'.' expected.", source);
        }
    } else {
        error(strcat("Unable to open ", f_name), source);
    }
}
/* read_new_file */

/* TODO(johnicholas.hines@gmail.com): source should be passed by reference */
void compile(text_file source)
/*
// The recursive descent compiler. It reads tokens until the token
//   'EXIT' is found. If the token is '?-', a query is performed, a '@' token
//   is the command to read a new file and source statements are read form that
//   file, otherwise the token is assumed to be part of a sentence and the rest
//   of the sentence is parsed.
*/
{
    for (scan(source, token); token != eof_mark; scan(source, token)) {
        error_flag= FALSE;
        if (strcmp(token, "?-") == 0) {
            scan(source, token); /* read past the match */
            query();
        } else if (strcmp(token, "@") == 0) {
            scan(source, token);  /* read past the match */
            read_new_file();
        } else if (strcmp(token, "EXIT") == 0 || strcmp(token, "exit") == 0) {
            scan(source, token);  /* read past the match */
            do_exit(source);
        } else {
            rule(source);
        }
    }
}
/* compile */

void initialize()
/* Write a heading line and initialize the global variables */
{
    /* clrscr(); */
    printf("\n");
    printf("Very Tiny Prolog - Version 1.0     [c] 1986 MicroExpert Systems\n");
    printf("\n");
    in_comment= FALSE;

    /* delim_set := [' ',')','(',',','[',']',eof_mark,tab,quote_char,':',
    // '@','.','?'] ; */
    /* text_chars := [' ' .. '~'] ; */

    line[0]= '\0';
    data_base= NULL;
    vtprolog_free= NULL;
    saved_list= NULL;
    total_free= 0.0;
    initial_heap= HeapPtr;
}
/* initialize */

#ifndef BUILD_AS_LIBRARY
int main() {
    initialize();
    compile(stdin);
    return 0;
}
#endif
