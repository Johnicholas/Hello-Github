(*$V-,R+,B- *)
PROGRAM very_tiny_prolog ;

(* Copyright 1986 - MicroExpert Systems
                    Box 430 R.D. 2
                    Nassau, NY 12123       *)

(* VTPROLOG implements the data base searching and pattern matching of
   PROLOG. It is described in "PROLOG from the Bottom Up" in issues
   1 and 2 of AI Expert.

   This program has been tested using Turbo ver 3.01A on an IBM PC. It has
   been run under both DOS 2.1 and Concurrent 4.1 .

   We would be pleased to hear your comments, good or bad, or any applications
   and modifications of the program. Contact us at:

     AI Expert
     CL Publications Inc.
     650 Fifth St.
     Suite 311
     San Francisco, CA 94107

   or on the AI Expert BBS. Our id is BillandBev Thompson. You can also
   contact us on BIX, our id is bbt.

   Bill and Bev Thompson    *)

 CONST
  debug = false ;
  back_space = ^H ;
  tab = ^I ;
  eof_mark = ^Z ;
  esc = #27 ;
  quote_char = #39 ;
  left_arrow = #75 ;
  end_key = #79 ;
  del_line = ^X ;
  return = ^M ;
  bell = ^G ;

 TYPE
  counter = 0 .. maxint ;
  string80 = string[80] ;
  string132 = string[132] ;
  string255 = string[255] ;
  text_file = text ;
  char_set = SET OF char ;
  node_type = (cons_node,func,variable,constant,free_node) ;
  node_ptr = ^node ;
  node = RECORD
          in_use : boolean ;
          CASE tag : node_type OF
           cons_node : (tail_ptr : node_ptr ;
                        head_ptr : node_ptr) ;
           func,
           constant,
           variable  : (string_data : string80) ;
           free_node : (next_free : node_ptr ;
                        block_cnt : counter) ;
          END ;

(* node is the basic allocation unit for lists. The fields are used as
   follows:

    in_use     - in_use = false tells the garbage collector that this node
                 is available for re-use.
    tag        - which kind of node this is.
    cons_node  - cons_nodes consist of two pointers. one to the head (first item)
                 the other to the rest of the list. They are the "glue" which
                 holds the list together. The list (A B C) would be stored as
                   -------         --------          --------
                   | .| . |----->  |  .| . |------> |  .| . |---> NIL
                   --|-----         --|------        --|-----
                     |                |                |
                     V                V                V
                     A                B                C

                 The boxes are the cons nodes, the first part of the box
                 holds the head pointer, then second contains the tail.
    constant   - holds string values, we don't actually use the entire 80
                 characters in most cases.
    variable   - also conatins a string value, these nodes will be treated as
                 PROLOG variables rather than constants.
    free_node  - the garbage collector gathers all unused nodes and puts
                 them on a free list. It also compacts the free space into
                 contiguous blocks. next_free points to the next free block.
                 block_cnt contains a count of the number of contiguous 8 byte free
                 blocks which follow this one.    *)


 VAR
  line,saved_line : string132 ;
  token : string80 ;
  source_file : text_file ;
  error_flag,in_comment : boolean ;
  delim_set,text_chars : char_set ;
  data_base,initial_heap,free,saved_list : node_ptr ;
  total_free : real ;

(* The important globals are:
   source_file  - text file containing PROLOG statements.
   line         - line buffer for reading in the text file
   saved_list   - list of all items that absolutely must be saved if garbage
                  collection occurs. Usually has at least the data_base and
                  the currents query attached to it.
   initial_heap - the value of the heap pointer at the start of the program.
                  used by the garbage collector
   free         - the list of free nodes.
   total_free   - total number of free blocks on the free list.
   data_base    - a pointer to the start of the data base. It points to a
                  node pointing to the first sentence in the data base. Nodes
                  pointing to sentences are linked together to form the data
                  base.
   delim_set    - set of characters which delimit tokens. *)


(* ----------------------------------------------------------------------
        Utility Routines
   ---------------------------------------------------------------------- *)

 PROCEDURE noise ;
  (* Make a noise on the terminal - used for warnings. *)
  BEGIN
   write(bell) ;
  END ; (* noise *)

 FUNCTION open(VAR f : text_file ; f_name : string80) : boolean ;
  (* open a file - returns true if the file exists and was opened properly
     f      - file pointer
     f_name - external name of the file *)
  BEGIN
   assign(f,f_name) ;
   (*$I- *)
   reset(f) ;
   (*$I+ *)
   open := (ioresult = 0) ;
  END ; (* open *)


 FUNCTION is_console(VAR f : text_file) : boolean ;
  (* return true if f is open on the system console
     for details of fibs and fib_ptrs see the Turbo Pascal ver 3.0 reference
     manual chapter 20. This should work under CP/M-86 or 80, but we haven't
     tried it. *)
  TYPE
   fib = ARRAY [0 .. 75] OF byte ;
  VAR
   fib_ptr : ^fib ;
   dev_type : byte ;
  BEGIN
   fib_ptr := addr(f) ;
   dev_type := fib_ptr^[2] AND $07 ;
   is_console := (dev_type = 1) OR (dev_type = 2) ;
  END ; (* is_console *)


 PROCEDURE strip_leading_blanks(VAR s : string80) ;
  BEGIN
   IF length(s) > 0
    THEN
     IF (s[1] = ' ') OR (s[1] = tab)
      THEN
       BEGIN
        delete(s,1,1) ;
        strip_leading_blanks(s) ;
       END ;
  END ; (* strip_leading_blanks *)


 PROCEDURE strip_trailing_blanks(VAR s : string80) ;
  BEGIN
   IF length(s) > 0
    THEN
     IF (s[length(s)] = ' ') OR (s[length(s)] = tab)
      THEN
       BEGIN
        delete(s,length(s),1) ;
        strip_trailing_blanks(s) ;
       END ;
  END ; (* strip_trailing_blanks *)



 FUNCTION toupper(s : string80) : string80 ;
  (* returns s converted to upper case *)
  VAR
   i : byte ;
  BEGIN
   IF length(s) > 0
    THEN
     FOR i := 1 TO length(s) DO
      s[i] := upcase(s[i]) ;
   toupper := s ;
  END ; (* toupper *)


 FUNCTION is_number(s : string80) : boolean ;
  (* checks to see if s contains a legitimate numerical string.
     It ignores leading and trailing blanks *)
  VAR
   num : real ;
   code : integer ;
  BEGIN
   strip_trailing_blanks(s) ;
   strip_leading_blanks(s) ;
   IF s <> ''
    THEN val(s,num,code)
    ELSE code := -1 ;
   is_number := (code = 0) ;
  END ; (* is_number *)


 FUNCTION head(list : node_ptr) : node_ptr ;
  (* returns a pointer to the first item in the list.
     If the list is empty, it returns NIL.  *)
  BEGIN
   IF list = NIL
    THEN head := NIL
    ELSE head := list^.head_ptr ;
  END ; (* head *)


 FUNCTION tail(list : node_ptr) : node_ptr ;
  (* returns a pointer to a list starting at the second item in the list.
     Note - tail( (a b c) ) points to the list (b c), but
            tail( ((a b) c d) ) points to the list (c d) .  *)
  BEGIN
   IF list = NIL
    THEN tail := NIL
   ELSE
    CASE list^.tag OF
     cons_node : tail := list^.tail_ptr ;
     free_node : tail := list^.next_free ;
     ELSE        tail := NIL ;
    END ;
  END ; (* tail *)


 FUNCTION allocation_size(x : counter) : counter ;
  (* Turbo 3.0 allocates memory in 8 byte blocks, this routine calculates the
     actual number of bytes returned for a request of x bytes.  *)
  BEGIN
   allocation_size := (((x - 1) DIV 8) + 1) * 8 ;
  END ; (* allocation_size *)


 FUNCTION node_size : counter ;
  (* calculates the base size of a node. Add the rest of the node to this
     to get the actual size of a node *)
  BEGIN
   node_size := 2 * sizeof(node_ptr) + sizeof(boolean) + sizeof(node_type) ;
  END ; (* node_size *)


 FUNCTION normalize(pt : node_ptr) : node_ptr ;
  (* returns a normalized pointer. Pointers are 32 bit addresses. The first
     16 bits contain the segment number and the second 16 bits contain the
     offset within the segment. Normalized pointers have offsets in the range
     $0 to $F (0 .. 15)    *)
  VAR
   pt_seg,pt_ofs : integer ;
  BEGIN
   pt_seg := seg(pt^) + (ofs(pt^) DIV 16) ;
   pt_ofs := ofs(pt^) MOD 16 ;
   normalize := ptr(pt_seg,pt_ofs) ;
  END ; (* normalize *)


 FUNCTION string_val(list : node_ptr) : string80 ;
  (* returns the string pointed to by list. If list points to a number
     node, it returns a string representing that number *)
  VAR
   s : string[15] ;
  BEGIN
   IF list = NIL
    THEN string_val := ''
   ELSE IF list^.tag IN [constant,variable,func]
    THEN string_val := list^.string_data
   ELSE string_val := '' ;
  END ; (* string_val *)


 FUNCTION tag_value(list : node_ptr) : node_type ;
  (* returns the value of the tag for a node.     *)
  BEGIN
   IF list = NIL
    THEN tag_value := free_node
    ELSE tag_value := list^.tag ;
  END ; (* tag_value *)


 PROCEDURE print_list(list : node_ptr) ;
  (* recursively traverses the list and prints its elements. This is
     not a pretty printer, so the lists may look a bit messy.  *)
  VAR
   p : node_ptr ;
  BEGIN
   IF list <> NIL
    THEN
     CASE list^.tag OF
      constant,
      func,
      variable  : write(string_val(list),' ') ;
      cons_node : BEGIN
                   write('(') ;
                   p := list ;
                   WHILE p <> NIL DO
                    BEGIN
                     print_list(head(p)) ;
                     p := tail(p) ;
                    END ;
                   write(') ') ;
                  END ;
     END ;
  END ; (* print_list *)


 PROCEDURE get_memory(VAR p : node_ptr ; size : counter) ;
  (* On exit p contains a pointer to a block of allocation_size(size) bytes.
     If possible this routine tries to get memory from the free list before
     requesting it from the heap *)
  VAR
   blks : counter ;
   allocated : boolean ;

  PROCEDURE get_from_free(VAR list : node_ptr) ;
   (* Try and get need memory from the free list. This routine uses a
      first-fit algorithm to get the space. It takes the first free block it
      finds with enough storage. If the free block has more storage than was
      requested, the block is shrunk by the requested amount.  *)
   BEGIN
    IF list <> NIL
     THEN
      IF list^.block_cnt >= (blks - 1)
       THEN
        BEGIN
         p := normalize(ptr(seg(list^),ofs(list^) +
                                       (list^.block_cnt - blks + 1) * 8)) ;
         IF list^.block_cnt = blks - 1
          THEN list := list^.next_free
          ELSE list^.block_cnt := list^.block_cnt - blks ;
         allocated := true ;
         total_free := total_free - (blks * 8.0) ;
        END
       ELSE get_from_free(list^.next_free) ;
   END ; (* get_from_free *)

  BEGIN
   blks := ((size - 1) DIV 8) + 1 ;
   allocated := false ;
   get_from_free(free) ;
   IF NOT allocated
    THEN getmem(p,blks * 8) ;
  END ; (* get_memory *)


 FUNCTION alloc_str(typ : node_type ; s : string80) : node_ptr ;
  (* Allocate storage for a string and return a pointer to the new node.
     This routine only allocates enough storage for the actual number of
     characters in the string plus one for the length. Because of this,
     concatenating anything to the end of a string stored in a symbol node
     will lead to disaster. Copy the string to a new string do the
     concatenation and then allocate a new node.  *)
  VAR
   pt : node_ptr ;
  BEGIN
   get_memory(pt,allocation_size(sizeof(node_type) + sizeof(boolean) +
                                 length(s) + 1)) ;
   pt^.tag := typ   ;
   pt^.string_data := s ;
   alloc_str := pt ;
  END ; (* alloc_str *)


 FUNCTION cons(new_node,list : node_ptr) : node_ptr ;
  (* Construct a list. This routine allocates storage for a new cons node.
     new_node points to the new head of the list. The tail pointer of the
     new node points to list. This routine adds the new cons node to the
     beginning of the list and returns a pointer to it. The list described
     in the comments at the beginning of the program could be constructed
     as cons(alloc_str('A'),cons(alloc_str('B'),cons(alloc_str('C'),NIL))). *)
  VAR
   p : node_ptr ;
  BEGIN
   get_memory(p,allocation_size(node_size)) ;
   p^.tag := cons_node ;
   p^.head_ptr := new_node ;
   p^.tail_ptr := list ;
   cons := p ;
  END ; (* cons *)


 FUNCTION append_list(list1,list2 : node_ptr) : node_ptr ;
  (* Append list2 to list1. This routine returns a pointer to the
     combined list. Appending is done by consing each item on the first
     list to the second list. This routine is one of the major sources of
     garbage so if garbage collection becomes a problem, you may want to
     rewrite it. *)
  BEGIN
   IF list1 = NIL
    THEN append_list := list2
    ELSE append_list := cons(head(list1),append_list(tail(list1),list2)) ;
  END ; (* append_list *)


 FUNCTION list_length(list : node_ptr) : counter ;
  (* returns the length of a list.
     Note - both (A B C) and ( (A B) C D) have length 3.   *)
  BEGIN
   IF list = NIL
    THEN list_length := 0
    ELSE list_length := 1 + list_length(list^.tail_ptr) ;
  END ; (* list_length *)


 PROCEDURE collect_garbage ;
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

  FUNCTION lower(p1,p2 : node_ptr) : boolean ;
   (* returns true if p1 points to a lower memory address than p2 *)
   BEGIN
    p1 := normalize(p1) ;
    p2 := normalize(p2) ;
    lower := (seg(p1^) < seg(p2^)) OR
              ((seg(p1^) = seg(p2^)) AND (ofs(p1^) < ofs(p2^))) ;
   END ; (* lower *)

  PROCEDURE mark(list : node_ptr) ;
   (* Mark the blocks on list as being in use. Since a node may be on several
      lists at one time, if it is already marked we don't continue processing
      the tail of the list. *)
   BEGIN
    IF list <> NIL
     THEN
      BEGIN
       IF NOT list^.in_use
        THEN
         BEGIN
          list^.in_use := true ;
          IF list^.tag = cons_node
           THEN
            BEGIN
             mark(head(list)) ;
             mark(tail(list)) ;
            END ;
         END ;
      END ;
   END ; (* mark *)

  PROCEDURE unmark_mem ;
   (* Go through memory from initial_heap^ to HeapPtr^ and mark each node
      as not in use. The tricky part here is updating the pointer p to point
      to the next cell. *)
   VAR
    p : node_ptr ;
    string_base,node_allocation : counter ;
   BEGIN
    string_base := sizeof(node_type) + sizeof(boolean) ;
    p := normalize(initial_heap) ;
    node_allocation := allocation_size(node_size) ;
    WHILE lower(p,HeapPtr) DO
     BEGIN
      p^.in_use := false ;
      CASE p^.tag OF
       cons_node : p := normalize(ptr(seg(p^),ofs(p^) + node_allocation)) ;
       free_node : p := normalize(ptr(seg(p^),ofs(p^) + (p^.block_cnt + 1) * 8)) ;
       func,
       constant,
       variable  : p := normalize(ptr(seg(p^),
                                  ofs(p^) +
                                  allocation_size(string_base +
                                                  length(p^.string_data) + 1))) ;
      END ;
     END ;
   END ; (* unmark_mem *)

  PROCEDURE release_mem ;
   (* This procedure does the actual collection and compaction of nodes.
      This is the slow phase of garbage collection because of all the pointer
      manipulation.  *)
   VAR
    heap_top : node_ptr ;
    string_base,node_allocation,string_allocation,block_allocation : counter ;

   PROCEDURE free_memory(pt : node_ptr ; size : counter) ;
    (* return size bytes pointed to by pt to the free list. If pt points to
       a block next to the head of the free list combine it with the top
       free node. total_free keeps track of the total number of free bytes. *)
    VAR
     blks : counter ;
    BEGIN
     blks := ((size - 1) DIV 8) + 1 ;
     pt^.tag := free_node ;
     IF normalize(ptr(seg(pt^),ofs(pt^) + 8 * blks)) = free
      THEN
       BEGIN
        pt^.next_free := free^.next_free ;
        pt^.block_cnt := free^.block_cnt + blks ;
        free := pt ;
       END
     ELSE IF normalize(ptr(seg(free^),ofs(free^) + 8 * (free^.block_cnt + 1))) =
             normalize(pt)
      THEN free^.block_cnt := free^.block_cnt + blks
     ELSE
      BEGIN
       pt^.next_free := free ;
       pt^.block_cnt := blks - 1 ;
       free := pt ;
      END ;
     total_free := total_free + (blks * 8.0) ;
    END ; (* free_memory *)

   PROCEDURE do_release ;
    (* This routine sweeps through memory and checks for nodes with
       in_use = false. *)
    VAR
     p : node_ptr ;
    BEGIN
     p := normalize(initial_heap) ;
     WHILE lower(p,heap_top) DO
      CASE p^.tag OF
       cons_node : BEGIN
                    IF NOT p^.in_use
                     THEN free_memory(p,node_size) ;
                    p := normalize(ptr(seg(p^),ofs(p^) + node_allocation)) ;
                   END ;
       free_node : BEGIN
                    block_allocation := (p^.block_cnt + 1) * 8 ;
                    free_memory(p,block_allocation) ;
                    p := normalize(ptr(seg(p^),ofs(p^) + block_allocation)) ;
                   END ;
       func,
       constant,
       variable  : BEGIN
                    string_allocation := allocation_size(string_base +
                                                length(p^.string_data) + 1) ;
                    IF NOT p^.in_use
                     THEN free_memory(p,string_base + length(p^.string_data)
                                      + 1) ;
                    p := normalize(ptr(seg(p^),ofs(p^) + string_allocation)) ;
                   END ;
      END ;
    END ; (* do_release *)

   BEGIN
    free := NIL ;
    total_free := 0.0 ;
    heap_top := HeapPtr ;
    string_base := sizeof(node_type) + sizeof(boolean) ;
    node_allocation := allocation_size(node_size) ;
    do_release ;
   END ; (* release_mem *)

  BEGIN
   write('*') ;
   unmark_mem ;
   mark(saved_list) ;
   release_mem ;
   write(back_space) ;
   clreol ;
  END ; (* collect_garbage *)


 PROCEDURE test_memory ;
  (* This routine activates the garbage collector, if the the total available
     memory (free_list + heap) is less than a specified amount. Lowering the
     minimum causes garbage collection to be called less often, but if you
     make it too small you may not have enough room left for recursion or any
     temporary lists you need. Using 10000 is probably being overly
     cautious.   *)
  BEGIN
   IF (memavail * 16.0) + total_free < 10000
    THEN collect_garbage ;
  END ; (* test_memory *)


 PROCEDURE wait ;
  (* Just like it says. It waits for the user to press a key before
     continuing. *)
  VAR
   ch : char ;
  BEGIN
   writeln ;
   writeln ;
   write('Press any key to continue. ') ;
   read(kbd,ch) ;
   write(return) ;
   clreol ;
  END ; (* wait *)


(* ------------------------------------------------------------------------
        End of utility routines
   ------------------------------------------------------------------------ *)

 PROCEDURE read_kbd(VAR s : string80) ;
  (* Read a line from the keyboard *)
  BEGIN
   write('-> ') ;
   readln(s) ;
  END ; (* read_kbd *)


 PROCEDURE read_from_file(VAR f : text_file) ;
  (* Read a line from file f and store it in the global variable line.
     It ignores blank lines and when the end of file is reached an
     eof_mark is returned. *)

  PROCEDURE read_a_line ;
   BEGIN
    (*$I- *)
    readln(f,line) ;
    (*$I+ *)
    IF ioresult <> 0
     THEN line := eof_mark
    ELSE IF eof(f)
     THEN line := concat(line,eof_mark) ;
   END ; (* read_a_line *)

  BEGIN
   line := '' ;
   IF is_console(f)
    THEN read_kbd(line)
    ELSE read_a_line ;
   IF in_comment
    THEN
     IF pos('*)',line) > 0
      THEN
       BEGIN
        delete(line,1,pos('*)',line) + 1) ;
        in_comment := false ;
       END
      ELSE read_from_file(f) ;
   saved_line := line ;
  END ; (* read_from_file *)


 PROCEDURE get_token(VAR t_line : string132 ; VAR token : string80) ;
  (* Extract a token from t_line. Comments are ignored. A token is
     a string surrounded by delimiters or an end of line. Tokens may
     contain embedded spaces if they are surrounded by quote marks *)

  PROCEDURE get_word ;
   VAR
    done : boolean ;
    cn : integer ;
    len : byte ;
   BEGIN
    cn := 1 ;
    len := length(t_line) ;
    done := false ;
    WHILE NOT done DO
     IF cn > len
      THEN done := true
     ELSE IF t_line[cn] IN delim_set
      THEN done := true
     ELSE cn := cn + 1 ;
    token := copy(t_line,1,cn-1) ;
    delete(t_line,1,cn-1) ;
   END ; (* get_word *)

  PROCEDURE comment ;
   BEGIN
    IF pos('*)',t_line) > 0
     THEN
      BEGIN
       delete(t_line,1,pos('*)',t_line)+1) ;
       get_token(line,token) ;
      END
     ELSE
      BEGIN
       t_line := '' ;
       token := '' ;
       in_comment := true ;
      END ;
   END ; (* comment *)

  PROCEDURE get_quote ;
   BEGIN
    delete(t_line,1,1) ;
    IF pos(quote_char,t_line) > 0
     THEN
      BEGIN
       token := concat(quote_char,copy(t_line,1,pos(quote_char,t_line) - 1)) ;
       delete(t_line,1,pos(quote_char,t_line)) ;
      END
     ELSE
      BEGIN
       token := t_line ;
       t_line := '' ;
      END ;
   END ; (* get_quote *)

  BEGIN
   strip_leading_blanks(t_line) ;
   IF length(t_line) > 0
    THEN
     BEGIN
      IF copy(t_line,1,2) = '(*'
       THEN comment
      ELSE IF (copy(t_line,1,2) = ':-') OR (copy(t_line,1,2) = '?-')
       THEN
        BEGIN
         token := copy(t_line,1,2) ;
         delete(t_line,1,2) ;
        END
      ELSE IF t_line[1] = quote_char
       THEN get_quote
      ELSE IF t_line[1] IN delim_set
       THEN
        BEGIN
         token := t_line[1] ;
         delete(t_line,1,1) ;
        END
      ELSE get_word ;
     END
    ELSE token := '' ;
  END ; (* get_token *)


 PROCEDURE scan(VAR f : text_file ; VAR token : string80) ;
  (* Scan repeatedly calls get_token to retreive tokens. When the
     end of a line has been reached, read_from_file is called to
     get a new line. *)
  BEGIN
   IF length(line) > 0
    THEN
     BEGIN
      get_token(line,token) ;
      IF token = ''
       THEN scan(f,token) ;
     END
    ELSE
     BEGIN
      read_from_file(f) ;
      scan(f,token) ;
     END ;
  END ; (* scan *)


 PROCEDURE compile(VAR source : text_file) ;
  (* The recursive descent compiler. It reads tokens until the token
     'EXIT' is found. If the token is '?-', a query is performed, a '@' token
     is the command to read a new file and source statements are read form that
     file, otherwise the token is assumed to be part of a sentence and the rest
     of the sentence is parsed. *)

  PROCEDURE error(error_msg : string80) ;
   (* Signal an error. Prints saved_line to show where the error is located.
      saved_line contains the current line being parsed. it is required,
      because get_token chews up line as it reads tokens. *)

   PROCEDURE runout ;
    BEGIN
     WHILE (token <> '.') AND (token <> eof_mark)
      scan(source,token) ;
    END ; (* runout *)

   BEGIN
    error_flag := true ;
    writeln ;
    writeln(error_msg) ;
    writeln ;
    writeln(saved_line) ;
    writeln('' : length(saved_line) - length(line) - 1,'^') ; ;
    runout ;
    wait ;
   END ; (* error *)

  PROCEDURE goal(VAR l_ptr : node_ptr) ;
   (* Read a goal. The new goal is appended to l_ptr. Each goal is appended
      to l_ptr as a list. Thus, the sentence 'likes(john,X) :- likes(X,wine) .'
      becomes the list ( (likes john X) (likes X wine) ) *)
   VAR
    goal_token : string80 ;

   PROCEDURE functor(VAR f_ptr : node_ptr ; func_token : string80) ;
    (* The current goal is a functor. This routine allocates a node
       to store the functor and then processes the components of the
       functor. On exit, f_ptr points to the list containing the functor
       and its components. func_token contains the functor name. *)
    VAR
     c_ptr : node_ptr ;

    PROCEDURE components(VAR cm_ptr : node_ptr) ;
     (* Process the components of the functor. The components are terms
        seperated by commas. On exit, cm_ptr points to the list of
        components. *)

     PROCEDURE term(VAR t_ptr : node_ptr) ;
      (* Process a single term. The new term is appended to t_ptr. *)
      VAR
       t_token : string80 ;

      PROCEDURE quoted_str(VAR q_ptr : node_ptr) ;
       (* Process a quote *)
       BEGIN
        q_ptr := append_list(q_ptr,cons(alloc_str(constant,
                                        copy(token,2,length(token) - 1)),
                                        NIL)) ;
        scan(source,token) ;
       END ; (* quoted_str *)

      PROCEDURE varbl(VAR v_ptr : node_ptr) ;
       (* The current token is a varaible, allocate a node and return
          a pointer to it. *)
       BEGIN
        v_ptr := append_list(v_ptr,cons(alloc_str(variable,token),NIL)) ;
        scan(source,token) ;
       END ; (* varbl *)

      PROCEDURE number(VAR n_ptr : node_ptr) ;
       (* Numbers are treated as string constants. This isn't particularly
          efficent, but it is easy. *)
       BEGIN
        n_ptr := append_list(n_ptr,cons(alloc_str(constant,token),NIL)) ;
        scan(source,token) ;
       END ; (* handle_number *)

      BEGIN
       IF token[1] IN ['A' .. 'Z','_']
        THEN varbl(t_ptr)
       ELSE IF token[1] = quote_char
        THEN quoted_str(t_ptr)
       ELSE IF is_number(token)
        THEN number(t_ptr)
       ELSE IF token[1] IN ['a' .. 'z']
        THEN
         BEGIN
          t_token := token ;
          scan(source,token) ;
          IF token = '('
           THEN functor(t_ptr,t_token)
           ELSE t_ptr := append_list(t_ptr,
                                     cons(alloc_str(constant,t_token),NIL)) ;
         END
       ELSE error('Illegal Symbol.') ;
      END ; (* term *)

     BEGIN
      term(cm_ptr) ;
      IF token = ','
       THEN
        BEGIN
         scan(source,token) ;
         components(cm_ptr) ;
        END ;
     END ; (* components *)

    BEGIN
     c_ptr := cons(alloc_str(func,func_token),NIL) ;
     scan(source,token) ;
     components(c_ptr) ;
     IF token = ')'
      THEN
       BEGIN
        f_ptr := append_list(f_ptr,cons(c_ptr,NIL)) ;
        scan(source,token) ;
       END
      ELSE error('Missing '')''.') ;
    END ; (* functor *)

   BEGIN
    IF token[1] IN ['a' .. 'z',quote_char]
     THEN
      BEGIN
       IF token[1] = quote_char
        THEN
         BEGIN
          l_ptr := append_list(l_ptr,
                               cons(cons(alloc_str(constant,
                               copy(token,2,length(token) - 1)),NIL),NIL)) ;
          scan(source,token) ;
         END
        ELSE
         BEGIN
          goal_token := token ;
          scan(source,token) ;
          IF token = '('
           THEN functor(l_ptr,goal_token)
           ELSE l_ptr := append_list(l_ptr,
                                     cons(cons(alloc_str(constant,goal_token),
                                               NIL),NIL)) ;
         END
      END
     ELSE error('A goal must begin with ''a .. z'' or be a quoted string.') ;
   END ; (* goal *)

  PROCEDURE tail_list(VAR t_ptr : node_ptr) ;
   (* Process the tail of a rule. Since the a query is syntactically identical
      to the tail of a rule, this routine is used to compile queries.
      On exit, t_ptr points to the list containing the tail. *)
   BEGIN
    goal(t_ptr) ;
    IF token = ','
     THEN
      BEGIN
       scan(source,token) ;
       tail_list(t_ptr) ;
      END ;
   END ; (* tail *)

  PROCEDURE rule ;
   (* Procees a rule, actually any sentence. If no error occurs the
      new sentence is appended to the data base. *)
   VAR
    r_ptr : node_ptr ;

   PROCEDURE head_list(VAR h_ptr : node_ptr) ;
    BEGIN
     goal(h_ptr) ;
    END ; (* head *)

   BEGIN
    saved_list := cons(data_base,NIL) ;
    test_memory ;
    r_ptr := NIL ;
    head_list(r_ptr) ;
    IF token = ':-'
     THEN
      BEGIN
       scan(source,token) ;
       tail_list(r_ptr) ;
      END ;
    IF token <> '.'
     THEN error('''.'' expected.') ;
    IF NOT error_flag
     THEN data_base := append_list(data_base,cons(r_ptr,NIL)) ;
   END ; (* rule *)

  PROCEDURE query ;
   (* Process a query. Compile the query, and then call solve to search the
      data base. q_ptr points to the compiled query and solved is a boolean
      indicating whether the query was successfully solved. *)
   VAR
    q_ptr : node_ptr ;
    solved : boolean ;

   PROCEDURE solve(list,env : node_ptr ; level : counter) ;
    (* This is where all the hard work is done. This routine follows the
       steps outlined in the article. list is the query to be soved, env is
       the current environment and level is the recursion level. level can
       only get to 32767, but you'll run out of stack space long before you
       get that far.
       solve saves list and env on the saved list so that they won't be
       destroyed by garbage collection. The data base is always on the
       saved list. At the end of solve, list and env are removed from
       saved_list. *)
    VAR
     new_env,p : node_ptr ;

    FUNCTION look_up(var_str : string80 ; environ : node_ptr) : node_ptr ;
     (* Search the environment list pointed to by environ for the variable,
        var_str. If found return a pointer to var_str's binding, otherwise
        return NIL *)
     VAR
      found : boolean ;
      p : node_ptr ;
     BEGIN
      p := environ ;
      found := false ;
      WHILE (p <> NIL) AND (NOT found) DO
       BEGIN
        IF var_str = string_val(head(head(p)))
         THEN
          BEGIN
           found := true ;
           look_up := tail(head(p)) ;
          END
         ELSE p := tail(p) ;
       END ;
      IF NOT found
       THEN look_up := NIL ;
     END ; (* look_up *)

    PROCEDURE check_continue ;
     (* Print the bindings and see if the user is satisfied. If nothing
        is printed from the environment, then print 'Yes' to indicate
        that the query was successfully satisfied. *)
     VAR
      printed : boolean ;
      ch : char ;

     PROCEDURE print_bindings(list : node_ptr) ;
      (* Print the bindings for level 0 variables only, intermediate variables
         aren't of interest. The routine recursivley searches for the
         end of the environments list and then prints the binding. This
         is so that variables bound first are printed first. *)

      PROCEDURE print_functor(l : node_ptr) ; FORWARD ;

      PROCEDURE print_variable(var_str : string80) ;
       (* The varaible in question was bound to another varaible, so look
          up that variable's binding and print it. If a match can't be found
          print '_' to tell the user that the variable is anonymous. *)
       VAR
        var_ptr : node_ptr ;
       BEGIN
        var_ptr := look_up(var_str,env) ;
        IF var_ptr <> NIL
         THEN
          CASE tag_value(head(var_ptr)) OF
           constant  : write(string_val(head(var_ptr)),' ') ;
           variable  : print_variable(string_val(head(var_ptr))) ;
           cons_node : print_functor(head(var_ptr)) ;
          END
         ELSE write('_ ') ;
       END ; (* print_variable *)

      PROCEDURE print_functor (* l : node_ptr *) ;
       (* The variable was bound to a functor. Print the functor and its
          components. *)

       PROCEDURE print_components(p : node_ptr) ;
        (* Print the components of a functor. These may be variables or
           other functors, so call the approriate routines to print them. *)
        BEGIN
         IF p <> NIL
          THEN
           BEGIN
            CASE tag_value(head(p)) OF
             constant  : write(string_val(head(p)),' ') ;
             variable  : print_variable(string_val(head(p))) ;
             cons_node : print_functor(head(p)) ;
            END ;
            IF tail(p) <> NIL
             THEN
              BEGIN
               write(',') ;
               print_components(tail(p)) ;
              END ;
           END ;
        END ; (* print_components *)

       BEGIN
        IF l <> NIL
         THEN
          BEGIN
           write(string_val(head(l))) ;
           IF tail(l) <> NIL
            THEN
             BEGIN
              write('(') ;
              print_components(tail(l)) ;
              write(')') ;
             END ;
          END ;
       END ; (* print_functor *)

      BEGIN
       IF list <> NIL
        THEN
         BEGIN
          print_bindings(tail(list)) ;
          IF pos('#',string_val(head(head(list)))) = 0
           THEN
            BEGIN
             printed := true ;
             writeln ;
             write(string_val(head(head(list))),' = ') ;
             CASE tag_value(head(tail(head(list)))) OF
              constant  : write(string_val(head(tail(head(list)))),' ') ;
              variable  : print_variable(string_val(head(tail(head(list))))) ;
              cons_node : print_functor(head(tail(head(list)))) ;
             END ;
            END ;
         END ;
      END ; (* print_bindings *)

     BEGIN
      printed := false ;
      print_bindings(env) ;
      IF NOT printed
       THEN
        BEGIN
         writeln ;
         write('Yes ') ;
        END ;
      REPEAT
       read(kbd,ch) ;
      UNTIL ch IN [return,';'] ;
      solved := (ch = return) ;
      writeln ;
     END ; (* check_continue *)

    FUNCTION copy_list(list : node_ptr ; copy_level : counter) : node_ptr ;
     (* Copy a list and append the copy_level (recursion level) to all
        variables. *)
     VAR
      temp_list,p : node_ptr ;
      level_str : string[6] ;

     PROCEDURE list_copy(from_list : node_ptr ; VAR to_list : node_ptr) ;
      BEGIN
       IF from_list <> NIL
        THEN
         CASE from_list^.tag OF
          variable : to_list := alloc_str(variable,
                                          concat(from_list^.string_data,
                                                 level_str)) ;
          func,
          constant  : to_list := from_list ;
          cons_node : BEGIN
                       list_copy(tail(from_list),to_list) ;
                       to_list := cons(copy_list(head(from_list),copy_level),
                                       to_list) ;
                      END ;
         END ;
      END ; (* list_copy *)

     BEGIN
      str(copy_level,level_str) ;
      level_str := concat('#',level_str) ;
      temp_list := NIL ;
      list_copy(list,temp_list) ;
      copy_list := temp_list ;
     END ; (* copy_list *)

    FUNCTION unify(list1,list2,environ : node_ptr ; VAR new_environ : node_ptr) :
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
     VAR
      var_ptr : node_ptr ;

     PROCEDURE make_binding(l1,l2 : node_ptr) ;
      (* Bind a variable to the environment. Anonymous variables are not bound.
         l1 points to the variable and l2 points to its binding. *)
      BEGIN
       IF copy(string_val(head(l1)),1,1) <> '_'
        THEN new_environ := cons(cons(head(l1),l2),environ)
        ELSE new_environ := environ ;
       unify := true ;
      END ; (* make_binding *)

     PROCEDURE fail ;
      (* Unification failed. *)
      BEGIN
       unify := false ;
       new_environ := environ ;
      END ; (* fail *)

     PROCEDURE unify_constant ;
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
      BEGIN
       CASE tag_value(head(list2)) OF
        constant  : IF string_val(head(list1)) = string_val(head(list2))
                     THEN
                      BEGIN
                       unify := true ;
                       new_environ := environ ;
                      END
                     ELSE fail ;
        variable  : BEGIN
                     var_ptr := look_up(string_val(head(list2)),environ) ;
                     IF var_ptr = NIL
                      THEN make_binding(list2,list1)
                      ELSE unify := unify(list1,var_ptr,environ,new_environ) ;
                    END ;
        cons_node,
        func      : fail ;
       END ;
      END ; (* unify_constant *)

     PROCEDURE unify_func ;
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

      PROCEDURE unify_tail ;
       (* This routine does the term by term unification of the component
          lists *)
       VAR
        p,q : node_ptr ;
        unified : boolean ;
       BEGIN
        p := tail(list1) ;
        q := tail(list2) ;
        unified := true ;
        new_environ := environ ;
        WHILE (p <> NIL) AND unified DO
         BEGIN
          unified := unified AND unify(cons(head(p),NIL),cons(head(q),NIL),
                                       new_environ,new_environ) ;
          p := tail(p) ;
          q := tail(q) ;
         END ;
        IF NOT unified
         THEN fail ;
       END ; (* unify_tail *)

      BEGIN
       CASE tag_value(head(list2)) OF
        constant  : fail ;
        variable  : BEGIN
                     var_ptr := look_up(string_val(head(list2)),environ) ;
                     IF var_ptr = NIL
                      THEN make_binding(list2,list1)
                      ELSE unify := unify(list1,var_ptr,environ,new_environ) ;
                    END ;
        func      : IF string_val(head(list1)) = string_val(head(list2))
                     THEN
                      IF list_length(tail(list1)) = list_length(tail(list2))
                       THEN unify_tail
                       ELSE fail
                     ELSE fail ;
        cons_node : fail ;
       END ;
      END ; (* unify_func *)

     PROCEDURE unify_expr ;
      (* List1 contains an expression. Try to unify it with list2. The 4 cases
         are:
          list2 contains
           constant  - can't be unified.
           variable  - look up binding, if no current binding bind the
                       functor to the variable, otherwise unify list1
                       with the binding.
           cons_node - If the heads can be unified, the unify the tails.
           func      - fail *)
      BEGIN
       CASE tag_value(head(list2)) OF
         constant  : fail ;
         variable  : BEGIN
                      var_ptr := look_up(string_val(head(list2)),environ) ;
                      IF var_ptr = NIL
                       THEN make_binding(list2,list1)
                       ELSE unify := unify(list1,var_ptr,environ,new_environ) ;
                     END ;
         func      : fail ;
         cons_node : IF unify(head(list1),head(list2),environ,new_environ)
                      THEN unify := unify(tail(list1),tail(list2),new_environ,
                                          new_environ)
                      ELSE fail ;
        END ;
      END ; (* unify_expr *)

     BEGIN
      IF (list1 = NIL) AND (list2 = NIL)
       THEN
        BEGIN
         unify := true ;
         new_environ := environ ;
        END
      ELSE IF list1 = NIL
       THEN fail
      ELSE IF list2 = NIL
       THEN fail
      ELSE
       CASE tag_value(head(list1)) OF
        constant  : unify_constant ;
        variable  : BEGIN
                     var_ptr := look_up(string_val(head(list1)),environ) ;
                     IF var_ptr = NIL
                      THEN make_binding(list1,list2)
                      ELSE unify := unify(var_ptr,list2,environ,new_environ) ;
                    END ;
        func      : unify_func ;
        cons_node : unify_expr ;
       END ;
     END ; (* unify *)

    BEGIN
     saved_list := cons(list,cons(env,saved_list)) ;
     IF list = NIL
      THEN check_continue
      ELSE
       BEGIN
        p := data_base ;
        WHILE (p <> NIL) AND (NOT solved) DO
         BEGIN
          test_memory ;
          IF unify(copy_list(head(head(p)),level),head(list),env,new_env)
           THEN solve(append_list(copy_list(tail(head(p)),level),tail(list)),
                      new_env,level + 1) ;
          p := tail(p) ;
         END ;
       END ;
     saved_list := tail(tail(saved_list)) ;
    END ; (* solve *)

   BEGIN
    q_ptr := NIL ;
    tail_list(q_ptr) ;
    IF token <> '.'
     THEN error('''.'' expected.')
    ELSE IF NOT error_flag
     THEN
      BEGIN
       solved := false ;
       saved_list := cons(data_base,NIL) ;
       solve(q_ptr,NIL,0) ;
       IF NOT solved
        THEN writeln('No') ;
      END ;
   END ; (* query *)

  PROCEDURE read_new_file ;
   (* Read source statements from a new file. When all done, close file
      and continue reading from the old file. Files may be nested, but you
      will run into trouble if you nest them deaper than 15 levels. This
      is Turbo's default for open files. *)
   VAR
    new_file : text_file ;
    old_line,old_save : string132 ;
    f_name : string80 ;
   BEGIN
    IF token[1] = quote_char
     THEN delete(token,1,1) ;
    IF pos('.',token) = 0
     THEN f_name := concat(token,'.PRO')
     ELSE f_name := token ;
    IF open(new_file,f_name)
     THEN
      BEGIN
       old_line := line ;
       old_save := saved_line ;
       line := '' ;
       compile(new_file) ;
       close(new_file) ;
       line := old_line ;
       saved_line := old_save ;
       scan(source,token) ;
       IF token <> '.'
        THEN error('''.'' expected.') ;
      END
     ELSE error(concat('Unable to open ',f_name)) ;
   END ; (* read_new_file *)

  PROCEDURE do_exit ;
   (* Exit the program. This really should be a built-in function and handled
      in solve, but this does the trick. *)
   BEGIN
    scan(source,token) ;
    IF token <> '.'
     THEN error('''.'' expected.')
     ELSE halt
   END ; (* do_exit *)

  BEGIN
   scan(source,token) ;
   WHILE token <> eof_mark DO
    BEGIN
     error_flag := false ;
     IF token = '?-'
      THEN
       BEGIN
        scan(source,token) ;
        query ;
       END
      ELSE IF token = '@'
       THEN
        BEGIN
         scan(source,token) ;
         read_new_file ;
        END
      ELSE IF toupper(token) = 'EXIT'
       THEN do_exit
      ELSE rule ;
    scan(source,token) ;
   END ;
  END ; (* compile *)


 PROCEDURE initialize ;
  (* Write a heading line and initialize the global variables *)
  BEGIN
   clrscr ;
   writeln ;
   writeln('Very Tiny Prolog - Version 1.0     [c] 1986 MicroExpert Systems') ;
   writeln ;
   in_comment := false ;
   delim_set := [' ',')','(',',','[',']',eof_mark,tab,quote_char,':',
                 '@','.','?'] ;
   text_chars := [' ' .. '~'] ;
   line := '' ;
   data_base := NIL ;
   free := NIL ;
   saved_list := NIL ;
   total_free := 0.0 ;
   initial_heap := HeapPtr ;
  END ; (* initialize *)


 BEGIN
  initialize ;
  compile(kbd) ;
 END.
                