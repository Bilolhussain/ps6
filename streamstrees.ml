(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                      Section 2: Lazy Evaluation
                             Spring 2018
 *)

(*======================================================================
Section 2.1: Series acceleration with infinite streams

In nativeLazyStreams.ml, we provide the definitions of lazy streams
using OCaml's native Lazy module as presented in lecture, up to and
including code for approxiating pi through partial sums of the terms
in a Taylor series. In the next problem, you'll use streams to find
faster approximations for pi.

Recall from lecture the use of streams to generate approximations
of pi of whatever accuracy. Try it. You should be able to reproduce
the following:

   # within 0.01 pi_sums ;;
   - : int * float = (199, 3.13659268483881615)
   # within 0.001 pi_sums ;;
   - : int * float = (1999, 3.14109265362104129)
   # within 0.0001 pi_sums ;;
   - : int * float = (19999, 3.14154265358982476)

Notice that it takes about 2000 terms in the Taylor series to get
within .001 of the value of pi.  This method converges quite
slowly. But we can increase the speed dramatically by averaging
adjacent elements in the approximation stream.
......................................................................*)

open NativeLazyStreams ;;

(*......................................................................
Problem 5: Implementing average on streams

Write a function average that takes a float stream and returns a
stream of floats each of which is the average of adjacent values in
the input stream. For example:

# first 5 (average (to_float nats)) ;;
- : float list = [0.5; 1.5; 2.5; 3.5; 4.5]
......................................................................*)
  
let average (s : float stream) : float stream =
  match s with 
  | Cons (hd, Cons(hd2, tl) -> lazy Cons (((hd +. hd2) /. 2.), average tl)

(* Now instead of using the stream of approximations in pi_sums, you
can instead use the stream of averaged pi_sums, which converges much
more quickly. Test that it requires far fewer steps to get within,
say, 0.001 of pi. You'll want to record your results below for Problem
7. *)
   
(*......................................................................
Problem 6: Implementing Aitken's method

An even better accelerator of convergence for series of this sort
is Aitken's method. The formula is given in the problem set
writeup. Write a function to apply this accelerator to a stream, and
use it to generate approximations of pi.
......................................................................*)
   
let aitken (s: float stream) : float stream =
  match s with
  |Nil -> ()
  |Cons (hd, tl) ->  let s1 = head s in
                     let s2 = head (tail s) in 
                     let s3 = head (tail (tail s)) in
                     let rtn_stream x x1 x2 x3 : float stream =
                        x1 -. ((x2 -. x3) ** 2.) /. (x2 -. 2. * x3 +. x3)
                        in 
                        y = rtn_stream s s1 s2 s3 in
                        lazy (Cons (y, aitken tl)));;

(*......................................................................
Problem 7: Testing the acceleration

Fill out the following table, recording how many steps are needed to
get within different epsilons of pi.

    ---------------------------------------------------------
    epsilon  |  pi_sums  |  averaged method  |  aitken method 
    ---------------------------------------------------------
    0.1      |           |                   |
    ---------------------------------------------------------
    0.01     |           |                   |
    ---------------------------------------------------------
    0.001    |           |                   |
    ---------------------------------------------------------
    0.0001   |           |                   |
    ---------------------------------------------------------
......................................................................*)

(*======================================================================
Section 2.2 : Infinite trees

Just as streams are a lazy form of list, we can have a lazy form of
trees. In the definition below, each node in a lazy tree of type 'a
tree holds a value of some type 'a, and a (conventional, finite) list
of one or more (lazy) child trees. Complete the implementation by
writing print_depth, tmap, tmap2, and bfenumerate.  We recommend
implementing them in that order.
......................................................................*)
   
type 'a treeval = Node of 'a * 'a tree list
 and 'a tree = 'a treeval Lazy.t ;;

(* Infinite trees shouldn't have zero children. This exception is
available to raise in case that eventuality comes up. *)

exception Finite_tree ;;

(*......................................................................
Problem 8: Implementing infinite trees
*)

(*......................................................................
node t -- Returns the element of type 'a stored at the root node of
tree t of type 'a tree.
......................................................................*)


let node (t : 'a tree) : 'a =
   match t with 
  | Nil -> Raise Exception Finite_tree;
  | Node (a, _) -> Node a;;

(*......................................................................
children t -- Returns the list of children of the root node of tree t.
......................................................................*)
let children (t : 'a tree) : 'a tree list =
  match t with
  | Node (a', ls = [contents = {l_t == Nil, r_t == Nil}] -> Raise Exception Finite_tree
  | Node (a, ls : 'a tree list = [l_t, r_t]) -> ls ;;

(*......................................................................
print_depth n indent t -- Prints a representation of the first n
levels of the tree t indented indent spaces. You can see some examples
of the intended output of print_depth below.
......................................................................*)
let rec print_depth (n : int) (indent : int) (t : int tree) : unit =
  let rec helper (n: int) (indent: int) (t: int tree) : unit  = 
    let output = node t in 
    let spaces = !indent in 
    let no_spaces := ref 0 in 
    let sp = " " in
    let printf = print ("%s" sp) in 
    let printl = print ("%a\n%..." output) in 
    match children t with
    | ls = [lc == Nil, rc == Nil]  -> print ("%a\n%..." output)
    | ls = [lc, rc] -> 
        let s = "" in 
        let rec loop spaces str = 
          if (!spaces !== 0) then 
            loop (spaces-1) (s^print(lazy printf)) 
          else if (!space == 0) then
            lazy printl 
        in
        loop indent s 
  in 
  print (helper n indent lc, helper n indent rc);;
                

(*......................................................................
tmap f t -- Returns a tree obtained by mapping the function f over
each node in t.
......................................................................*)
let rec tmap (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match t with
  | Node (r, ls = []) -> f r
  | Node (r', ls = [l_c = {Node (_, _)}, r_c = {Node (_, _)}]) -> Node (f r', lazy (tmap f l_c::tmap f r_c))
  ;;

(*......................................................................
tmap2 f t1 t2 -- Returns the tree obtained by applying the function f
to corresponding nodes in t1 and t2, which must have the same
"shape". If they don't an Invalid_argument exception is raised.
......................................................................*)
let rec tmap2 (f : 'a -> 'b -> 'c)
              (t1 : 'a tree) (t2 : 'b tree)
            : 'c tree =
   match t1, t2 with
  | Node (r, ls = [t_lc! = [] || Null ;_), Node (r', ls = [t_lc' = []; _]) -> Invalid_argument
  | Node (r, ls = [_;  t_rc = []]), Node (r', ls = [_; t_rc' != []]) -> Invalid_argument
  | Node (r, ls = [t_lc != []; t_rc != []]), Node (r', ls' = [t_lc'!= [], t_rc' != []]) 
    ->
      Node (Node ((f r) (f r'), lazy (f tl_c)::lazy (f t_rc):: lazy (tmap2 f tl_c)::lazy (tmap2 f t_rc)))
         

(*......................................................................
bfenumerate tslist -- Returns a LazyStreams.stream of the nodes in the
list of trees tslist enumerated in breadth first order, that is, the
root nodes of each of the trees, then the level one nodes, and so
forth. There is an example of bfenumerate being applied below.
......................................................................
 *)
let rec bfenumerate (tslist : 'a tree list) : 'a stream =
  match tslist with 
  | Node (r, ls = (fst == Nil, tl == Nil) ) -> lazy Cons (r, [])
  | Node (r, ls = (fst !== Nil, tl == Nil) ) -> lazy Cons (r, lazy (Cons((bfenumerate fst), Nil)))
  | Node (r, ls = (fst == Nil, tl != Nil) ) -> lazy Cons (r, lazy (Cons (Nil, (bfenumerate tl)))
  | Node (r, ls = [fst;tl]) -> lazy Cons (r, Cons (lazy (bfenumerate fst), lazy (bfenumerate tl)) ;;

(* Now use your implementation to generate some interesting infinite
trees.  Hint: Drawing a tree considering how the values change along
each branch will yield helpful intuition for the next problems. *)

(*......................................................................
onest -- An infinite binary tree all of whose nodes hold the integer 1.
......................................................................*)
let rec onest : int tree =
  Node (1, [onest, onest]);; ;;

(*......................................................................
levels n -- Returns an infinite binary tree where the value of each
node in the tree is its level or depth in the tree, starting with the
argument n. For example:

# print_depth 2 0 (levels 0) ;;
0...
 1...
  2...
  2...
 1...
  2...
  2...
- : unit = ()
......................................................................*)
let rec levels (n : int) : int tree =
  let rec loop ln_times n () =
      if (ln_times == 0) then 
        let ll = l := !l + 1 in 
        Node (n+1, lazy (loop l*l ll)))
      else 
        let ln_tm = ln_times := !ln_times - 1 in 
        Node (n, lazy [loop ln_tm l])
      in
  let l := !n in
  if (n==0) then Node (0, lazy [loop 1 1])
  else 
  lazy loop l*l l;

(*......................................................................
Define an infinite binary tree tree_nats where the value of each node in
the tree is consecutively numbered in breadth-first order starting
with 0. For example:

# print_depth 2 0 tree_nats ;;
0...
 1...
  3...
  4...
 2...
  5...
  6...
- : unit = ()
# first 10 (bfenumerate [tree_nats]) ;;
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
......................................................................*)
let rec tree_nats : int tree =
  let rec loop (t: int stream) () =
    let r = node t in
    match t with 
    | Node (r, children t)
    | Node (r, [lc;rc]) -> Node (0, (lazy (loop (mapt (fun x -> x * 2) lc )))::(lazy loop (mapt (fun x -> ((x * 2) + 1)) rc ))) in
  loop nats
                                                 
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = failwith "not provided" ;;
