(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                 Section 1: Mutable Lists and Cycles
                             Spring 2018
 *)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref

(*......................................................................
Problem 1: Write a function has_cycle that returns whether a mutable
list has a cycle. You may want a recursive helper function. Don't
worry about space usage.
......................................................................*)
                                      
let has_cycle (ls: 'a mlist) : bool = 
  let rec check (ls1: 'a mlist) (ls2: 'a mlist) (ls2_ref: ('a mlist) ref) : bool =
    (match ls1 with 
     |Nil -> false
     |Cons (_, tl) -> if (!tl == Nil) then false else
           (match !tl with
            |Cons (_, y_ls) -> 
                if ((y_ls != Nil) && (ls2_ref == y_ls)) then true
                else check tl ls2 (!y_ls || has_cycle ls2)) in
  check ls ls (ref ls);;

(*......................................................................
Problem 2: Write a function flatten that flattens a list (removes its
cycles if it has any) destructively. Again, you may want a recursive
helper function and you shouldn't worry about space.
......................................................................*)
let flatten (lst : 'a mlist) : unit =
  failwith "flatten not implemented"

(*......................................................................
Problem 3: Write mlength, which nondestructively finds the number of
nodes in a mutable list that may have cycles.
......................................................................*)
let mlength (lst : 'a mlist) : int =
  failwith "mlength not implemented"
                         
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = failwith "not provided" ;;
