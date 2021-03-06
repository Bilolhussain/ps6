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
 let rt_cycle ls ls_cpy : unit = 
  let rec check (ls1: ('a mlist) ref) (ls2_ref: ('a mlist) ref) : bool =
    (match !ls1 with 
     |Nil -> None
     |Cons (_, tl) -> match !tl with
       |Nil -> None
       |Cons (hd, y_ls) -> 
          if ((y_ls !== Nil) && (!ls2_ref == y_ls)) then Some tl
          else (check (tl) (!y_ls)) in
  check ls_cpy (ref ls);;
  
  
  let has_cycle (ls: 'a mlist) : bool =  
  match ls with 
  | Nil -> false
  | Cons (_, _) -> if ((rt_cycle (ls) ([ref ls])) == None) then false else true

(*
let rec rtn_cycle (lst) (ls_copy) = 
  match lst with 
  | Nil -> None
  | Cons (_, tl) ->
      match !tl with 
      | Nil -> None
      | Cons (_, y_ls) -> 
          let rec find_cycle (y_ref) (ls_cpy) =
            if List.exists (fun x -> x == y_ref) ls_cpy then Some tl
            else (rtn_cycle (!tl) (tl::ls_cpy)) 
          in find_cycle y_ls ls_copy;;

  
  
 let rec find_cycle mls : unit =
        match !mls with 
        | Nil -> None
        | Cons (_, tl) ->
            (match !tl with 
            | Nil -> None
            | Cons (_, xd) -> if (xd==Nil) then None
            | Cons (_, y_ls) -> 
              (* return tail with cycle if referencing same node*)
               if (y_ls!== Nil && !mls == y_ls) 
                then Some tl
               else 
                (find_cycle tl)

let rec has_cycle (ls: 'a mlist) : bool =  
    match ls with 
    | Nil -> false
    | Cons (_, rt) -> 
        if ((find_cycle !ls) == None) then false else true
*)

(*......................................................................
Problem 2: Write a function flatten that flattens a list (removes its
cycles if it has any) destructively. Again, you may want a recursive
helper function and you shouldn't worry about space.
......................................................................*)

let flatten (lst: 'a mlist)  : unit =
  match lst with 
  | Nil -> ()
  | Cons (_, tail) -> if (has_cycle (lst)) then 
        match (rt_cycle (ref lst)) with
        | None -> ()
        | Some t -> t:= Nil
        
(*        
let flatten (lst: 'a mlist)  : unit =
    match lst with 
    | Nil -> false
    | Cons (_, tail) -> 
        if (has_cycle tail) then 
          match find_cycle !lst with
          | None -> ()
          | Some t -> t:= Nil
 *)
(*......................................................................
Problem 3: Write mlength, which nondestructively finds the number of
nodes in a mutable list that may have cycles.
......................................................................*)
let rec helper_length mlst n  = 
    match mlst with 
    | Nil -> n
    | Cons (_, tail) -> 
        match !tail with
        | Nil -> n+1
        | Cons (_, rest) -> 
            match !rest with
            | Nil -> n+2
            | Cons (_, rest') -> 
                    if (!rest' == Nil) then n+2 else
                    helper_length rest n+2;;
                    
let m_length (lst: 'a mlist) : int =
  if (has_cycle lst) then flatten lst in 
    match lst with
    | Nil -> 0
    | Cons (_, tl) -> helper_length tl 0;;
                         
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = failwith "not provided" ;;
