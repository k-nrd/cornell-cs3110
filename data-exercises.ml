(* List comprehensions *)
let a = [ 1; 2; 3; 4; 5 ]
let b = [ 1; 2; 3; 4; 5 ]
let c = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* Product *)
let rec prod = function
  | [] -> 1
  | x :: xs -> x * prod xs
;;

(* Concat *)
let rec concat = function
  | [] -> ""
  | s :: ss -> s ^ concat ss
;;

(* Patterns *)
let pm1 = function
  | "bigred" :: _ -> true
  | _ -> false
;;

let pm2 = function
  | [ a; b ] -> true
  | [ a; b; c; d ] -> true
  | _ -> false
;;

let pm3 = function
  | a :: b :: xs -> a = b
  | _ -> false
;;

(* List library *)
let get_fifth lst =
  match List.nth lst 4 with
  | a -> a
  | exception Failure _ -> 0
;;

let get_sorted = List.sort (fun a b -> b - a)

(* Library puzzle *)
let get_last lst = List.nth lst (List.length lst - 1)
let any_zeroes lst = List.mem 0 lst

(* Take drop *)
(* Already tail recursive lmao *)
let take n lst =
  let rec tk m l acc =
    match l with
    | [] -> acc
    | x :: xs -> if m = 0 then acc else tk (m - 1) xs (x :: acc)
  in
  List.rev (tk n lst [])
;;

let drop n lst =
  let rec dp m l acc =
    match l with
    | [] -> acc
    | x :: xs -> dp (m - 1) xs (if m > 0 then acc else x :: acc)
  in
  List.rev (dp n lst [])
;;

(* Unimodal *)
let is_unimodal = function
  | [] -> true
  | x :: xs ->
    let rec uni min = function
      | [] -> true
      | x :: xs -> x > min && uni x xs
    in
    uni x xs
;;

(* Powerset *)
let powerset lst =
  let rec cross elt set = function
    | [] -> [ elt ] :: set
    | x :: xs -> cross elt ((elt :: x) :: x :: set) xs
  in
  let rec ps set = function
    | [] -> [] :: set
    | x :: xs -> ps (cross x [] set) xs
  in
  ps [] lst
;;

(* Print int list *)
let rec print_int_list = function
  | [] -> ()
  | x :: xs ->
    print_int x;
    print_newline ();
    print_int_list xs
;;

let print_int_list' =
  List.iter (fun a ->
    print_int a;
    print_newline ())
;;

(* Student *)
type student =
  { first_name : string
  ; last_name : string
  ; gpa : float
  }

let alice = { first_name = "Alice"; last_name = "Bobson"; gpa = 4.0 }
let student_name { first_name; last_name } = first_name, last_name
let create_student first_name last_name gpa = { first_name; last_name; gpa }

(* Pokerecord *)
type poketype =
  | Normal
  | Fire
  | Water

type pokemon =
  { name : string
  ; hp : int
  ; ptype : poketype
  }

let charizard = { name = "Charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "Squirtle"; hp = 44; ptype = Water }

(* Safe hd and tl *)
let safe_hd = function
  | [] -> None
  | x :: xs -> Some x
;;

let safe_tl = function
  | [] -> None
  | x :: [] -> None
  | x :: xs -> Some xs
;;

(* Pokefun *)
(* Fails with no head but spec didnt say anything so whatever *)
let max_hp pokelst =
  let rec find_max cur = function
    | [] -> cur
    | x :: xs -> if x.hp > cur.hp then find_max x xs else find_max cur xs
  in
  find_max (List.hd pokelst) pokelst
;;

(* Date before *)
type date_triple = int * int * int

let is_before (d1, m1, y1) (d2, m2, y2) =
  if y1 = y2 then if m1 = m2 then d2 > d1 else m2 > m1 else y2 > y1
;;

(* Earliest date *)
let earliest_date = function
  | [] -> None
  | x :: xs ->
    let rec go cur = function
      | [] -> Some cur
      | x :: xs -> go (if is_before cur x then cur else x) xs
    in
    go x xs
;;

(* Assoc list *)
(* TODO *)

(* Cards *)
type suit =
  | Spades
  | Hearts
  | Clubs
  | Diamonds

type rank =
  | Ace
  | King
  | Queen
  | Jack
  | Number of int

type card =
  { suit : suit
  ; rank : rank
  }

let ace_of_clubs = { suit = Clubs; rank = Ace }
let queen_of_hearts = { suit = Hearts; rank = Queen }
let two_of_diamonds = { suit = Diamonds; rank = Number 2 }

(* Matching *)
(* TODO *)

(* Quadrant *)
type quad =
  | I
  | II
  | III
  | IV

type sign =
  | Neg
  | Zero
  | Pos

let sign_of = function
  | 0 -> Zero
  | a when a > 0 -> Pos
  | _ -> Neg
;;

let quadrant_of (x, y) =
  match sign_of x, sign_of y with
  | Pos, Pos -> Some I
  | Neg, Neg -> Some II
  | Neg, Pos -> Some III
  | Pos, Neg -> Some IV
  | _ -> None
;;

(* quadrant_when is inefficient so I won't do it*)

(* Depth *)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(*
   first we write a generic fold_tree function that accumulates some value
   while iterating through a tree
   we can run different things if we're on nodes of leaves
   we add indexes bc it's useful in many situations (including calculating depth)
*)
let fold_tree on_node on_leaf acc = function
  | Leaf -> on_leaf 0 acc
  | node ->
    let rec loop res = function
      | [] -> res
      | (i, n) :: ns ->
        (match n with
         | Leaf -> loop (on_leaf i res) ns
         | Node (v, l, r) ->
           let double_idx = 2 * i in
           let left_idx, right_idx = double_idx + 1, double_idx + 2 in
           loop (on_node (v, l, r) i res) ((left_idx, l) :: (right_idx, r) :: ns))
    in
    loop acc [ 0, node ]
;;

(* take advantage of the fact that we can tell height by index through log *)
let depth tree =
  let compute_height index highest_depth =
    let new_depth = index |> Int.add 1 |> Int.to_float |> Float.log2 |> Float.to_int in
    if new_depth > highest_depth then new_depth else highest_depth
  in
  let on_node _node = compute_height in
  let on_leaf = compute_height in
  tree |> fold_tree on_node on_leaf 0
;;

(* Shape *)
