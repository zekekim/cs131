let rec is_member x lst =
    match lst with
    | [] -> false
    | hd::tl -> x = hd || is_member x tl

let rec subset a b =
    match a with
    | [] -> true
    | hd::tl -> is_member hd b && subset tl b

let rec equal_sets a b = subset a b && subset b a

let rec set_union a b =
    match a with
    | [] -> b
    | hd::tl -> if is_member hd b then set_union tl b else set_union tl (hd::b)

let set_all_union a =
    List.fold_left set_union [] a
(*
It is not possible to write a function `self_member` in OCaml that determines
if a set is a member of itself because OCaml's type system prevents the creation
of such self-referential structures to avoid paradoxes like Russell's Paradox.

In set theory, Russell's Paradox shows the impossibility of a set containing
all sets that do not contain themselves, as it leads to a logical contradiction.
Similarly, in programming languages with strict type systems like OCaml, a set
(or any type) containing itself would violate the rules of the type system,
leading to undefined or contradictory behavior.

Therefore, any attempt to define `self_member` would fail to compile due to the
inability to construct a self-containing set within OCaml's type system.
*)

let rec computed_fixed_point eq f x =
    let next = f x in
    if eq x next then x
    else computed_fixed_point eq f next

let rec apply_p_times f p x =
    if p <= 0 then x
    else apply_p_times f (p-1) (f x)

let rec computed_periodic_point eq f p x =
    let next = apply_p_times f p x in
    if eq x next then x
    else computed_periodic_point eq f p (f x)

let rec whileseq s p x =
  if p x
  then x :: whileseq s p (s x)
  else []

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let check_terminable terminable_symbols symbol=
    match symbol with
    | T _ -> true
    | N sym -> (is_member sym terminable_symbols)

let check_terminable_rhs terminable_symbols rhs  =
    List.for_all (check_terminable terminable_symbols) rhs

let rec check_terminable_rules rules terminable_symbols =
    match rules with
    | (lhs,rhs)::t -> if (check_terminable_rhs terminable_symbols rhs) then
        (check_terminable_rules t (lhs::terminable_symbols))
    else
        (check_terminable_rules t (terminable_symbols))
    | _ -> terminable_symbols

let get_terminable_symbols rules =
    (computed_fixed_point equal_sets (check_terminable_rules rules) [])

let rec remove_rules terminable_symbols acc rules =
    match rules with
    | (lhs, rhs)::t ->
        if check_terminable_rhs terminable_symbols rhs then
            remove_rules terminable_symbols ((lhs, rhs) :: acc) t
        else remove_rules terminable_symbols acc t
    | [] -> (List.rev acc)

let filter_blind_alleys grammar =
    match grammar with
    (start, rules) -> (start,(remove_rules (get_terminable_symbols rules) [] rules))


