type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec convert_list rules acc nonterminal =
    match rules with
    | [] -> (List.rev acc)
    | hd::tl -> if (List.hd hd) = nonterminal
                    then convert_list tl ((List.tl hd)::acc) nonterminal
                    else convert_list tl acc nonterminal

let rec convert_grammar g =
    let symbol = (List.hd g) in
    let rules = (List.tl g) in
    let func = convert_list rules [] in
    (symbol, func)

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let rec parse_tree_leaves tree =
    let rec helper tr =
        match tr with
        | [] -> []
        | (Leaf l)::tl -> l::(helper tl)
        | (Node (_,b))::tl -> (helper b)@(helper tl)

    in
    match tree with
    | Leaf x -> [x]
    | Node (_,b) -> helper b

let rec or_matcher grammar rules accept frag =
  match rules with
  | [] -> None
  | first_rule::other_rules ->
      match (and_matcher grammar first_rule accept frag) with
      | None -> or_matcher grammar other_rules accept frag
      | Some suffix -> Some suffix

and and_matcher grammar rule accept frag =
  match rule with
  | [] -> accept frag
  | (T terminal)::rest_rule ->
      begin
        match frag with
        | [] -> None
        | head::tail ->
            if head = terminal then and_matcher grammar rest_rule accept tail
            else None
      end
  | (N nonterminal)::rest_rule ->
      let new_accept = and_matcher grammar rest_rule accept in
      let rules = snd grammar nonterminal in
      or_matcher grammar rules new_accept frag

let make_matcher grammar accept frag =
  let start_symbol = fst grammar in
  let rules = snd grammar start_symbol in
  or_matcher grammar rules accept frag

let tree_accept frag tree = match frag with
| [] -> Some tree
| _ -> None

let rec or_parser grammar symbol rules accept frag tree =
  match rules with
  | [] -> None
  | first_rule::other_rules ->
      let and_rules = (and_parser grammar symbol first_rule accept frag tree) in
      match and_rules with
      | None -> or_parser grammar symbol other_rules accept frag tree
      | Some suffix -> and_rules

and and_parser grammar symbol rule accept frag tree =
  match rule with
  | [] -> accept frag (Node(symbol, tree))
  | (T terminal)::rest_rule ->
      begin
        match frag with
        | [] -> None
        | head::tail ->
                if head = terminal then and_parser grammar symbol rest_rule accept tail (tree@[(Leaf terminal)])
            else None
      end
  | (N nonterminal)::rest_rule ->
          let new_accept new_frag new_tree = and_parser grammar symbol rest_rule accept new_frag (tree@[new_tree]) in
          or_parser grammar nonterminal (grammar nonterminal) new_accept frag []

let make_parser_helper gram =
    let start_symbol = fst gram in
    let gram_rules = snd gram in
        let func accept frag = or_parser gram_rules start_symbol (gram_rules start_symbol) accept frag [] in
        func

let make_parser gram = fun frag -> make_parser_helper gram tree_accept frag
