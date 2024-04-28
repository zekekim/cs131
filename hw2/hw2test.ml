let accept_all string = Some string

type arithmetic_nonterminals =
  | Expr | Term | Factor | Number



let arithmetic_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; T "+"; N Expr];
          [N Term]]
     | Term ->
         [[N Factor; T "*"; N Term];
          [N Factor]]
     | Factor ->
         [[T "("; N Expr; T ")"];
          [N Number]]
     | Number ->
         (List.map (fun n -> [T (string_of_int n)]) [0;1;2;3;4;5;6;7;8;9]))

let make_matcher_t = ((make_matcher arithmetic_grammar accept_all ["3"; "+"; "4"; "*"; "2"; "+"; "5"]))



let make_matcher_test =
  ((make_matcher arithmetic_grammar accept_all ["3"; "+"; "4"; "*"; "2"; "+"; "5"])
   = Some [])

let make_parser_test =
  let input = ["3"; "+"; "4"; "*"; "2"]
  in
  match make_parser arithmetic_grammar input with
    | Some tree -> parse_tree_leaves tree = input
    | _ -> false


