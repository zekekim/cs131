let subset_test0 = subset [] [5;4;3]
let subset_test1 = subset [5;4;3] [1]

let equal_sets_test0 = equal_sets [1;1;1] [1;1;1]
let equal_sets_test1 = equal_sets [1] [1;1]

let set_union_test0 = equal_sets (set_union [] [3;4;5;6]) [3;4;5;6]
let set_union_test1 = equal_sets (set_union [3;4;5] [3;5;6]) [3;4;5;6]

let set_all_union_test0 =
    equal_sets (set_all_union []) []
let set_all_union_test1 =
    equal_sets (set_all_union [[3;4;5]; [5;3;4]; [6;7;5]]) [3;4;5;6;7]

let computed_fixed_point_test0 =
    computed_fixed_point (=) (fun x -> x / 2) 10 = 0

let computed_periodic_point_test1 =
    computed_periodic_point (=) (fun x -> x * x) 2 2 = 16

let while_seq_test0 =
    equal_sets (while_seq (fun x -> x + 3) (fun x -> x < 10) 0) [0;3;6;9]
let while_seq_test1 =
    equal_sets (while_seq (fun x -> x * 2) (fun x -> x < 20) 1) [1;2;4;8;16]


type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]


let awksub_grammar = Expr, awksub_rules

let awksub_test0 =
  filter_blind_alleys awksub_grammar = awksub_grammar

let extended_rules =
  awksub_rules @
  [Expr, [N Expr; N Lvalue];
   Lvalue, [N Expr; N Binop]]

let test1_grammar = (Expr, extended_rules)

let expected_rules1 =
  List.filter (fun (lhs, rhs) -> not (List.mem lhs [Lvalue])) awksub_rules

let awksub_test1 =
  filter_blind_alleys test1_grammar = (Expr, expected_rules1)



