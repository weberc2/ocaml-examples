type cmpop = Eq | Ne | Gt | Lt | Ge | Le;;

type comparison = { op : cmpop; left : string; right : string };;

type 'a nonemptylist = 'a * 'a list;;

type boolexpr =
    | Comparison of comparison
    | And of boolexpr nonemptylist
    | Or of boolexpr nonemptylist;;

let cmpeval cmp =
    match cmp.op with
    | Eq -> cmp.left =  cmp.right
    | Ne -> cmp.left != cmp.right
    | Gt -> cmp.left >  cmp.right
    | Lt -> cmp.left <  cmp.right
    | Ge -> cmp.left >= cmp.right
    | Le -> cmp.left <= cmp.right;;

let rec eval bexpr =
    match bexpr with
    | Comparison cmp -> cmpeval cmp
    | And and_ -> andeval and_
    | Or or_ -> oreval or_
and andeval and_ =
    match and_ with
    | (bexpr, []) -> eval bexpr
    | (bexpr, head :: tail) -> (eval bexpr) && (andeval (head, tail))
and oreval or_ =
    match or_ with
    | (bexpr, []) -> eval bexpr
    | (bexpr, head :: tail) -> (eval bexpr) && (oreval (head, tail));;

let expr = And (
    Comparison {op=Eq; left="a"; right="a"},
    [Comparison {op=Ne; left="x"; right="y"}]);;

print_endline (string_of_bool (eval expr))
