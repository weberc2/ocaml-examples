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


type field = { key : string; value : json }
and json =
    | Array of json list
    | Object of field list
    | Number of float
    | String of string
    | Null
    | True
    | False;;

let rec string_of_json value =
    match value with
    | Array arr -> string_of_json_arr arr
    | Object obj -> string_of_json_obj obj
    | Number num -> string_of_float num
    | String str -> "\"" ^ str ^ "\""
    | Null -> "null"
    | True -> "true"
    | False -> "false"
and string_of_json_arr arr =
    match arr with
    | [] -> "[]"
    | head :: tail -> "[" ^ (string_of_json head) ^ (string_of_json_arr_tail tail)
and string_of_json_arr_tail tail =
    match tail with
    | [] -> "]"
    | head :: tail -> ", " ^ (string_of_json head) ^ (string_of_json_arr_tail tail)
and string_of_json_obj obj =
    match obj with
    | [] -> "{}"
    | head :: tail -> "{" ^ (string_of_json_field head) ^ (string_of_json_obj_tail tail)
and string_of_json_obj_tail tail =
    match tail with
    | [] -> "}"
    | head :: tail -> ", " ^ (string_of_json_field head) ^ (string_of_json_obj_tail tail)
and string_of_json_field field = "\"" ^ field.key ^ "\": " ^ (string_of_json field.value);;



let expr = And (
    Comparison {op=Eq; left="a"; right="a"},
    [Comparison {op=Ne; left="x"; right="y"}]);;

let obj = Array [
    Object [
        {key="name"; value=String "bob"};
        {key="age"; value=Number 54.0};
    ];
    Object [
        {key="name"; value=String "joe"};
        {key="age"; value=Number 39.0};
    ]];;

print_endline (string_of_json obj);

print_endline (string_of_bool (eval expr))
