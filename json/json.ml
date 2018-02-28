type field = { key : string; value : json }
and json =
    | Array of json list
    | Object of field list
    | Number of float
    | String of string
    | Null
    | True
    | False;;

let rec joinstr = function
    | [] -> ""
    | h :: [] -> h
    | h :: t -> h ^ ", " ^ (joinstr t);;

let rec string_of_json = function
    | Array arr -> "[" ^ joinstr (List.map string_of_json arr) ^ "]"
    | Object obj -> "{" ^ joinstr (List.map string_of_json_field obj) ^ "}"
    | Number num -> string_of_float num
    | String str -> "\"" ^ str ^ "\""
    | Null -> "null"
    | True -> "true"
    | False -> "false"
and string_of_json_field f = "\"" ^ f.key ^ "\": " ^ (string_of_json f.value);;

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
