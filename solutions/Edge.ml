(* Definition of edge record *)
type edge = {
  uid_start_node : int;
  uid_end_node   : int;
  name           : string;
  nature         : string;
  speed_limit    : int;
  one_way        : bool
}

(* Accessors *)
let get_uid_start_node edge =
  edge.uid_start_node

let get_uid_end_node edge =
  edge.uid_end_node

let get_name edge =
  edge.name

let get_nature edge =
  edge.nature

let get_speed_limit edge =
  edge.speed_limit

let get_one_way edge =
  edge.one_way

(* Read all lines of an edge file and print them on stdout *)
let read_edge_file city =
  let filename = "city_maps/" ^ city ^ "_edges.txt" in
  try 
    let channel = open_in filename in
    try
      while true do
        let line = input_line channel in
        Printf.printf "%s\n" line
      done
    with
    | End_of_file -> close_in channel
  with
  | Sys_error err -> Printf.printf "Error : %s\n" err

(* Converts a line of a <city_name>\_edges.txt file on an edge record *)
let edge_of_line line =
  Scanf.sscanf line "{ %d, %d, %S, %S, %d, %B }" (fun uid_start_node uid_end_node name nature speed_limit one_way -> {uid_start_node; uid_end_node; name; nature; speed_limit; one_way})

(* Prints an edge *)
let print_edge edge =
  Printf.printf "#%d -> #%d %s = %s, %d km/h, %B\n" (get_uid_start_node edge) (get_uid_end_node edge) (get_name edge) (get_nature edge) (get_speed_limit edge) (get_one_way edge)

(* Prints an edge list *)
let print_edge_list edge_list =
  List.iter (fun edge -> print_edge edge) edge_list

(* Creates and returns the node list of the given city *)
let load_edge_list city =
  let filename = "city_maps/" ^ city ^ "_edges.txt" in
  try 
    let channel = open_in filename in
    let rec aux edge_list = 
    try
        let line = input_line channel in
        let edge = edge_of_line line in
        aux (edge::edge_list)
    with
    | End_of_file -> edge_list
    in
    let edge_list = aux [] in 
    close_in channel;
    List.rev edge_list
  with
  | Sys_error err -> Printf.printf "Error : %s\n" err; []

(* Checks if the string str contains the substring sub *)
let str_contains str sub =
  let str = String.lowercase_ascii str in
  let sub = String.lowercase_ascii sub in
  let str_length = String.length str in
  let sub_length = String.length sub in
  if str_length = 0 || sub_length = 0 || str_length < sub_length then false
  else
    let rec check i =
      if i + sub_length > str_length then
        false
      else
      if String.sub str i sub_length = sub then
        true
      else
        check (i+1)
    in check 0

(* Returns the list of edges with a given name *)
let find_edges name edge_list =
  let lower_name = String.lowercase_ascii name in
  let rec aux edge_list new_list =
    match edge_list with
    | [] -> new_list
    | h::t -> if str_contains (String.lowercase_ascii (get_name h)) lower_name then
        h :: (aux t (h::new_list))
      else aux t new_list
  in aux edge_list []

