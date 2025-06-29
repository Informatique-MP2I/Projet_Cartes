(* Definition of node record *)
type node = {
  uid       : int;
  longitude : float;
  latitude  : float;
  nature    : string
}

(* Accessors *)
let get_uid node =
  node.uid

let get_longitude node =
  node.longitude

let get_latitude node =
  node.latitude

let get_nature node =
  node.nature

(* Read all lines of a node file and print them on stdout *)
let read_node_file city =
  let filename = "city_maps/" ^ city ^ "_nodes.txt" in
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

(* Converts a line of a <city_name>\_nodes.txt file on a node record *)
let node_of_line line =
  Scanf.sscanf line "{ %d, %f, %f, %S }" (fun uid longitude latitude nature -> {uid; longitude; latitude; nature})

(* Prints a node *)
let print_node node =
  Printf.printf "#%d = longitude : %f, latitude : %f, nature : %s\n" (get_uid node) (get_longitude node) (get_latitude node) (get_nature node)

(* Prints a node list *)
let print_node_list node_list =
  List.iter (fun node -> print_node node) node_list

(* Creates and returns the node list of the given city *)
let load_node_list city =
  let filename = "city_maps/" ^ city ^ "_nodes.txt" in
  try 
    let channel = open_in filename in
    let rec aux node_list = 
    try
        let line = input_line channel in
        let node = node_of_line line in
        aux (node::node_list)
    with
    | End_of_file -> node_list
    in
    let node_list = aux [] in 
    close_in channel;
    List.rev node_list
  with
  | Sys_error err -> Printf.printf "Error : %s\n" err; []

(* Finds the min longitude in a node list *)
let get_min_longitude node_list =
  let rec aux node_list min = 
    match node_list with
    | [] -> min
    | h::t -> if (get_longitude h) < min then aux t (get_longitude h) else aux t min
  in
  match node_list with
  | [] -> raise (Invalid_argument "Empty list")
  | h::t -> aux node_list (get_longitude h)

(* Finds the max longitude in a node list *)
let get_max_longitude node_list =
  let rec aux node_list max = 
    match node_list with
    | [] -> max
    | h::t -> if (get_longitude h) > max then aux t (get_longitude h) else aux t max
  in
  match node_list with
  | [] -> raise (Invalid_argument "Empty list")
  | h::t -> aux node_list (get_longitude h)
        
(* Finds the min latitude in a node list *)
let get_min_latitude node_list =
  let rec aux node_list min = 
    match node_list with
    | [] -> min
    | h::t -> if (get_latitude h) < min then aux t (get_latitude h) else aux t min
  in
  match node_list with
  | [] -> raise (Invalid_argument "Empty list")
  | h::t -> aux node_list (get_latitude h)

(* Finds the max longitude in a node list *)
let get_max_latitude node_list =
  let rec aux node_list max = 
    match node_list with
    | [] -> max
    | h::t -> if (get_latitude h) > max then aux t (get_latitude h) else aux t max
  in
  match node_list with
  | [] -> raise (Invalid_argument "Empty list")
  | h::t -> aux node_list (get_latitude h)

(* Returns a node from a node list via its uid *)
let find_node node_uid node_list =
  let found_node = List.filter (fun node -> (get_uid node) = node_uid) node_list in
  match found_node with
  | [] -> raise  (Invalid_argument "Node not found")
  | [node] -> node 
  | _ -> raise (Invalid_argument "Redundant node")
  
