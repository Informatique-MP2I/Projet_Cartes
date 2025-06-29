(* Abstract type of node record *)
type node

(* Accessors *)
val get_uid : node -> int

val get_longitude : node -> float

val get_latitude : node -> float

val get_nature : node -> string
    
(* Read all lines of a node file and print them on stdout *)
val read_node_file : string -> unit

(* Converts a line of a <city_name>\_nodes.txt file on a node record *)
val node_of_line : string -> node

(* Prints a node *)
val print_node : node -> unit                                

(* Prints a node list *)
val print_node_list : node list -> unit                            

(* Creates and returns the node list of the given city *)
val load_node_list : string -> node list

(* Finds the min longitude in a node list *)
val get_min_longitude : node list -> float

(* Finds the max longitude in a node list *)
val get_max_longitude : node list -> float

(* Finds the min latitude in a node list *)
val get_min_latitude : node list -> float

(* Finds the max longitude in a node list *)
val get_max_latitude : node list -> float

(* Returns a node from a node list via its uid *)
val find_node : int -> node list -> node
