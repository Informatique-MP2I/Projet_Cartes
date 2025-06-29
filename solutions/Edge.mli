(* Abstract type of edge record *)
type edge 

(* Accessors *)
val get_uid_start_node : edge -> int

val get_uid_end_node : edge -> int

val get_name : edge -> string

val get_nature : edge -> string

val get_speed_limit : edge -> int

val get_one_way : edge -> bool

(* Read all lines of an edge file and print them on stdout *)
val read_edge_file : string -> unit

(* Converts a line of a <city_name>\_edges.txt file on an edge record *)
val edge_of_line : string -> edge
  
(* Prints an edge *)
val print_edge : edge -> unit

(* Prints an edge list *)
val print_edge_list : edge list -> unit

(* Creates and returns the node list of the given city *)
val load_edge_list : string -> edge list

(* Returns the list of edges with a given name *)
val find_edges : string -> edge list -> edge list
