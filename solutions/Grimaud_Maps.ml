open Node
open Edge

let () =
  if Array.length Sys.argv = 2 then
    let city = Sys.argv.(1) in
    Printf.printf "The city name is : %s\n" city;
    print_node (List.hd (load_node_list city));
    print_edge (List.hd (load_edge_list city));
    let node_list = load_node_list city in
    let edge_list = load_edge_list city in
    let min_longitude = get_min_longitude node_list in
    let max_longitude = get_max_longitude node_list in
    let min_latitude = get_min_latitude node_list in
    let max_latitude = get_max_latitude node_list in
    Printf.printf "Longitudes (%f, %f), Latitudes (%f, %f)\n" min_longitude max_longitude min_latitude max_latitude;
    print_edge_list (find_edges "liberté" edge_list)
  else
    Printf.eprintf "Usage : %s <city>\n" Sys.argv.(0)
      
