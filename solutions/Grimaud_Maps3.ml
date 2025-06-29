open Node       
open Edge
open Graphics

(* Checks if the string str contains the substring sub *)
let str_contains str sub =
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

let draw_edge edge node_list x_scale y_scale min_longitude min_latitude name =
  let start_node = find_node (get_uid_start_node edge) node_list in
  let end_node = find_node (get_uid_end_node edge) node_list in
  let x_s = int_of_float (((get_longitude start_node) -. min_longitude) *. x_scale) in
  let y_s = int_of_float (((get_latitude start_node) -. min_latitude) *. y_scale) in
  let x_e = int_of_float (((get_longitude end_node) -. min_longitude) *. x_scale) in
  let y_e = int_of_float (((get_latitude end_node) -. min_latitude) *. y_scale) in
  begin
    if name <> "" && str_contains (get_name edge) name then
      set_color red
    else
      match get_nature edge with
      | "footway" -> set_color green
      | _ -> set_color blue
  end;
  moveto x_s y_s;
  lineto x_e y_e


(* Draws all the given edges *)
let draw_edges node_list edge_list x_size y_size name =
  let min_longitude = get_min_longitude node_list in
  let max_longitude = get_max_longitude node_list in
  let min_latitude = get_min_latitude node_list in
  let max_latitude = get_max_latitude node_list in
  let x_scale = (float_of_int x_size) /. (max_longitude -. min_longitude) in
  let y_scale = (float_of_int y_size) /. (max_latitude -. min_latitude) in
  List.iter (fun edge -> draw_edge edge node_list x_scale y_scale min_longitude min_latitude name) edge_list

(* Opens a map and draws all the edges *)
let open_map city node_list edge_list name =
  let x_size = 1000 in
  let y_size = 700 in
  open_graph " 1000x700";
  resize_window x_size y_size;
  set_window_title ("Carte de " ^ city);
  draw_edges node_list edge_list x_size y_size name;
  ignore (wait_next_event [Key_pressed]);
  close_graph ()

(* main *)
let () =
  let name =
    begin
      if Array.length Sys.argv = 3 then
        Sys.argv.(2)
      else
        ""
    end
  in
  if Array.length Sys.argv = 2 || Array.length Sys.argv = 3 then
    let city = Sys.argv.(1) in
    Printf.printf "The city name is : %s\n" city;
    let node_list = load_node_list city in
    let edge_list = load_edge_list city in
    open_map city node_list edge_list name
  else
    Printf.eprintf "Usage : %s <city>\n" Sys.argv.(0)
