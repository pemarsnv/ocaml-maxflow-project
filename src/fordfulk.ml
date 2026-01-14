open Graph
open Tools
open List
open Gfile

type flow = {
    value: int;
    max: int;
}

let export_ford path g =
  let flow_to_str flow = string_of_int(flow.value)^"/"^string_of_int(flow.max)
  in let g2 = gmap g flow_to_str in export path g2 ;;
let get_residual_value x = x.lbl

let is_arc_augmenting x = x.lbl > 0

let rec depth g src tgt visited nodes = 
  if src = tgt then 
    List.rev nodes 
  else 
    let arcs = out_arcs g src in 
    let rec explore arcs =
      match arcs with
      | [] -> [] 
      | x :: rest -> (if ( List.mem x.tgt visited || not (is_arc_augmenting x)) then explore rest else
                      match depth g x.tgt tgt (x.tgt :: visited) (x.tgt :: nodes) with 
                     | [] -> explore rest
                     | list -> list)
    in explore arcs;; 

let get_flow_btw g src nodes =
  let first_value g src nodes = 
  match (find_arc g src (List.hd nodes)) with 
  | None -> raise Not_found
  | Some x -> x.lbl in
  let rec get_flow_btw_bis g src nodes value =
  match nodes with 
  | [] -> value
  | x :: rest -> let arc = find_arc g src x in (match arc with
                                              None -> raise Not_found
                                             | Some arc ->  get_flow_btw_bis g x rest (if value < (get_residual_value arc) then value else (get_residual_value arc))) in
  (get_flow_btw_bis g src nodes (first_value g src nodes))

let rec apply_path g src nodes value =
  match nodes with
  | [] -> g
  | x :: rest -> apply_path (add_arc (add_arc g x src value) src x (-value)) x rest value

let diff_graph g1 g2 = 
  e_fold g1 (fun boolean arc -> boolean && (match (find_arc g2 arc.src arc.tgt) with 
                                            | None -> false
                                            | Some x -> x.lbl = arc.lbl)) true
  && 
  e_fold g2 (fun boolean arc -> boolean && (match (find_arc g1 arc.src arc.tgt) with 
                                            | None -> false
                                            | Some x -> x.lbl = arc.lbl)) true

let iteration g src tgt =
  let path = depth g src tgt [] [] in
  match path with
  | [] -> g                      (* arrêt : plus de chemin *)
  | _  ->
      let flow = get_flow_btw g src path in
      apply_path g src path flow  

let rec get_residual_graph g src tgt = 
  Printf.printf("get_residual_graph for %d %d\n") src tgt;
  let g2 = iteration g src tgt in
  match diff_graph g g2 with
  | true -> g
  | false -> get_residual_graph g2 src tgt

let get_ford_algo_arc ge gf arc = 
  match (find_arc ge arc.tgt arc.src) with
  | None -> new_arc gf {src=arc.src; lbl={value=0; max=arc.lbl}; tgt=arc.tgt}
  | Some x -> new_arc gf {src=arc.src; lbl={value=x.lbl; max=arc.lbl}; tgt=arc.tgt}

let to_ford_format g ge = 
  let g2 = clone_nodes ge in
  e_fold g (get_ford_algo_arc ge) g2

let ford g src tgt =
  let gr = get_residual_graph g src tgt in 
  to_ford_format g gr