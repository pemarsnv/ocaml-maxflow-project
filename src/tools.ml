open Graph

let clone_nodes gr = let g = empty_graph in n_fold gr new_node g;;

let gmap gr f =
  let g = n_fold gr (fun g n -> new_node g n) empty_graph in
  let g2 g x = new_arc g { src = x.src; tgt = x.tgt; lbl = f x.lbl }
  in e_fold gr g2 g;;

let add_arc g id1 id2 n = 
  match (find_arc g id1 id2) with
  | None -> new_arc g {src=id1; tgt=id2; lbl=n}
  | Some x -> new_arc g {src=id1; tgt=id2; lbl=(n+x.lbl)};;
