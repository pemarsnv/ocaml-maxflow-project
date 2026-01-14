open Graph 
open Gfile

type flow = {
    value: int;
    max: int;
}

val get_residual_graph : int graph -> id -> id -> int graph

val depth : int graph -> id -> id -> id list -> id list -> id list

val get_flow_btw : int graph -> id -> id list -> int

val apply_path : int graph -> id -> id list -> int -> int graph

val is_arc_augmenting : int arc -> bool

val get_residual_value : int arc -> int

val diff_graph : 'a graph -> 'a graph -> bool

val to_ford_format : int graph -> int graph -> flow graph

val export_ford: path -> flow graph -> unit

val ford : int graph -> int -> int -> flow graph


