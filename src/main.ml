open List
open Printf

type 'a edge =
  { data : 'a;
    cost : int }

type 'a vertex =
  { data : 'a;
    edges : 'a edge list }

type 'a graph =
  { verts : 'a vertex list }

let contains_vertex graph data =
  let rec aux lst data =
    match lst with
    | [] -> false
    | hd :: tl when hd.data = data -> true
    | _ :: tl -> aux tl data in
  aux graph.verts data

let add_vertex graph data =
  match contains_vertex graph data with
  | true -> graph
  | false -> { verts = graph.verts @ [{ data = data; edges = [] }] }

let add_edge graph src dest cost =
  let rec aux lst i idx fdest =
    match lst with
    | [] -> idx, fdest
    | hd :: tl when hd.data = src -> aux tl (i + 1) (Some i) fdest
    | hd :: tl when hd.data = dest -> aux tl (i + 1) idx true
    | _ :: tl -> aux tl (i + 1) idx fdest in

  let idx, fdest = aux graph.verts 0 None false in

  let graph, idx =
    match idx with
    | Some k -> graph, k
    | _ -> add_vertex graph src, length graph.verts in

  let graph = if fdest then graph else add_vertex graph dest in

  { verts = mapi (fun i v ->
                if i <> idx then v
                else { v with edges = v.edges @ [{ data = dest; cost = cost }] }
              ) graph.verts }

(* This function only works if 'a = char. *)
let char_graph_print (graph : char graph) : unit =
  iter (fun (v : char vertex) ->
      let _ = printf "vertex: %c" v.data in
      let _ = iter (fun (e : char edge)
                         -> printf " -> [%c, %d]" e.data e.cost) v.edges in
      print_endline ""
    ) graph.verts

let () =
  let g = { verts = [] } in

  let g = add_vertex g 'a' in
  let g = add_vertex g 'b' in
  let g = add_vertex g 'c' in

  let g = add_edge g 'a' 'b' 1 in
  let g = add_edge g 'a' 'c' 2 in
  let g = add_edge g 'c' 'b' 3 in
  let g = add_edge g 'x' 'y' 4 in
  let g = add_edge g 'x' 'x' 0 in
  let g = add_edge g 'b' 'y' 8 in
  let g = add_edge g 'y' 'b' 10 in

  char_graph_print g
