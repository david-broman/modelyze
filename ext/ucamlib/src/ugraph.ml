
open Printf

exception Not_a_DAG

type graph = (int list) array



(** ---------------------- Topological sort ------------------------ *)
  
type topmark = TempMark | PermMark | NoMark

let topological_sort graph = 
  let nodes = Array.length graph in
  let marked = Array.make nodes NoMark in
  let rec visit acc v =
    match marked.(v) with
    | TempMark -> raise Not_a_DAG
    | PermMark -> acc
    | NoMark -> 
        marked.(v) <- TempMark;
        let acc2 = List.fold_left visit acc graph.(v) in
        marked.(v) <- PermMark;
        v::acc2      
  in
    Utils.fold_interval visit [] 0 (nodes-1)


(** ---------------------- Dominator -------------------------------- *)

   
let dominator out_graph v = [||]
(*
  let nodes = Array.length graph in
  let visited = Array.make nodes false in
  let domtree = Array.make nodes (-1) in
  let in_graph = Array.make nodes [] in
  for v = 0 to nodes-1 do
    List.iter (fun w -> in_graph.(w) <- (v::(in_graph.(w)))) (out_graph.(v))      
  done;
  let rec visit v =
    if not visited.(v) && List.for_all (fun w -> visited.(w)) (in_graph.(v)) then      
      visited.(v) <- true;
      domtree.(v) <- find_idom (in_graph.(v));
      List.iter visit (out_graph.(v));
  in
    visit v ;
    domtree
*)
    

(** ---------------------- Strong -------------------------------- *)

let strongly_connected_components graph = 
  let nodes = Array.length graph in
  let lowlink = Array.make nodes 0 in
  let number = Array.make nodes 0 in
  let stack = Stack.create() in
  let stackmem = Array.make nodes false in
  let i = ref 0 in
  let scomps = ref [] in
  let rec strong_connect v = 
    i := !i + 1;
    lowlink.(v) <- !i;
    number.(v) <- !i;
    Stack.push v stack;
    stackmem.(v) <- true;
    List.iter (fun w ->
      if number.(w) = 0 then (
        strong_connect w;
        lowlink.(v) <- min lowlink.(v) lowlink.(w))
      else if stackmem.(w) && number.(w) < number.(v) then
        lowlink.(v) <- min lowlink.(v) number.(w)
    ) graph.(v);
    if lowlink.(v) = number.(v) then
      let comp = ref [] in
      while not (Stack.is_empty stack) && number.(Stack.top stack) >= number.(v) do
        let elm = Stack.pop stack in        
        comp := elm::(!comp);
        stackmem.(elm) <- false;
      done;
      scomps := (!comp)::(!scomps)
  in 
    for v = 0 to nodes-1 do
      if number.(v) = 0 then
        strong_connect v;
    done; 
    !scomps 


let reverse graph  =
  let rev = Array.make (Array.length graph) [] in
  Array.iteri (fun v ws -> List.iter (fun w ->
    rev.(w) <- v::(rev.(w))) ws) graph;
  rev


let make_undirected graph =
  let undir = Array.copy graph in
  Array.iteri (fun v ws -> List.iter (fun w ->
    if not (List.mem v (undir.(w))) then undir.(w) <- v::(undir.(w))
  ) ws) graph;
  undir


let shortest_distance_graph_ext rev_graph v_start = 
  let nodes = Array.length rev_graph in
  let visited = Array.make nodes false in
  let dist_graph = Array.make nodes [] in
  let queue = Queue.create() in
  Queue.add (v_start,0) queue;
  while not (Queue.is_empty queue) do
    let (v,dist) = Queue.take queue in
    if not visited.(v) then (
      visited.(v) <- true;
      List.iter (fun w ->
         dist_graph.(w) <- (v,dist)::(dist_graph.(w));       
         if not visited.(w) then
           Queue.add (w,dist+1) queue
      ) rev_graph.(v);
    );
  done;
  dist_graph
  

let shortest_distance_graph graph v = 
  shortest_distance_graph_ext (reverse graph) v









