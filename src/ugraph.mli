

exception Not_a_DAG


type graph = (int list) array
(** A graph is represented as an adjacency list. Each vertex in the
    graph is given a unique integer value *)

val topological_sort : graph -> int list 
(** [topological_sort G] takes a directed graph [G] and returns a topologically 
    sorted list of nodes. The function is implemented using depth-first search 
    according to Cormen et al. (2001) *)

val dominator : graph -> int -> int array
(** [dominator G v] computes the dominator tree for the directed graph [G]
    with start node [v]. The returned dominator tree is stored in an integer
    array such that dom[v] = parent of note v. This function assumes that
    all vertices in the graph are reachable from [v]. *)

val strongly_connected_components : graph -> (int list) list
(** [strongly_connected_components G] returns a list of the strongly connected
    components of graph [G]. Each element in the list is a list of integers 
    representing the vertices in each component. The order of the returned 
    strongly connected components is topologically sorted. *)

val reverse : graph -> graph
(** Returned the reverse of a directed graph, that is, each edge
    (v,w) in E is replaced with (w,v). *)

val make_undirected : graph -> graph
(** Takes a directed graph and add edges so that the graph becomes 
    undirected. If the input graph has no redundant edges, the resulting
    graph will not have any redundant edges. *)

val shortest_distance_graph : graph -> int -> ((int * int) list) array
(** [shortest_distance_graph G v] computes the distance for vertex [v]
    to all vertices in the directed graph [G]. If the an vertex [w] is
    predecessor to [v], then the distance is 0. The function returns a
    graph, where the array is indexed using vertex numbers and each
    associate lists represents adjacent vertices. The key in the
    associate list is the vertex and the value is the distance to
    vertex [v]. For instance, for a graph G=(V,E), where V=\{v0,v1,v2\}
    and E=\{(v0,v1),(v1,v2)\}, and the start v is 2, then the resulting
    distance graph (in array form) is [[| [(v1,1)]; [v2,0]; [] |]] *)

val shortest_distance_graph_ext : graph -> int -> ((int * int) list) array
(** [shortest_distance_graph_ext G_r v] is identical to [shortest_distance_graph]
    with the exception that extended version takes the reversed graph [G_r] instead of
    [G]. It is useful, for performance reason, to call this 
    function instead if the caller already have computed the reverse graph. *)


