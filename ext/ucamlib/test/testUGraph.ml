


open Ustring.Op
open Utest
open Ugraph


let main() = 

  init "Test the UGraph module.";


  (** ------------------- Topological Sort ------------------- *)

  (* Topological test 1 *)  
  let graph = [| [1;2];[3];[3];[4];[] |] in
  test_list "Simple topological sort of digraph."
            (Ugraph.topological_sort graph) [0;2;1;3;4]
             ustring_of_int;

  (* Topological test 2 *)  
  let graph = [| [1;2];[0;3];[3];[4;0];[1] |] in
  let res = (try (let _ = Ugraph.topological_sort graph in false)
             with _ -> true) in
  test "Simple topological sort with cycles." res;

  (* Topological test 3 *)  
  let graph = [| [1];[3;2];[];[6];[2;5];[2];[] |] in
  test_list "Another topological sort"
            (Ugraph.topological_sort graph) [4;5;0;1;2;3;6] 
             ustring_of_int;


  (** ------------------- Dominator tree ------------------- *)



  (** ----------- Strongly Connected Components ------------ *)

  let ustring_of_intlist lst =  
    us"[" ^. Ustring.concat (us",") 
    (List.map ustring_of_int lst) ^. us"]" in


  (* Test 1 *)  
  let graph = [| [];[3];[1;4];[1];[0;1;5];[0;2;3] |] in
  let expected = [[2;4;5];[1;3];[0]] in
  test_list "Strongly connected components, test 1"
            (Ugraph.strongly_connected_components graph) expected 
             ustring_of_intlist;
  
  

  (** ------------------- Reverse and Undirected  ------------ *)
   
  let graph = [| [];[3];[1;4];[1];[0;1;5];[0;2;3] |] in
  let expected = [|[5;4];[4;3;2];[5];[5;1];[2];[4]|] in
  test_array "Reverse directed graph" (Ugraph.reverse graph) expected
             ustring_of_intlist;

  let expected = [|[5;4];[4;2;3];[5;1;4];[5;1];[2;0;1;5];[4;0;2;3]|] in
  test_array "Make undirected" (Ugraph.make_undirected graph) expected
             ustring_of_intlist;
  

  (** ------------------- Shortest distance graph  ------------ *)
  let ustring_of_inttuple (e1,e2) = 
    us"(" ^. ustring_of_int e1 ^. us"," ^. ustring_of_int e2 ^. us")" in

  let ustring_of_inttuplelist lst =  
    us"[" ^. Ustring.concat (us",") 
    (List.map ustring_of_inttuple lst) ^. us"]" in

  let graph = [|[1];[2;3];[4];[3;4];[5];[6;7];[0];[]|] in
  let expected = [|[(1,4)];[(2,3);(3,3)];[(4,2)];[(3,3);(4,2)];[(5,1)];
                   [(6,6);(7,0)];[(0,5)];[]|] in
  test_array "Shortest distance graph" (Ugraph.shortest_distance_graph graph 7)
    expected ustring_of_inttuplelist;

    
  result()



