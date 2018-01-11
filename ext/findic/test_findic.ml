open OUnit2
open Findic

let resf t y yp r =
  r.{0} <- y.{0} +. yp.{0};
  r.{1} <- y.{1} +. yp.{1}

let y0 = Sundials.RealArray.of_list [1.; 2.]
let y0fix = Sundials.RealArray.of_list [Constraint.fixed; Constraint.free]
let yp0 = Sundials.RealArray.of_list [3.; 4.]
let yvarvid = Sundials.RealArray.of_list [Ida.VarId.differential; Ida.VarId.differential]
let u0 = Sundials.RealArray.create 4
let up0 = Sundials.RealArray.create 4
let uvarvid = Sundials.RealArray.create 4
let u0c = Sundials.RealArray.create 4
let r = Sundials.RealArray.create 4
let epsilon = 1.0e-5

let test_to_fixed_DAE_IC_Data = (fun _ ->
    let u0exp = Sundials.RealArray.of_list [1.; 2.; epsilon; -.epsilon] in
    let up0exp = Sundials.RealArray.of_list [3.; 4.; 3.; 3.] in
    let uvarvidexp = Sundials.RealArray.of_list [Ida.VarId.differential;
                                                 Ida.VarId.differential;
                                                 Ida.VarId.algebraic;
                                                 Ida.VarId.algebraic] in
    let u0cexp = Sundials.RealArray.of_list [Sundials.Constraint.unconstrained;
                                             Sundials.Constraint.unconstrained;
                                             Sundials.Constraint.geq_zero;
                                             Sundials.Constraint.leq_zero] in
    let rexp = Sundials.RealArray.of_list [2.;
                                           4.;
                                           3. -. 1. +. 1. -. epsilon;
                                           4. -. 1. +. 1. +. epsilon] in
    let newresf = Data.to_fixed_DAE_IC_Data
                    epsilon resf y0 y0fix yp0 yvarvid u0 up0 uvarvid u0c
    in
    newresf
      0.
      (Sundials.RealArray.of_list [1.; 2.; 3.; 4.])
      (Sundials.RealArray.of_list [1.; 2.; 3.; 4.])
      r;
    assert_equal u0exp u0;
    assert_equal up0exp up0;
    assert_equal uvarvidexp uvarvid;
    assert_equal u0cexp u0c;
    assert_equal rexp r;
  )

let test_of_fixed_DAE_IC_Res = (fun _ ->
    let u0in = Sundials.RealArray.of_list [1.; 2.; 3.; 4.;] in
    let up0in = Sundials.RealArray.of_list [5.; 6.; 7.; 8.] in
    let y0act = Sundials.RealArray.create 2 in
    let yp0act = Sundials.RealArray.create 2 in
    let y0exp = Sundials.RealArray.of_list [1.; 2.] in
    let yp0exp = Sundials.RealArray.of_list [5.; 6.] in
    Data.of_fixed_DAE_IC_Res u0in up0in y0act yp0act;
    assert_equal y0act y0exp;
    assert_equal yp0act yp0exp;
  )

let test_nfixed = (fun _ ->
    let fixed = Sundials.RealArray.of_list [Constraint.fixed;
                                            Constraint.free;
                                            Constraint.free;
                                            Constraint.fixed]
    in
    let nexp = 2 in
    let neact = Data.nfixed fixed in
    assert_equal neact nexp;
  )

let tests = "test suite for Findic" >::: [
      "Free to float" >:: (fun _ ->
        assert_equal (Constraint.to_float Constraint.Free) 0.);
      "Fixed to float" >:: (fun _ ->
        assert_equal (Constraint.to_float Constraint.Fixed) 1.);
      "Free of float" >:: (fun _ ->
        assert_equal (Constraint.of_float 0.) Constraint.Free);
      "Fixed of float" >:: (fun _ ->
        assert_equal (Constraint.of_float 1.) Constraint.Fixed);
      "test to DAE data" >:: test_to_fixed_DAE_IC_Data;
      "test of DAE res data" >:: test_of_fixed_DAE_IC_Res;
      "test nfixed" >:: test_nfixed;
    ]

let test_findic = (fun _ ->
    let resf t v v' r =
      r.{0} <- v.{2}  -. v'.{0};
      r.{1} <- v.{3}  -. v'.{1};
      r.{2} <- v'.{2} -. v.{4} *. v.{0};
      r.{3} <- v'.{3} -. v.{4} *. v.{1} +. 9.81;
      r.{4} <- v.{2}*.v.{2} +. v'.{2}*.v.{0} +. v.{3}*.v'.{1} +. v'.{3}*.v.{1}
    in

    let v  = Sundials.RealArray.of_list [ 1.; 2.; 3.; 4.; 5. ] in
    let vfix = Sundials.RealArray.make 5 Constraint.free in
    let v' = Sundials.RealArray.make 5 1. in
    (* vfix.{1} <- Constraint.fixed; *)
    print_string "Before correction \n";
    Sundials.RealArray.pp Format.std_formatter v;
    Sundials.RealArray.pp Format.std_formatter v';

    let vids = Sundials.RealArray.make 5 Ida.VarId.differential in

    vids.{4} <- Ida.VarId.algebraic;

    findic epsilon resf 0.01 0. v vfix v' vids v v';

    print_string "\nAfter correction \n";
    Sundials.RealArray.pp Format.std_formatter v;
    Sundials.RealArray.pp Format.std_formatter v'
  )

let _ = run_test_tt_main tests

let _ = test_findic()
