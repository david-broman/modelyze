open OUnit2
open Findic

let resf t y yp r =
  r.{0} <- y.{0} +. yp.{0};
  r.{1} <- y.{1} +. yp.{1}

let dapprox y = y

let y0 = Sundials.RealArray.of_list [1.; 2.]
let fixy0 = Sundials.RealArray.of_list [Constraint.fixed; Constraint.free]
let u0 = Sundials.RealArray.create 4
let cu0 = Sundials.RealArray.create 4
let r = Sundials.RealArray.create 4
let epsilon = 1.0e-5

let test_of_DAE_data = (fun _ ->
    let sysf = Data.of_DAE_data epsilon resf dapprox 0. y0 fixy0 u0 cu0 in
    let u0exp = Sundials.RealArray.of_list [1.; 2.; 1.; 1.] in
    let cu0exp = Sundials.RealArray.of_list [0.; 0.; 1.; -1.] in
    let rexp = Sundials.RealArray.of_list [2.;
                                           4.;
                                           3. -. 1. +. 1. -. epsilon;
                                           4. -. 1. +. 1. +. epsilon] in
    sysf (Sundials.RealArray.of_list [1.; 2.; 3.; 4.]) r;
    assert_equal rexp r;
    assert_equal u0exp u0;
    assert_equal cu0exp cu0
  )

let test_to_DAE_data = (fun _ ->
    let u = Sundials.RealArray.of_list [1.; 2.; 3.; 4.; 5.] in
    let dapprox y = y in
    let y = Sundials.RealArray.create 4 in
    let yp = Sundials.RealArray.create 4 in
    let yexp = Sundials.RealArray.of_list [1.; 2.; 3.; 4.] in
    let ypexp = dapprox yexp in
    Data.to_DAE_data dapprox u y yp;
    assert_equal yexp y;
    assert_equal ypexp yp;
  )

let tests = "test suite for Constraints" >::: [
      "Free to float" >:: (fun _ ->
        assert_equal (Constraint.to_float Constraint.Free) 0.);
      "Fixed to float" >:: (fun _ ->
        assert_equal (Constraint.to_float Constraint.Fixed) 1.);
      "Free of float" >:: (fun _ ->
        assert_equal (Constraint.of_float 0.) Constraint.Free);
      "Fixed of float" >:: (fun _ ->
        assert_equal (Constraint.of_float 1.) Constraint.Fixed);
      "test of DAE data" >:: test_of_DAE_data;
      "test to DAE data" >:: test_to_DAE_data;
    ]

let _ = run_test_tt_main tests
