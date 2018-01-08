
module Constraint = struct
  let fixed = 1.
  let free = 0.
  type t = Fixed | Free
  let to_float c =
    match c with
    | Fixed -> fixed
    | Free -> free

  let of_float f =
    if f = fixed then Fixed
    else
      if f = free then Free
      else raise (Invalid_argument "Valid arguments are 0. and 1.")
end

module Data = struct
  type t = Sundials.RealArray.t
  type tdapprox = t -> t
  let of_DAE_data epsilon resf dapprox t y0 fixy0 u0 cu0 =

    (* We need two new variables for each fixed varaible *)
    let idx_arr =
      fixy0
      |> Sundials.RealArray.to_array
      |> Array.mapi (fun i e -> (i, e))
      |> Array.fold_left
           (fun l (i, e) -> if e = Constraint.fixed then i::i::l else l) []
      |> Array.of_list
    in
    let n = Sundials.RealArray.length y0 in
    let m = Array.length idx_arr in

    (* build a system of equations from the residual function and the new variables
     * governing fixed guesses *)
    let sysf u r =

      (* Represents the state variables from the original problem *)
      let upper_arr a = Sundials.RealArray.sub a 0 n in

      (* Represents the added variables to handle constraints *)
      let lower_arr a = Sundials.RealArray.sub a n m in

      (* For each fixed guess a for variable v, we add the functions
       * v_1 = v - a + epsilon and v_2 = v - a - epsilon in addition to
       * the constraints v_1 >= 0 and v_2 <= 0 *)
      let f i u =
        let v = u.{i + n} -. u.{idx_arr.(i)} +. y0.{idx_arr.(i)} in
        if i mod 2 = 0 then v -. epsilon else v +. epsilon
      in

      (* update r *)
      resf t (upper_arr u) (dapprox (upper_arr u)) (upper_arr r);
      Sundials.RealArray.mapi (fun i _ -> f i u) (lower_arr r)
    in

    (* Fill u0 with values from y0 *)
    Sundials.RealArray.mapi
      (fun i _ ->
        if i < n then y0.{i}
        else y0.{idx_arr.(i - n)}) u0;

    (* Fill cu0 with constraints bounding variables associated with
     * fixed guesses *)
    Sundials.RealArray.mapi
      (fun i _ ->
        if i >= n then
          if i mod 2 = 0 then Sundials.Constraint.geq_zero
          else Sundials.Constraint.leq_zero
        else
          Sundials.Constraint.unconstrained) cu0;
    sysf
end
