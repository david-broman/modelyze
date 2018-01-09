
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

  let to_fixed_DAE_IC_Data epsilon resf t0 y0 y0fix yp0 yvarid u0 up0 uvarid u0c =
    let ly0 = Sundials.RealArray.length y0 in
    let ly0fix = Sundials.RealArray.length y0fix in
    let lyp0 = Sundials.RealArray.length yp0 in
    let lyvarvid = Sundials.RealArray.length yvarid in
    let lu0 = Sundials.RealArray.length u0 in
    let lup0 = Sundials.RealArray.length up0 in
    let luvarvid = Sundials.RealArray.length uvarid in
    let lu0c = Sundials.RealArray.length u0c in

    (* We need two new variables for each fixed varaible *)
    let idx_arr =
      y0fix
      |> Sundials.RealArray.to_array
      |> Array.mapi (fun i e -> (i, e))
      |> Array.fold_left (fun l (i, e) ->
             if e = Constraint.fixed then i::i::l else l) []
      |> Array.of_list
    in

    (* number of original variables *)
    let n = ly0 in

    (* number of new variables to account for fixed guesses *)
    let m = Array.length idx_arr in

    (* Input validation *)
    if
      ly0 != lyp0 || ly0 != ly0fix || ly0 != lyvarvid || lu0 != lup0 ||
        lu0 != luvarvid || lu0 != lu0c || (ly0 + n) != lu0
    then raise (Failure "Vector dimensions does not match");

    let newrestf t u up r =

      (* Represents the state variables from the original problem *)
      let upper_arr a = Sundials.RealArray.sub a 0 n in

      (* Represents the added variables to handle constraints *)
      let lower_arr a = Sundials.RealArray.sub a n m in

      (* For each fixed guess a for variable v, we add the equations
       * v_1 = v - a + epsilon and v_2 = v - a - epsilon in addition to
       * the constraints v_1 >= 0 and v_2 <= 0 *)
      let f i u =
        let v = u.{i + n} -. u.{idx_arr.(i)} +. y0.{idx_arr.(i)} in
        if i mod 2 = 0 then v -. epsilon else v +. epsilon
      in

      (* update r *)
      resf t (upper_arr u) (upper_arr up) (upper_arr r);
      Sundials.RealArray.mapi (fun i _ -> f i u) (lower_arr r)
    in

    let mapi_u_l mapi_u mapi_l a =
      Sundials.RealArray.mapi (fun i e ->
        if i < n then mapi_u i e else mapi_l i e) a
    in

    (* Fill u0 with values from y0 *)
    mapi_u_l (fun i _ -> y0.{i}) (fun i _ -> y0.{idx_arr.(i - n)}) u0;

    (* Fill up0 with values from yp0 *)
    mapi_u_l (fun i _ -> yp0.{i}) (fun i _ -> yp0.{idx_arr.(i - n)}) up0;

    (* Fill uvarid with values from yvarvid *)
    mapi_u_l (fun i _ -> yvarid.{i}) (fun i _ -> Ida.VarId.algebraic) uvarid;

    (* Fill u0c with constraints bounding variables associated with
     * fixed guesses *)
    mapi_u_l
      (fun i _ -> Sundials.Constraint.unconstrained)
      (fun i _ ->
        if i mod 2 = 0 then Sundials.Constraint.geq_zero
        else Sundials.Constraint.leq_zero)
      u0c;

    newrestf
end
