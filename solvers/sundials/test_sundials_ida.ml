(*
    Modeling Kernel Language (Modelyze) evaluator
    Copyright (C) 2010-2012 David Broman

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open OUnit
open Sundials
open Printf


let residual time yy yp = [|0.0;0.0;0.0;0.0|]  

let test_ida_make_def_tolerance() =
  let deftol = 1.0e-5 in
  let si = Ida.make [|1.0|] [|1.0|] residual in
  assert_bool "Default relative tolerance" (Ida.reltol si = deftol);
  assert_bool "Default absolute tolerance" (Ida.abstol si = deftol)

let test_ida_make_init() =
  let reltol = 16.5123 and abstol = 232.1221 in
  let y = [|77.10;3.3;521.121;33.12|] and yp = [|2.0;2.1;333.331;0.0;|] in
  let si = Ida.make ~reltol:reltol ~abstol:abstol y yp residual in
  assert_bool "Set/get relative tolerance" (Ida.reltol si = reltol);
  assert_bool "Set/get absolute tolerance" (Ida.abstol si = abstol);
  assert_equal ~msg:"Initial variables" (Ida.y si) y;
  assert_equal ~msg:"Initial differentiated variables" (Ida.yp si) yp;
  Ida.close si



