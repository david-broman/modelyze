
open Ustring.Op
open Printf
open Utest
open Utils

let testdata = "\x00\x1f\x55\xf8\x21\x20\x43\x20\x50\x66\x00\x1f\xbb\xaa\x12\x32\x78" ^
               "\xe1\xe4\x32\x33\x01\x07\xab\x22\x99\x9a\xb3\x32\x17\x87\x23\xa1\xb1" 
let filename = "testdata.bin"

let main() = 

  init "Test the UString module.";


  (** ------------------- Trim functions ------------------- *)

  (* --- *)
  let text = "Trim function. " in
  let res = Ustring.trim (us"  This is how it should be   ") in 
  let exp = us"This is how it should be" in
  test_ustr text res exp;

  (* --- *)
  let text = "Trim left function. " in
  let res = Ustring.trim_left (us"  This is how it should be   ") in 
  let exp = us"This is how it should be   " in
  test_ustr text res exp;

  (* --- *)
  let text = "Trim right function. " in
  let res = Ustring.trim_right (us"  This is how it should be   ") in 
  let exp = us"  This is how it should be" in
  test_ustr text res exp;

  (** ------------------- Split function ------------------- *)

  (* --- *)
  let text = "Split function. Test 1." in
  let res = Ustring.split (us"This is a test") (us" ") in 
  let exp = List.map us ["This";"is";"a";"test"] in
  test_list text res exp (fun x -> us"\"" ^. x ^. us"\"");

  (* --- *)
  let text = "Split function. Test 2." in
  let res = Ustring.split (us"") (us" ") in 
  let exp = List.map us [] in
  test_list text res exp (fun x -> us"\"" ^. x ^. us"\"");

  (* --- *)
  let text = "Split function. Test 3." in
  let res = Ustring.split (us",foo1,foo2,") (us",") in 
  let exp = List.map us ["";"foo1";"foo2";""] in
  test_list text res exp (fun x -> us"\"" ^. x ^. us"\"");

  (* --- *)
  let text = "Split function. Test 4." in
  let res = Ustring.split (us",") (us",") in 
  let exp = List.map us ["";""] in
  test_list text res exp (fun x -> us"\"" ^. x ^. us"\"");

  (* --- *)
  let text = "Split function. Test 5." in
  let res = Ustring.split (us"foo") (us",") in 
  let exp = List.map us ["foo"] in
  test_list text res exp (fun x -> us"\"" ^. x ^. us"\"");

  (* --- *)
  let text = "Split function. Test 6." in
  let res = Ustring.split (us"foo,hi:can:,bye") (us",:") in 
  let exp = List.map us ["foo";"hi";"can";"";"bye"] in
  test_list text res exp (fun x -> us"\"" ^. x ^. us"\"");

 
  result()
