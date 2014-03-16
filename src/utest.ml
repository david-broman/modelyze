
open Ustring.Op
open Printf

let name = ref ""
let count_ok = ref 0
let count_fail = ref 0
let total_ok = ref 0
let total_fail = ref 0
let textshown = ref false

let maintext() =
  if !textshown then () else (
  textshown := true;
  printf "Simple unit testing framework - Utest\n";
  printf "(C) Copyright David Broman, 2013.\n\n")
  
let init str =
   maintext();
   name := str;
   printf "Module: '%s'\n" str

let test str ok =
  maintext();
  printf "%d. Test '%s' : " (!count_ok + !count_fail+1) str;
  if ok then (
    count_ok := !count_ok + 1;
    printf "OK\n")
  else (
    count_fail := !count_fail + 1;
    printf "FAIL\n") 

let test_ext str res fail_str ok_str = 
  test str res;
  if res then ()
  else (
    uprint_endline (us"   Expected: '" ^. ok_str ^. us"'");
    uprint_endline (us"   Result:   '" ^. fail_str ^. us"'"))
  
let test_int str res exp =
  test_ext str (res = exp) (ustring_of_int res) (ustring_of_int exp)

let test_str str result expected =
  let res = (result = expected) in
  test str res;
  if res then ()
  else (
    printf "   Expected: '%s'\n" expected;
    printf "   Result:  '%s'\n" result)

let test_ustr str result expected =
  let res = (result =. expected) in
  test str res;
  if res then ()
  else (
    uprint_endline (us"   Expected: '" ^. expected ^. us"'");
    uprint_endline (us"   Result:   '" ^. result ^. us"'"))


let test_list str result expected elem2str =
  let res = (result = expected) in
  let tostr lst = us"[" ^. Ustring.concat (us",") 
                  (List.map elem2str lst) ^. us"]" in
  test_ext str res (tostr result) (tostr expected)

let test_array str result expected elem2str =
  test_list str (Array.to_list result) (Array.to_list expected) elem2str

  
let result() =
  maintext();
  printf "Result: %d successful and  %d failed test(s).\n\n" (!count_ok) (!count_fail);
  name := "";
  total_ok := !total_ok + !count_ok;
  count_ok := 0;
  total_fail := !total_fail + !count_fail;
  count_fail := 0;
  if !total_fail > 0 then (exit 1) else ()

let summary() =  
  maintext();
  printf "========================================================\n";
  printf "Test summary: Successful tests = %d Failed tests = %d\n" !total_ok !total_fail;
  printf "========================================================\n"
  

