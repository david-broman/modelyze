(* 
Copyright (c) 2010, David Broman
All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright 
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, 
      this list of conditions and the following disclaimer in the 
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Linköping University nor the names of its 
      contributors may be used to endorse or promote products derived from 
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open OUnit
open Ustring.Op

let debug_print_hex_of_string m s = Printf.printf "%s" m;
   (String.iter (fun c -> Printf.printf "%2x," (int_of_char c)) s);
   Printf.printf "\n"

let true_if_fail f = try f(); false with _ -> true 

(* Tests basic functions e.g. length, latin1 encoding etc. *)
let test_encoding_latin1() =
  let us1 = us"This is a string with Swedish characters åäö ÅÄÖ." in
  let us2 = us"" in
  let s1 = "üÿêÅÄÖ=é@åäöÜ" in
  assert_bool "assert 1" (Ustring.length us1 = 49);  
  assert_bool "assert 2" (Ustring.length us2 = 0);
  assert_bool "assert 3" 
    ((Ustring.to_latin1 (Ustring.from_latin1 s1)) = s1);
  assert_bool "assert 4" (Ustring.get us1 12 = int_of_char 'r');
  assert_bool "assert 5" (Ustring.get (us s1) 3 = int_of_char 'Å');
  let testval1 = try Ustring.get us1 49
                 with Invalid_argument _ -> -1 in
  assert_bool "assert 6" (testval1 = -1)

(* Encode Korean characters to latin1. Check that we get an exception *)
let test_encode_latin1_error() =
  let utf8_in = "\xED\x95\x9C\xEA\xB5\xAD\xEC\x96\xB4" in 
  let retval = 
    try let _ = Ustring.to_latin1 (Ustring.from_utf8 utf8_in) in false 
    with Invalid_argument _ -> true in
    assert_bool "assert 1" retval

(* Test out of range error with get function. *)
let test_encode_get_error() =       
  let foo i = 
    try let _ = Ustring.get (us"text") i in false 
    with Invalid_argument _ -> true in
    assert_bool "assert 1" (foo (-1));
    assert_bool "assert 2" (foo 4);
    assert_bool "assert 3" (foo 41312)

(* Test out of range error with get function. *)
let test_encode_set_error() =       
  let foo i = 
    try let _ = Ustring.set (us"text") i (uc 'a') in false 
    with Invalid_argument _ -> true in
    assert_bool "assert 1" (foo (-1));
    assert_bool "assert 2" (foo 4);
    assert_bool "assert 3" (foo 41312)

(* Example from STD 63/RFC3629, Section 7 
   Tests encoding of 1,2 and 3 bytes sequences. *)
let test_encoded_utf8_1() =
  let uchars = [|0x0041;0x2262;0x0391;0x002E|] in
  let utf8_in = "\x41\xE2\x89\xA2\xCE\x91\x2E" in
  let utf8_out = Ustring.to_utf8 (Ustring.from_uchars uchars) in
  assert_bool "assert 1" (utf8_in = utf8_out);
  let uchars_out = Ustring.to_uchars (Ustring.from_utf8 utf8_in) in
  assert_bool "assert 2" (uchars = uchars_out)
  
(* Example from STD 63/RFC3629, Section 7. Korean for "hangugeo" 
   meaning "the Korean langauge". 
   Tests encoding of 3 bytes sequences. *)
let test_encoded_utf8_2() =
  let uchars = [|0xD55C;0xAD6D;0xC5B4|] in
  let utf8_in = "\xED\x95\x9C\xEA\xB5\xAD\xEC\x96\xB4" in
  let utf8_out = Ustring.to_utf8 (Ustring.from_uchars uchars) in
  assert_bool "assert 1" (utf8_in = utf8_out);
  let uchars_out = Ustring.to_uchars (Ustring.from_utf8 utf8_in) in
  assert_bool "assert 2" (uchars = uchars_out)
    
(* Example from STD 63/RFC3629, Section 7. Japanese for "nihongo" 
   meaning "the Japanese language". 
   Tests encoding of 1,2 and 3 bytes sequences. *)
let test_encoded_utf8_3() =
  let uchars = [|0x65E5;0x672C;0x8A9E|] in
  let utf8_in = "\xE6\x97\xA5\xE6\x9C\xAC\xE8\xAA\x9E" in
  let utf8_out = Ustring.to_utf8 (Ustring.from_uchars uchars) in
  assert_bool "assert 1" (utf8_in = utf8_out);
  let uchars_out = Ustring.to_uchars (Ustring.from_utf8 utf8_in) in
  assert_bool "assert 2" (uchars = uchars_out)

(* Cherokee letter and mathematical symbols
   Tests encoding of 3 and 4 bytes sequences. *)
let test_encoded_utf8_4() =
  let uchars = [|0x1D4D0;0x13AF;0x1D51A|] in
  let utf8_in = "\xF0\x9D\x93\x90\xE1\x8E\xAF\xF0\x9D\x94\x9A" in
  let utf8_out = Ustring.to_utf8 (Ustring.from_uchars uchars) in
  assert_bool "assert 1" (utf8_in = utf8_out);
  let uchars_out = Ustring.to_uchars (Ustring.from_utf8 utf8_in) in
  assert_bool "assert 2" (uchars = uchars_out)

(* Test exceptions with wrong uchar input *)
let test_encoded_from_uchars()=
  let uchars1 = [|0x23;0x200000;0x123|] in
  let uchars2 = [|0x23;0x43;0x123;0x77|] in
  let uchars3 = [|0x23;0x43;-1;0x77|] in
  let foo s = 
    try let _ = Ustring.from_uchars s in false 
    with Invalid_argument _ -> true 
  in
      assert_bool "assert 1" (foo uchars1);
      assert_bool "assert 2" (not (foo uchars2));
      assert_bool "assert 3" (foo uchars3)

(* Example 1 from STD 63/RFC3629, Section 3. 
   Exampel 2 shows a utf8 string that is too short (cut-off in a char)
   Tests an illegal utf8 string *)
let test_encode_from_utf8_error()=
  let utf8_in1 = "\xC0\x80" in
  let utf8_in2 = "\xF0\x9D\x93\x90\xE1\x8E\xAF\xF0\x9D\x94" in
  let foo s = 
    try let _ = Ustring.from_utf8 s in false 
    with Invalid_argument _ -> true 
  in
    assert_bool "assert 1" (foo utf8_in1);
    assert_bool "assert 2" (foo utf8_in2)

(* Tests both append and fast_append. Also tests safe equality and
   inequality *)
let test_append_1() =
  let s1a = us"This is a string123456789. See if it works.ÅÄÖ" in
  let s1b = us"Thi" ^. (us"s is" ^. us" a string1") ^. us"23456" ^. 
    ((us"7" ^. us"" ^. us"89. ") ^. us"See if it works.ÅÄÖ") in
  let s1c = us"Thi" ^.. (us"s is" ^.. us" a string1") ^.. us"23456" ^.. 
    ((us"7" ^.. us"" ^.. us"89. ") ^.. us"See if it works.ÅÄÖ") in
  let s1d = us"Thi" ^. (us"s is" ^.. us" a string1") ^. us"23456" ^.. 
    ((us"7" ^.. us"" ^.. us"89. ") ^. us"See if it works.ÅÄÖ") in
  assert_bool "assert 1" (s1a =. s1b);
  assert_bool "assert 2" (s1a =. s1c);
  assert_bool "assert 3" (s1a =. s1d);
  assert_bool "assert 4" (not (s1a <>. s1b));
  assert_bool "assert 5" (not (s1a <>. s1c));
  assert_bool "assert 6" (not (s1a <>. s1d));
  assert_bool "assert 7" (Ustring.equal s1a s1d);
  assert_bool "assert 8" (not (Ustring.not_equal s1a s1d))

(* Tests both append and fast_append to varify that the normal equality
   operator '=' is unsafe. *)
let test_append_2() =
  let s1a = us"This is a string123456789. See if it works.ÅÄÖ" in
  let s1b = us"Thi" ^. (us"s is" ^. us" a string1") ^. us"23456" ^. 
    ((us"7" ^. us"" ^. us"89. ") ^. us"See if it works.ÅÄÖ") in
  let s1c = us"Thi" ^.. (us"s is" ^.. us" a string1") ^.. us"23456" ^.. 
    ((us"7" ^.. us"" ^.. us"89. ") ^.. us"See if it works.ÅÄÖ") in
  let s1d = us"Thi" ^. (us"s is" ^.. us" a string1") ^. us"23456" ^.. 
    ((us"7" ^.. us"" ^.. us"89. ") ^. us"See if it works.ÅÄÖ") in
  assert_bool "assert 1" (not (s1a = s1b));
  assert_bool "assert 2" (s1a = s1c);
  assert_bool "assert 3" (not (s1a = s1d))

(* Tests that functions Ustring.get and Ustring.length work even 
   if a fast_append was used to create the string *)
let test_append_3() = 
  let s = us"A string" ^. us" to test" in
  let c = Ustring.get s 13 in
  let l = Ustring.length s in
  assert_bool "assert 1" (c = uc 'e');
  assert_bool "assert 2" (c = Ustring.latin1_to_uchar 'e');
  assert_bool "assert 3" (l = 16)

(* Test that the behaviour of fast_append is different if we
   modify the substrings in place using function Ustring.set *)
let test_append_4() =
  let s1a = us"This_" in
  let s1b = us"string" in
  let s2a = us"This_" in
  let s2b = us"string" in
  let comp1 = us"This_sAring" in
  let comp2 = us"This_string" in
  let out1 = s1a ^. s1b in
  let out2 = s2a ^.. s2b in
  Ustring.set s1b 1 (uc 'A');
  Ustring.set s2b 1 (uc 'A');
  assert_bool "assert 1" (out1 =. comp1);
  assert_bool "assert 2" (out2 =. comp2)

(* Make sure that Ustring.append and Ustring.append_fast also
   works directly, without using operators '^.' and '^..' *)
let test_append_5() = 
  let s1 = us"A string to test" in
  let s2 = Ustring.append (us"A string")  (us" to test") in
  let s3 = Ustring.fast_append (us"A string")  (us" to test") in
  assert_bool "assert 1" (s1 =. s2);
  assert_bool "assert 2" (s1 =. s3)

(* Tests the compare function by creating a map of ustrings. *)
module USMap = Map.Make (Ustring)
let test_compare() =
  let maplen m = USMap.fold (fun _ _ n -> n+1) m 0 in
  let m = USMap.empty in
  assert_bool "assert 1" (USMap.is_empty m);
  let m = USMap.add (us"Text1") 10 m in  
  let m = USMap.add (us"Text2") 20 m in
  assert_bool "assert 2" (not (USMap.is_empty m));
  assert_bool "assert 3" (maplen m = 2);
  let m2 = USMap.add (us"Text1") 100 m in
  assert_bool "assert 4" ((USMap.find (us"Text1") m2) = 100);
  assert_bool "assert 5" ((USMap.find (us"Text1") m) = 10)

let test_conversion_functions() =
  assert_bool "assert 1" (ustring_of_bool true =. us"true");
  assert_bool "assert 2" (ustring_of_bool false =. us"false");
  assert_bool "assert 3" (bool_of_ustring (us"true"));
  assert_bool "assert 4" (not (bool_of_ustring (us"false")));
  assert_bool "assert 5" (true_if_fail (fun () -> bool_of_ustring (us"adf")));
  assert_bool "assert 6" (ustring_of_int 1432 =. us"1432");
  assert_bool "assert 7" (ustring_of_int (-5432) =. us"-5432");
  assert_bool "assert 8" (ustring_of_int (-0) =. us"0");
  assert_bool "assert 9" (int_of_ustring (us"1234") = 1234);
  assert_bool "assert 10" (int_of_ustring (us"-43222") = -43222);
  assert_bool "assert 11" (int_of_ustring (us"-0") = 0);
  assert_bool "assert 12" (true_if_fail (fun () -> int_of_ustring (us"adf")));
  assert_bool "assert 13" (true_if_fail (fun () -> int_of_ustring (us"1.23")));
  assert_bool "assert 14" (ustring_of_float 123.21 =. us"123.21");
  assert_bool "assert 15" (ustring_of_float (-.0.1231) =. us"-0.1231");
  assert_bool "assert 16" (ustring_of_float 3.212e-12 =. us"3.212e-12");
  assert_bool "assert 17" (float_of_ustring (us"123.21") = 123.21);
  assert_bool "assert 18" (float_of_ustring (us"-0.1231") = -.0.1231);
  assert_bool "assert 19" (float_of_ustring (us"3.212e-12") = 3.212e-12);
  assert_bool "assert 19" (float_of_ustring (us"12") = 12.0); 
  assert_bool "assert 20" (true_if_fail (fun () -> float_of_ustring (us"adf")));
 
(* Tests using ustrings as keys in a map*)
module USHash = Hashtbl.Make(Ustring)
let test_hashtbl() =
  let h = USHash.create 1024 in
  assert_bool "assert 1" (USHash.length h = 0);
  USHash.add h (us"eleven") 11;
  USHash.add h (us"three") 3;
  assert_bool "assert 2" (USHash.length h = 2);
  assert_bool "assert 3" (USHash.find h (us"eleven") = 11);
  USHash.replace h (us"eleven") 77;
  assert_bool "assert 4" (USHash.find h (us"eleven") = 77);
  USHash.replace h (us"ele" ^. us"ven") 88;
  assert_bool "assert 5" (USHash.find h (us"elev" ^. us"en") = 88)


  

