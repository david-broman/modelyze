


open Ustring.Op
open Utest
open Utils

let testdata = "\x00\x1f\x55\xf8\x21\x20\x43\x20\x50\x66\x00\x1f\xbb\xaa\x12\x32\x78" ^
               "\xe1\xe4\x32\x33\x01\x07\xab\x22\x99\x9a\xb3\x32\x17\x87\x23\xa1\xb1" 
let filename = "testdata.bin"

let main() = 

  init "Test the Utils module.";

  (* --- *) 
  write_binfile filename testdata;
  let testdata2 = read_binfile filename in
  test_str "Test loading and writing a binary file." testdata testdata2;
  Sys.remove filename;

 
  result()
