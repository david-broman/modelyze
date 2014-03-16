
open Printf

let main = 
  TestUtils.main();
  TestUGraph.main();
  TestUArg.main();
  TestUString.main();
  Utest.summary()
   
