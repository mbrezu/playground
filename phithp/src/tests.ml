
open OUnit

let suite = "Phithp Tests" >::: [ SexpTests.suite;
                                  FormatterTests.suite;
                                  ParserTests.suite;
                                  EvalTests.suite;
                                  CompilerTests.suite ]

let _ =
  run_test_tt_main suite
