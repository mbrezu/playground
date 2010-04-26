
open OUnit;;

let suite = "Parser tests" >::: [LexerTests.suite;
                                 PwmTests.suite;
                                 ExprTests.suite;
                                 SelectTests.suite;
                                 StmtTests.suite;
                                ]

let _ =
  run_test_tt_main suite


