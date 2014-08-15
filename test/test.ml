open OUnit2


let tests =
  "tests" >::: [
    Test_ngamia_message.tests;
  ]

let () = run_test_tt_main tests
