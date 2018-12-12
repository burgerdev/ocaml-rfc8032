open OUnit

let _ =
  "Rfc8032_Suite" >::: []
  |> run_test_tt_main
