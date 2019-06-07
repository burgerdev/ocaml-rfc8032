
open OUnit

open Rfc8032.Internals

type z_of_cstruct_testcase = {data: string; bits: int; expected_result: int}


let z_of_cstruct_testcases = List.map begin fun {data; bits; expected_result} ->
    Fmt.strf "data=%s,bits=%d" data bits >:: fun _ ->
      let buf = Cstruct.of_hex data in
      let actual_result = Serde.z_of_cstruct bits buf in
      assert_equal ~printer:(Fmt.strf "%a" Z.pp_print)
        (Z.of_int expected_result)
        actual_result
  end
    [ {data="ff"; bits=8; expected_result=255}
    ; {data="ff"; bits=7; expected_result=127}
    ; {data="ff"; bits=6; expected_result=63}
    ; {data="ff"; bits=5; expected_result=31}
    ; {data="ff"; bits=4; expected_result=15}
    ; {data="ff"; bits=3; expected_result=7}
    ; {data="ff"; bits=2; expected_result=3}
    ; {data="ff"; bits=1; expected_result=1}
    ; {data="ff"; bits=0; expected_result=0}
    ; {data="ffff"; bits=16; expected_result=65535}
    ; {data="ffff"; bits=15; expected_result=32767}
    ; {data="ffff"; bits=14; expected_result=16383}
    ; {data="ffff"; bits=13; expected_result=8191}
    ; {data="ffff"; bits=12; expected_result=4095}
    ; {data="ffff"; bits=11; expected_result=2047}
    ; {data="ffff"; bits=10; expected_result=1023}
    ; {data="ffff"; bits=9; expected_result=511}
    ; {data="ffff"; bits=8; expected_result=255}
    ; {data="ffff"; bits=7; expected_result=127}
    ; {data="ffff"; bits=6; expected_result=63}
    ; {data="ffff"; bits=5; expected_result=31}
    ; {data="ffff"; bits=4; expected_result=15}
    ; {data="ffff"; bits=3; expected_result=7}
    ; {data="ffff"; bits=2; expected_result=3}
    ; {data="ffff"; bits=1; expected_result=1}
    ; {data="ffff"; bits=0; expected_result=0}
    ; {data="0a"; bits=8; expected_result=10}
    ; {data="0a"; bits=4; expected_result=10}
    ; {data="0a"; bits=3; expected_result=2}
    ; {data="0a"; bits=2; expected_result=2}
    ; {data="0a"; bits=1; expected_result=0}
    ]

let _ =
  "Serialization_suite" >::: [ "decode_private" >::: z_of_cstruct_testcases ]
  |> run_test_tt_main
