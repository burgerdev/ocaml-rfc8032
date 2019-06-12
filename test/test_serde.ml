
open OUnit

open Rfc8032.Internals

let assert_eq_cstruct exp act = assert_equal ~printer:(Fmt.strf "%a" Cstruct.hexdump_pp) exp act
let assert_eq_z exp act = assert_equal ~printer:Z.to_string exp act

type z_of_cstruct_testcase = {data: string; bits: int; expected_result: int}

let z_of_cstruct_testcases = List.map begin fun {data; bits; expected_result} ->
    Fmt.strf "data=%s,bits=%d" data bits >:: fun _ ->
      let buf = Cstruct.of_hex data in
      let actual_result = Serde.z_of_cstruct bits buf in
      assert_eq_z (Z.of_int expected_result) actual_result
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

type cstruct_of_z_testcase = {z:Z.t; bits:int; expected_result:string}

let cstruct_of_z_testcases = List.map begin fun {z; bits; expected_result} ->
    Fmt.strf "z=%a,bits=%d" Z.pp_print z bits >:: fun _ ->
      let expected_result = Cstruct.of_hex expected_result in
      let actual_result = Serde.cstruct_of_z bits z in
      assert_eq_cstruct expected_result actual_result;
      let roundtripped_z = Serde.z_of_cstruct bits actual_result in
      assert_eq_z Z.(z land (one lsl bits - one)) roundtripped_z
  end
    [ {z=Z.(~$255); bits=8; expected_result="ff"}
    (* ; {z=Z.(~$255); bits=7; expected_result="7f"} *)
    ; {z=Z.(one lsl 256 - one); bits=256; expected_result="ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"}
    ; {z=Z.(one lsl 256 - one); bits=16; expected_result="ffff"}
    ; {z=Z.(one lsl 255); bits=256; expected_result="0000000000000000000000000000000000000000000000000000000000000080"}
    ]

type bit_at_testcase = {data: string; index: int; expected_result: int}

let bit_at_testcases = List.map begin fun {data; index; expected_result} ->
    Fmt.strf "data=%s,index=%d" data index >:: fun _ ->
      let buf = Cstruct.of_hex data in
      let actual_result = Serde.bit_at buf index in
      assert_equal ~printer:string_of_int
        expected_result
        actual_result
  end
    [ {data="ff"; index=7; expected_result=1}
    ; {data="ff"; index=6; expected_result=1}
    ; {data="ff"; index=5; expected_result=1}
    ; {data="ff"; index=4; expected_result=1}
    ; {data="ff"; index=3; expected_result=1}
    ; {data="ff"; index=2; expected_result=1}
    ; {data="ff"; index=1; expected_result=1}
    ; {data="ff"; index=0; expected_result=1}
    ; {data="00"; index=7; expected_result=0}
    ; {data="00"; index=6; expected_result=0}
    ; {data="00"; index=5; expected_result=0}
    ; {data="00"; index=4; expected_result=0}
    ; {data="00"; index=3; expected_result=0}
    ; {data="00"; index=2; expected_result=0}
    ; {data="00"; index=1; expected_result=0}
    ; {data="00"; index=0; expected_result=0}
    ; {data="0ff0"; index=15; expected_result=1}
    ; {data="0ff0"; index=14; expected_result=1}
    ; {data="0ff0"; index=13; expected_result=1}
    ; {data="0ff0"; index=12; expected_result=1}
    ; {data="0ff0"; index=11; expected_result=0}
    ; {data="0ff0"; index=10; expected_result=0}
    ; {data="0ff0"; index=9; expected_result=0}
    ; {data="0ff0"; index=8; expected_result=0}
    ; {data="0ff0"; index=7; expected_result=0}
    ; {data="0ff0"; index=6; expected_result=0}
    ; {data="0ff0"; index=5; expected_result=0}
    ; {data="0ff0"; index=4; expected_result=0}
    ; {data="0ff0"; index=3; expected_result=1}
    ; {data="0ff0"; index=2; expected_result=1}
    ; {data="0ff0"; index=1; expected_result=1}
    ; {data="0ff0"; index=0; expected_result=1}
    ]

type set_bit_at_testcase = {data: string; index: int; expected_result: string}

let set_bit_at_testcases = List.map begin fun {data; index; expected_result} ->
    Fmt.strf "data=%s,index=%d" data index >:: fun _ ->
      let buf = Cstruct.of_hex data in
      let _ = Serde.set_bit_at buf index in
      assert_equal ~printer:(Fmt.strf "%a" Cstruct.hexdump_pp)
        (Cstruct.of_hex expected_result)
        buf
  end
    [ {data="ff"; index=7; expected_result="ff"}
    ; {data="ff"; index=6; expected_result="ff"}
    ; {data="ff"; index=5; expected_result="ff"}
    ; {data="ff"; index=4; expected_result="ff"}
    ; {data="ff"; index=3; expected_result="ff"}
    ; {data="ff"; index=2; expected_result="ff"}
    ; {data="ff"; index=1; expected_result="ff"}
    ; {data="ff"; index=0; expected_result="ff"}
    ; {data="00"; index=7; expected_result="80"}
    ; {data="00"; index=6; expected_result="40"}
    ; {data="00"; index=5; expected_result="20"}
    ; {data="00"; index=4; expected_result="10"}
    ; {data="00"; index=3; expected_result="08"}
    ; {data="00"; index=2; expected_result="04"}
    ; {data="00"; index=1; expected_result="02"}
    ; {data="00"; index=0; expected_result="01"}
    ]

let _ =
  "Serialization_suite" >::: [ "z_of_cstruct" >::: z_of_cstruct_testcases
                             ; "cstruct_of_z" >::: cstruct_of_z_testcases
                             ; "bit_at" >::: bit_at_testcases
                             ; "set_bit_at" >::: set_bit_at_testcases ]
  |> run_test_tt_main
