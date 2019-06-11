open OUnit

open Rfc8032.Internals

let rec range ?start:(start=0) = function
  | n when n < start -> []
  | n -> n - 1 :: range ~start (n - 1)

let pp_point =
  let open Params.Edwards25519 in
  Fmt.pair ~sep:Fmt.(const string ", ") Z.pp_print Z.pp_print
  |> Fmt.using @@ begin function G.Point (x, y) -> (x, y) end
  |> Fmt.parens
  |> Fmt.prefix Fmt.(const string "Point")

let string_of_point =
  Fmt.strf "%a" pp_point

let assert_eq_point x y = assert_equal ~printer:string_of_point x y
let assert_eq_z x y = assert_equal ~printer:(Fmt.strf "%a" Z.pp_print) x y

let codec_testcases = List.map begin fun scale ->
    Fmt.strf "scale=%d" scale >:: fun _ ->
      let s = Z.of_int scale in
      let open Params.Edwards25519 in
      let orig_point = scale base s in
      let orig_enc = encode_point orig_point in
      assert_equal ~printer:string_of_int 32 (Cstruct.len orig_enc);
      let roundtripped_point = decode_point orig_enc in
      let roundtripped_enc = encode_point roundtripped_point in
      assert_equal ~msg:"encode-decode" ~printer:(Fmt.strf "%a" pp_point)
        orig_point
        roundtripped_point;
      assert_equal ~msg:"decode-encode" ~printer:(Fmt.strf "%a" Cstruct.hexdump_pp)
        orig_enc
        roundtripped_enc
  end @@ range 500

let basic_math_testcases =
  let open Params.Edwards25519 in
  [ "0 * p" >:: begin fun _ ->
        assert_eq_point (G.zero) (scale base Z.zero)
      end
  ; "1 * p" >:: begin fun _ ->
      assert_eq_point base (scale base Z.one)
    end
  ; "0 + 0" >:: begin fun _ ->
      assert_eq_point G.zero G.(zero + zero)
    end
  ; "0 + 1" >:: begin fun _ ->
      assert_eq_point base G.(zero + base)
    end
  ; "1 + 0" >:: begin fun _ ->
      assert_eq_point base G.(base + zero)
    end
  ; "F: 5 / 1" >:: begin fun _ ->
      let five = Z.(~$5) in
      assert_eq_z five F.(five / Z.one)
    end
  ]

let _ =
  "Parameters_suite" >::: [ "codec" >::: codec_testcases
                          ; "basic_math" >::: basic_math_testcases]
  |> run_test_tt_main
