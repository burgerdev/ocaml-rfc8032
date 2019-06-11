open OUnit

let test_private_to_public _ =
  let open Rfc8032.Internals.Dsa.EdDSA25519 in
  let secret_key = Cstruct.of_hex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60" in
  let expected_public_key = Cstruct.of_hex "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a" in
  let actual_public_key =
    secret_key
    |> private_key_of_cstruct
    |> public_key_of_private_key
    |> cstruct_of_public_key
  in
  assert_equal ~printer:(Fmt.strf "%a" Cstruct.hexdump_pp) expected_public_key actual_public_key

let _ =
  "Dsa_suite" >::: [ "private_to_public" >:: test_private_to_public ]
  |> run_test_tt_main
