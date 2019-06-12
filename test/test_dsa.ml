open OUnit

let assert_eq_cstruct exp act = assert_equal ~printer:(Fmt.strf "%a" Cstruct.hexdump_pp) exp act

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
  assert_eq_cstruct expected_public_key actual_public_key

let test_signature _ =
  let open Rfc8032.Internals.Dsa.EdDSA25519 in
  let expected_signature = Cstruct.of_hex "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b" in
  let secret_key = Cstruct.of_hex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60" in
  let actual_signature = sign (private_key_of_cstruct secret_key) Cstruct.empty |> encode_signature in
  assert_eq_cstruct expected_signature actual_signature

let _ =
  "Dsa_suite" >::: [ "private_to_public" >:: test_private_to_public
                   ; "test_signature" >:: test_signature
                   ]
  |> run_test_tt_main
