open OUnit

type algorithm =
  | Ed25519
  | Ed25519ph
  | Ed25519ctx
  | Ed448
  | Ed448ph

type testcase = { algorithm: algorithm
                ; secret_key_hex: string
                ; public_key_hex: string
                ; message_hex: string
                ; signature_hex: string
                }

let execute = function {algorithm; secret_key_hex; public_key_hex; message_hex; signature_hex} ->
  let m: (module Rfc8032.T) = match algorithm with
    | Ed25519 -> (module Rfc8032.Ed25519)
    | Ed448 -> (module Rfc8032.Ed448)
    | _ -> failwith "test function not implemented"
  in
  let module M = (val m) in
  let sig_out =
    M.sign (M.private_key_of_hex secret_key_hex) (M.data_of_hex message_hex)
    |> M.hex_of_signature
  in
  assert_equal signature_hex sig_out;
  let result = M.verify (M.public_key_of_hex public_key_hex) (M.signature_of_hex signature_hex) (M.data_of_hex message_hex) in
  assert_equal Rfc8032.Valid result


let testcase (name, case) =
  name >:: fun _ -> execute case


let tests = (*
  (* 7.1 *)
  [ "blank_ed25519", { algorithm=Ed25519
                     ; secret_key_hex="9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
                     ; public_key_hex="d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
                     ; message_hex=""
                     ; signature_hex="e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
                     }

  (* 7.4 *)
  ; "blank_ed448", { algorithm=Ed448
                   ; secret_key_hex="6c82a562cb808d10d632be89c8513ebf6c929f34ddfa8c9f63c9960ef6e348a3528c8a3fcc2f044e39a3fc5b94492f8f032e7549a20098f95b"
                   ; public_key_hex="5fd7449b59b461fd2ce787ec616ad46a1da1342485a70e1f8a0ea75d80e96778edf124769b46c7061bd6783df1e50f6cd1fa1abeafe8256180"
                   ; message_hex=""
                   ; signature_hex="533a37f6bbe457251f023c0d88f976ae2dfb504a843e34d2074fd823d41a591f2b233f034f628281f2fd7a22ddd47d7828c59bd0a21bfd3980ff0d2028d4b18a9df63e006c5d1c2d345b925d8dc00b4104852db99ac5c7cdda8530a113a0f4dbb61149f05a7363268c71d95808ff2e652600"
                   }
  ]
*) []

let _ =
  "Rfc8032_Suite" >::: List.map testcase tests
  |> run_test_tt_main
