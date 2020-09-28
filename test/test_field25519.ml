open OUnit


module ZField = struct
  type t = Z.t
  
  let of_z z = z
  let to_z z = z
  let of_cstruct data = data
  let to_cstruct t = t

  let p = Z.((shift_left one 255) - ~$19) 
  let num_bits = 255

  let zero = Z.zero
  let one = Z.one
  let two = Z.of_int 2
  let a24 = Z.of_int 121665

  let ( + ) x y = Z.erem (Z.add x y) p
  let ( * ) x y = Z.erem (Z.mul x y) p

  let invert x = Z.invert x p

  let ( / ) x y = x * invert y
  let ( - ) x y = Z.(erem (x - y) p)

  let sqrt x2 = 
    let x = Z.(powm x2 ((p + ~$3) / ~$8) p) in
    let x2' = x * x in
    if x2' = x2 then 
      x
    else
      Z.(powm ~$2 ((p - ~$1) / ~$4) p) * x
      (* TODO: should fail if not a sqrt! *)
end

let fail fmt = Format.ksprintf failwith fmt

let iterations = 1000000

(* 1M operations with no-op Field25519: 450ms *)
(* 1M operations with both Field25519: 900ms *)
(* 1M operations with OCaml Montgomery redc and Zarith else: 1.150s *)
(* 1M operations with custom C implementation of Field25519: 1.90s *)

module F2 = Field25519.Donna

let mult_mod_p _ =
  let rec aux x x' = function
    | n when n = 0 -> x, x'
    | n -> 
      let xx = ZField.(x * x) in
      let xx' = F2.(x' * x') in
      aux xx xx' (n-1)
  in 
  let x = Z.(shift_left one 241) in
  let x' = F2.of_z x in
  let y, y' = aux x x' iterations in
  let y' = F2.to_z y' in
  assert_equal ~printer:Z.to_string ~msg:"after XXX iterations" y y'

let encode_decode _ = 
  let hex = "ffffffffffffffff0000000000000000000000000000000000000000ffff0000" in
  let data = Hex.to_cstruct @@ `Hex hex in
  let elem = F2.of_cstruct data in
  let data' = F2.to_cstruct elem in
  let `Hex hex' = Hex.of_cstruct data' in
  assert_equal ~printer:(fun x -> x) ~msg:"encode(decode(x)) is not x" hex hex'


let _ =
  "Field25519_Suite" >::: [ "encode_decode" >:: encode_decode
                          ; "mult_mod_p" >:: mult_mod_p ]
  |> run_test_tt_main
