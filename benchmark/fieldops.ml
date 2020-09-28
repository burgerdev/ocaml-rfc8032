
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



let zf =
  let x = Z.(shift_left one 241 - one) in
  fun () -> ZField.(x * x)
let f2 = 
  let x = Z.(shift_left one 241 - one) |> Field25519.of_z in
  fun () -> Field25519.(x *= x)

let wrap f = fun x ->
  f x |> Sys.opaque_identity |> ignore 

let multiplications = [ "Field25519 mult", wrap f2, ()
                      ; "ZField mult", wrap zf, () ]

let _ =
  Benchmark.latencyN Int64.(of_int iterations) multiplications |> Benchmark.tabulate

