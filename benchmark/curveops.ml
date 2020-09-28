
module C = Rfc8032.Curve.Naive25519

let naive =
  let z = Z.(shift_left one 241) in
  fun _ -> C.scale z C.base

let donna =
  let x = Cstruct.create 32 in
  fun _ -> Rfc8032.Curve.Donna.(curve25519 x base base)

let hacl =
  let open Hacl_x25519 in
  let x = Cstruct.create 32 in
  Cstruct.set_char x 0 'f';
  let priv, pub = gen_key ~rng:(fun _ -> x) in
  fun _ -> key_exchange priv pub
  
let iterations = 1000

let wrap f = fun x ->
  f x |> Sys.opaque_identity |> ignore 

let curveops = [ "Naive25519 curve op", wrap naive, ()
               ; "curve25519-donna op", wrap donna, ()
               ; "hacl_x25519 op", wrap hacl, () ]

let _ =
  Benchmark.latencyN Int64.(of_int iterations) curveops |> Benchmark.tabulate

