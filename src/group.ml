

module type ADDITIVE_GROUP = sig
  type t
  val zero: t
  val (+): t -> t -> t
end

module Ops(G: ADDITIVE_GROUP): sig
  val scale_constant_time: int -> G.t -> Z.t -> G.t
end = struct
  open G

  let cswap z bit a b =
    (* G.constant_time_conditional_swap Z.((z asr bit) land one) a b
         TODO *)
    if Z.testbit z bit then (b, a) else (a, b)

  (* Montgomery ladder with constant number of operations and constant cache
     access pattern. *)
  let scale_constant_time num_bits element n =
    let rec aux r0 r1 = function
      | -1 -> r0
      | bit ->
        let (r0, r1) = cswap n bit r0 r1 in
        let a = r0 + r1 in
        let d = r0 + r0 in
        let (r0, r1) = cswap n bit d a in
        aux r0 r1 (bit - 1)
    in
    let r0 = zero in
    let r1 = element in
    aux r0 r1 num_bits
end
