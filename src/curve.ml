
module type Curve = sig
  type t
  type data

  val zero: t
  val base: t

  val encode: t -> data
  val decode: data -> t

  val (+): t -> t -> t
  val scale: Z.t -> t -> t
end

module F = struct
  type t = Z.t

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

module Naive25519 = struct

  type t = Point of Z.t * Z.t

  let order = Z.(shift_left one 252 + of_string "27742317777372353535851937790883648493")

  let zero = Z.(Point (zero, one))
  let base = Z.(Point (of_string "15112221349535400772501151409588531511454012693041857206046113283949847762202", of_string "46316835694926478169428394003475163141307993866256225615783033603165251855960"))

  let bit z n = Z.((z asr n) mod F.two)

  let cswap cond a b =
    let c = Z.(rem cond F.two) in
    let c' = Z.(one - c) in
    let a' = Z.(c'*a + c*b) in
    let b' = Z.(c'*b + c*a) in
    a', b'

  let d = Z.of_string "37095705934669439343138083508754565189542113879843219016388785533085940283555"
  let a = Z.(~- one)

  let ( + ) (Point (x1, y1)) (Point (x2, y2)) = 
    let open F in
    let x1y2 = x1 * y2 in
    let x2y1 = x2 * y1 in
    let y12 = y1 * y2 in
    let x12 = x1 * x2 in
    let denom = d * x12 * y12 in
    let x3 = (x1y2 + x2y1) / (one + denom) in 
    let y3 = (y12 - a * x12) / (one - denom) in 
    Point (x3, y3)

  let scale n p = 
    (* TODO: make this constant time! *)
    let rec aux p a b = function
      | n when Z.(n = zero) -> zero
      | n when Z.(n = one) -> a + b
      | n when Z.(rem n F.two = zero) -> aux p (a + a) b Z.(n / F.two)
      | n -> aux p a (a + b) Z.(n - one)
    in aux p p zero n

  type data = Cstruct.t

(*
   An element (x,y) of E is encoded as a b-bit string called ENC(x,y),
   which is the (b-1)-bit encoding of y concatenated with one bit that
   is 1 if x is negative and 0 if x is not negative.

   The encoding of GF(p) is used to define "negative" elements of GF(p):
   specifically, x is negative if the (b-1)-bit encoding of x is
   lexicographically larger than the (b-1)-bit encoding of -x.
*)
  let encode = function Point(x, y) ->
    (* figure out "sign" of x *)
    let maybe_high_bit = 
    (* TODO: use mutable cstruct operations instead. *)
      if Z.testbit x 0 then
        Z.(one lsl 255)
      else
        Z.zero
    in
    let buf = Cstruct.create 32 in
    let bits = Z.(y lor maybe_high_bit |> to_bits) in
    Cstruct.blit_from_string bits 0 buf 0 @@ String.length bits;
    buf
  let decode buf = 
    let s = Cstruct.to_string buf in
    let y = Z.of_bits s in
    let x_should_be_negative = Z.testbit y 255 in
    let y = Z.(one lsl 255 |> lognot |> logand y) in
    (* Need to fail if y >= 2^255 - 19? *)
    let y2 = F.(y * y) in
    let x2 = F.((one - y2) / (a - d * y2)) in
    let x = F.sqrt x2 in
    let x' = F.(zero - x) in
    (* Rearrange so that x is "negative" and x' is positive. *)
    let x, x' = 
      if Z.testbit x 0 then 
        x, x'
      else
        x', x
    in
    if x_should_be_negative then
      Point(x, y)
    else
      Point(x', y)
end

module Curve25519: Curve = Naive25519
