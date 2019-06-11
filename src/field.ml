
module Fp(P: sig val p: Z.t end): sig
  type t = Z.t
  val zero: t

  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val double: t -> t

  val one: t

  val ( * ): t -> t -> t
  val ( / ): t -> t -> t
  val square: t -> t
end = struct
  type t = Z.t

  let zero = Z.zero
  let one = Z.one

  let ( + ) x y = Z.erem (Z.add x y) P.p
  let ( * ) x y = Z.erem (Z.mul x y) P.p

  let double x = x + x
  let square x = x * x

  let invert x = Z.invert x P.p

  let ( / ) x y = x * invert y
  let ( - ) x y = Z.(erem (x - y) P.p)
end
