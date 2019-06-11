


module type PARAMS = sig
  val p: Z.t
  (** field prime *)

  val b: int
  (** b: public key length in bits. Below implementation requires thisto be a multiple of 8! *)

  val n: int
  (*  n: private scalars have length n+1 (bits) *)

  module H: Nocrypto.Hash.S

  val c: int
  (** cofactor *)

  val d: Z.t
  val a: Z.t

  val base_point: Z.t * Z.t
  val l: Z.t
end

module Params25519: PARAMS = struct
  let p = Z.(one lsl 255 - ~$19)

  let b = 256 (* i.e. 32 bytes *)

  let n = 254
  (*  n: private scalars have length n+1 (bits) *)

  module H = Nocrypto.Hash.SHA512

  let c = 3

  let d = Z.of_string "37095705934669439343138083508754565189542113879843219016388785533085940283555"
  let a = Z.of_int (-1)

  let base_point =
    let x = Z.of_string "15112221349535400772501151409588531511454012693041857206046113283949847762202" in
    let y = Z.of_string "46316835694926478169428394003475163141307993866256225615783033603165251855960" in
    (x, y)

  let l = Z.(one lsl 252 + of_string "27742317777372353535851937790883648493")
end

module Edwards(P: PARAMS) = struct
  module F = Field.Fp(P)

  module G = struct
    open F
    type t = Point of F.t * F.t

    let zero = Point (Z.zero, Z.one)
    let (+) (Point (x, y)) (Point (x', y')) =
      let a = (x * y' + x' * y) / (Z.one + P.d * x * x' * y * y') in
      let b = (y * y' - P.a * x * x') / (Z.one - P.d * x * x' * y * y') in
      Point (a, b)
  end

  module Gops = Group.Ops(G)

  let base = G.Point (fst P.base_point, snd P.base_point)

  let scale p s = Gops.scale_constant_time (P.n + 1) p s

  let decode_point buf =
    let sign = Serde.bit_at buf (P.b - 1) in
    let y = Serde.z_of_cstruct (P.b - 1) buf in
    let yy = F.(y * y) in
    let xx = F.((yy - Z.one)/(P.d * yy + Z.one)) in
    (* TODO the sqrt algorithm is hard-coded for Field25519 *)
    let x = Z.erem (Conversion.sqrt xx) P.p in
    if Z.(x land one |> to_int) != sign then
      (* Sign of x is different from the reconstructed one, negate it. *)
      G.Point (F.(P.p - x), y)
    else
      G.Point (x, y)

  let encode_point (G.Point (x, y)) =
    let s = Z.to_bits y in
    let buf = Cstruct.create (P.b / 8) in
    let () = Cstruct.blit_from_string s 0 buf 0 (String.length s) in
    let () = if Z.(x land one > zero) then
        Serde.set_bit_at buf (P.b - 1)
      else if Serde.bit_at buf (P.b - 1) != 0 then
        failwith "???"
    in
    buf

end

module Edwards25519 = Edwards(Params25519)
