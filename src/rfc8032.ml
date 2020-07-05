module Curve = Curve

type verification_result = Valid | Invalid

module type T = sig
  type private_key
  type public_key
  type signature

  type data

  val private_key_of_hex: string -> private_key
  val public_key_of_hex: string -> public_key
  val data_of_hex: string -> data
  val hex_of_signature: signature -> string
  val signature_of_hex: string -> signature

  val sign: private_key -> data -> signature
  val verify: public_key -> signature -> data -> verification_result
end


module Ed25519: T with type data = Cstruct.t = struct
  module C = Curve.Naive25519
  type private_key = Z.t
  type public_key = C.t
  type signature = Signature of string (* TODO!! *)
  type data = Cstruct.t

  let private_key_of_hex hex = 
    Hex.to_string (`Hex hex)
    |> Z.of_bits
    (* TODO: Do we need to reduce by p? *)

  let public_key_of_hex hex =
    Hex.to_cstruct (`Hex hex)
    |> C.decode
  let data_of_hex hex =
    Hex.to_cstruct (`Hex hex)
  let hex_of_signature = function Signature signature -> signature
  let signature_of_hex hex = Signature hex

  let sign _priv _data = failwith "not implemented"
  let verify _pub _signature _data = failwith "not implemented"
end

module Ed448 = Ed25519
