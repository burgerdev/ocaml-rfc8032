
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
  val hex_of_private_key: private_key -> string
  val hex_of_public_key: public_key -> string
  val signature_of_hex: string -> signature

  val public_key_of_private_key: private_key -> public_key
  val sign: private_key -> data -> signature
  val verify: public_key -> signature -> data -> verification_result
end



module Ed448: T = struct

  type private_key = string
  type public_key = string
  type signature = string
  type data = string

  let private_key_of_hex hex = hex
  let public_key_of_hex hex = hex
  let data_of_hex hex = hex
  let hex_of_signature signature = signature
  let hex_of_private_key k = k
  let hex_of_public_key k = k
  let signature_of_hex hex = hex

  let public_key_of_private_key k = k
  let sign _priv _data = failwith "not implemented"
  let verify _pub _signature _data = failwith "not implemented"
end

module Ed25519: T = Simple.SimpleEd25519
