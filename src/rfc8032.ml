module Curve = Curve



module type T = sig
  type private_key
  type public_key
  type signature
  type verification_result = Valid | Invalid

  val private_key_of_cstruct: Cstruct.t -> private_key
  val public_key_of_private_key: private_key -> public_key
  val cstruct_of_signature: signature -> Cstruct.t

  val cstruct_of_public_key: public_key -> Cstruct.t

  (* TODO: These functions can fail, make that part of the signature. *)
  val public_key_of_cstruct: Cstruct.t -> public_key
  val signature_of_cstruct: Cstruct.t -> signature

  val sign: private_key -> Cstruct.t -> signature
  val verify: public_key -> signature -> Cstruct.t -> verification_result
end

(* Stolen from thomas@gazagnaire.org, github.com/mirage/ocaml-hex (ISC license) *)
let invalid_arg fmt =
  Printf.ksprintf (fun str -> raise (Invalid_argument str)) fmt

module Ed25519: T = struct
  module C = Curve.Naive25519

  type private_key = Private of Cstruct.t
  type public_key = Public of C.t
  type signature = Signature of C.t * Z.t
  type verification_result = Valid | Invalid

  let private_key_of_cstruct blob = 
    match Cstruct.len blob with
    | 32 -> Private blob
    | n -> invalid_arg "private key must be exactly 32B, got %dB" n

  let public_key_of_cstruct blob = Public (C.decode blob)

  let cstruct_of_public_key = function Public p -> C.encode p

  let cstruct_of_signature = function Signature(point, num) -> 
    let cs1 = C.encode point in
    let cs2 = Cstruct.create 32 in
    let bits = Z.to_bits num in
    Cstruct.blit_from_string bits 0 cs2 0 @@ String.length bits;
    Cstruct.append cs1 cs2

  let signature_of_cstruct blob = 
    let p = Cstruct.sub blob 0 32 |> C.decode in
    let n = Cstruct.sub blob 32 32 |> Cstruct.to_string |> Z.of_bits in
    Signature(p, n)

  let sanitize_secret blob =
    (* clear 3 least significant bits *)
    Cstruct.set_uint8 blob 0 (Cstruct.get_uint8 blob 0 land 0xf8);
    (* unset most significant bit, set second-to-most significant bit *)
    Cstruct.set_uint8 blob 31 (Cstruct.get_uint8 blob 31 land 0x7f lor 0x40);
    blob

  let dom2 _ _ = Cstruct.empty
  let ph data = data

  let digestv blobs =
    Nocrypto.Hash.SHA512.digestv blobs
  
  let public_key_of_private_key (Private blob) =
    let h = Nocrypto.Hash.SHA512.digest blob in
    let s = Cstruct.sub h 0 32 
            |> sanitize_secret
            |> Cstruct.to_string 
            |> Z.of_bits in
    let a = C.scale s C.base in
    Public a

  let sign (Private blob) data =
    let h = Nocrypto.Hash.SHA512.digest blob in
    let s = Cstruct.sub h 0 32 
            |> sanitize_secret
            |> Cstruct.to_string 
            |> Z.of_bits in
    let s' = C.scale s C.base in
    let prefix = Cstruct.sub h 32 32 in
    let f = 0 in
    let c = Cstruct.empty in
    let r =
      digestv [ dom2 f c
              ; prefix
              ; ph data ]
      |> Cstruct.to_string
      |> Z.of_bits
    in
    let r' = C.scale (Z.erem r C.order) C.base in
    let k =
      digestv [ dom2 f c
              ; C.encode r'
              ; C.encode s'
              ; ph data ]
      |> Cstruct.to_string
      |> Z.of_bits
    in
    Signature (r', Z.(erem (r + k * s) C.order))


  let verify (Public s') (Signature (r', r_plus_ks)) data =
    let f = 0 in
    let c = Cstruct.empty in
    let k =
      digestv [ dom2 f c
              ; C.encode r'
              ; C.encode s'
              ; ph data ]
      |> Cstruct.to_string
      |> Z.of_bits
    in
    let p = C.(scale r_plus_ks base) in
    let q = C.(r' + scale k s') in
    (* TODO: according to RFC, need to check only 8*p = 8*q. *)
    if p = q then Valid else Invalid

end

module Ed448 = struct 
  include Ed25519
  let sign _priv _data = failwith "Ed448.sign: not implemented"
  let verify _pub _sig _data = failwith "Ed448.verify: not implemented"
end
