
module type Group = sig
  type t

  val base: t
  val add: t -> t -> t
  val scale: Z.t -> t -> t

  val of_cstruct: Cstruct.t -> t
  val to_cstruct: t -> Cstruct.t

  val field_prime: Z.t
end

module H = Nocrypto.Hash.SHA512

module Conv = struct
  let z_of_cstruct cstruct = failwith "no z_of_cstruct"
  let cstruct_of_z z = failwith "no cstruct_of_z"
end

module DSA(G: Group)(H: Nocrypto.Hash.S) = struct
  type public_key = Public_key of G.t
  type private_key = Private_key of Z.t
  type signature = Signature of G.t * Z.t

  let hash pub_a pub_b msg =
    H.digestv [G.to_cstruct pub_a; G.to_cstruct pub_b; msg]
    |> Conv.z_of_cstruct

  let derive_key secret_key msg = failwith "no kdf"

  let sign (Private_key private_key) msg =
    (* TODO: consider incremental construction instead of requiring the complete
             message up front *)
    let session_key = derive_key private_key msg in
    let session_pub = G.(scale session_key base) in
    let h = hash session_pub G.(scale private_key base) msg in
    let s = Z.erem Z.(session_key + h * private_key) G.field_prime in
    Signature (session_pub, s)

  let verify public_key signature msg =
    failwith "no verify"

end
