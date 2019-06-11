
module EdDSA25519 = struct
  (* TODO: make this a functor argument *)
  module E = Params.Edwards25519

  open E

  type private_key = Private_key of Z.t * Cstruct.t
  type public_key = Public_key of G.t

  let decode_scalar =
    let mask = Z.(one lsl P.c - one |> lognot) in
    let addend = Z.(one lsl P.n) in
    fun buf ->
      Serde.z_of_cstruct P.n buf
      |> Z.add addend
      |> Z.logand mask

  let cstruct_of_public_key = function Public_key a ->
    encode_point a

  let private_key_of_cstruct buf =
    let n = P.b / 8 in
    if Cstruct.len buf < n then
      raise (Invalid_argument "private key is too short")
    else
      let buf = Cstruct.sub buf 0 n in
      let digest = P.H.digest buf in
      (* decode_scalar limits itself to the first P.n bytes *)
      let s = decode_scalar digest in
      Private_key (s, Cstruct.sub digest n n)

  let public_key_of_private_key = function Private_key (s, _) ->
    let a = scale base s in
    Public_key a

end
