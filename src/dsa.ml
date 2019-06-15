
module EdDSA25519 = struct
  (* TODO: make this a functor argument *)
  module E = Params.Edwards25519

  open E

  type private_key = Private_key of Z.t * Cstruct.t
  type public_key = Public_key of G.t
  type signature = Signature of G.t * Z.t

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

  let sign (Private_key (s, prefix)) msg =
    (* Section 3.3 *)
    let r = P.H.digestv [prefix; msg] |> Serde.z_of_cstruct (2*P.b) in
    let r' = scale base r in
    let a = scale base s in

    let h = P.H.digestv [encode_point r'; encode_point a; msg] |> Serde.z_of_cstruct (2*P.b) in

    Signature (r', Z.(erem (r + h * s) P.l))

  let signature_of_cstruct buf =
    let n = P.b / 8 in
    if Cstruct.len buf < 2*n then
      raise (Invalid_argument "signature_of_cstruct: not enough data")
    else
      let r = Cstruct.sub buf 0 n |> decode_point in
      let s = Cstruct.sub buf n n |> Serde.z_of_cstruct P.b in
      Signature (r, s)

  let verify (Public_key a) (Signature (r, s)) msg =
    (* Section 3.4 *)
    (* TODO: s must be contained in {0, ..., L-1}! *)
    let h = P.H.digestv [encode_point r; encode_point a; msg] |> Serde.z_of_cstruct (2*P.b) in
    let cofactor = Z.(one lsl P.c) in
    let lhs = scale base s in
    let lhs = scale lhs cofactor in
    let rhs = scale a h in
    let rhs = G.(rhs + r) in
    let rhs = scale rhs cofactor in
    lhs = rhs

  let encode_signature = function Signature (r, s) ->
    let r = encode_point r in
    let s = Serde.cstruct_of_z P.b s in
    Cstruct.concat [r; s]

end
