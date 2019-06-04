
module Make_hash(H: Nocrypto.Hash.S) = struct
  type t = H.t option

  let empty = None
  let rec (>>) t d = match t with
    | Some t -> begin H.feed t d; Some t end
    | None -> Some (H.init ()) >> d
  let get = function
    | Some t -> H.get t
    | None -> H.init () |> H.get

  let digest = H.digest
  let digestv = H.digestv
end

module H = Make_hash(Nocrypto.Hash.SHA512)

let split_cstruct b =
  let n = Cstruct.len b in
  let half = n / 2 in
  let left = Cstruct.sub b 0 half in
  let right = Cstruct.sub b half half in
  (left, right)

let split_string s =
  let n = String.length s in
  let half = n / 2 in
  let left = String.sub s 0 half in
  let right = String.sub s half half in
  (left, right)

let hex_of_cstruct size c =
  let n = 2 * Cstruct.len c in
  let bufsize = 2 * size in
  let buf = Buffer.create bufsize in
  Cstruct.hexdump_to_buffer buf c;
  for _ = n to bufsize - 1 do
    Buffer.add_char buf '0'
  done;
  Buffer.contents buf

let z_of_cstruct c =
  let s = Cstruct.to_string c in
  Z.of_bits s

let z_of_hex hex =
  let n = String.length hex / 2 in
  let buf = Bytes.create n in
  let ic = Scanf.Scanning.from_string hex in
  for i = 0 to (n - 1) do
    Bytes.set buf i @@ Scanf.bscanf ic "%02x" char_of_int
  done;
  Bytes.unsafe_to_string buf
  |> Z.of_bits

let hex_of_z n z =
  let num_hex = 2 * n in
  let upper_bound = n - 1 in
  let src = Z.format ("%0" ^ string_of_int num_hex ^ "x") z in
  let dst = Bytes.create num_hex in
  for i = 0 to upper_bound do
    Bytes.blit_string src (2*i) dst (2*(upper_bound-i)) 2
  done;
  (* This is ok because we created the string and will forget it after returning. *)
  Bytes.unsafe_to_string dst


module SimpleEd25519 = struct
  open Rfc7748

  let q = Z.(one lsl 255 - ~$19)

  type private_key = Cstruct.t
  type public_key = X25519.public_key
  type signature = X25519.public_key * X25519.private_key
  type data = Cstruct.t

  let private_key_of_hex = Cstruct.of_hex
  let public_key_of_hex = X25519.public_key_of_string

let hex_of_private_key priv = hex_of_cstruct 32 priv
let hex_of_public_key pub = X25519.string_of_public_key pub

  let data_of_hex = Cstruct.of_hex

  let signature_of_hex: string -> signature = fun hex ->
    let l = String.length hex in
    let half = l / 2 in
    let left = String.sub hex 0 half in
    let right = String.sub hex half l in
    (X25519.public_key_of_string left, X25519.private_key_of_string right)

  let hex_of_signature: signature -> string = fun (left, right) ->
    X25519.string_of_public_key left ^ X25519.string_of_private_key right

  let public_key_of_private_key priv =
    let s = H.digest priv
            |> split_cstruct
            |> fst
            |> z_of_cstruct in
    hex_of_z 32 s
    |> X25519.private_key_of_string
    |> X25519.public_key_of_private_key

  let sign priv data =
    let (lower, upper) = H.digest priv |> split_cstruct in

    let r = H.digestv [upper; data]
            |> z_of_cstruct in
    let r' = hex_of_z 32 r
             |> X25519.private_key_of_string
             |> X25519.public_key_of_private_key in

    (* TODO sanitize properly *)
    let s = z_of_cstruct lower in
    let a = hex_of_z 32 r
            |> X25519.private_key_of_string
            |> X25519.public_key_of_private_key
            |> X25519.string_of_public_key
            |> Cstruct.of_hex in
    let h = H.digestv [X25519.string_of_public_key r' |> Cstruct.of_hex
                      ; a
                      ; data]
            |> z_of_cstruct in
    let s' = Z.(r + h * s) in
    let s' = Z.erem s' q
             |> hex_of_z 32
             |> X25519.private_key_of_string in
    (r', s')

  let verify _pub _signature _data = failwith "not implemented"
end
