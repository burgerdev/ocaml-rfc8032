
open Bigarray

type data = (char, int8_unsigned_elt, c_layout) Array1.t
type elem = (char, int8_unsigned_elt, c_layout) Array1.t

external c_decode: data -> elem -> unit = "caml_f25519_decode" [@@noalloc]
external c_encode: elem -> data -> unit = "caml_f25519_encode" [@@noalloc]
external c_mul: elem -> elem -> elem -> unit = "caml_f25519_mul" [@@noalloc]
external c_mul_into: elem -> elem -> unit = "caml_f25519_mul_into" [@@noalloc]

type t = elem

let of_cstruct data =
  let t = Cstruct.create 32 |> Cstruct.to_bigarray in
  c_decode (Cstruct.to_bigarray data) t;
  t

let to_cstruct t =
  let data = Cstruct.create 32 in
  c_encode t (Cstruct.to_bigarray data);
  data

let of_z z =
  let bits = Z.to_bits z in
  let data = Cstruct.create 32 in
  let n = min (String.length bits) 32 in
  Cstruct.blit_from_string bits 0 data 0 n;
  of_cstruct data

let to_z t =
  to_cstruct t
  |> Cstruct.to_string
  |> Z.of_bits

let ( * ) x y =
  let t = Cstruct.create 32 |> Cstruct.to_bigarray in
  c_mul x y t;
  t

let ( *= ) x y =
  c_mul_into x y
