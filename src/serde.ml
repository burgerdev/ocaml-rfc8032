
(* TODO Allocates a new buffer and copies data. *)
let z_of_cstruct num_bits buf =
  (* cut additional data *)
  let rem_bit = num_bits mod 8 in
  let num_bytes = if rem_bit > 0 then num_bits / 8 + 1 else num_bits / 8 in
  let dstbuf = Cstruct.create num_bytes in
  Cstruct.blit buf 0 dstbuf 0 num_bytes;
  let _ = if rem_bit > 0 then
      (* mask upper bits from byte that is consumed partially *)
      Cstruct.get_uint8 dstbuf (num_bytes - 1)
      |> (land) @@ 1 lsl rem_bit - 1
      |> Cstruct.set_uint8 dstbuf (num_bytes - 1)
  in
  (* read Z from LE encoded string *)
  Cstruct.to_string dstbuf |> Z.of_bits

let cstruct_of_z num_bits z =
  let rem_bit = num_bits mod 8 in
  let num_bytes = if rem_bit > 0 then failwith "cstruct_of_z expects bit size to be a multiple of 8" else num_bits / 8 in
  let buf = Cstruct.create num_bytes in
  let s = Z.to_bits z in
  let n = min (String.length s) num_bytes in
  Cstruct.blit_from_string s 0 buf 0 n;
  buf

let bit_at buf i =
  let byte_index = i / 8 in
  let bit_index = i mod 8 in
  let byte = Cstruct.get_uint8 buf byte_index in
  byte asr bit_index land 1

let set_bit_at buf i =
  let byte_index = i / 8 in
  let bit_index = i mod 8 in
  let byte = Cstruct.get_uint8 buf byte_index in
  let mask = 1 lsl bit_index in
  Cstruct.set_uint8 buf byte_index (byte lor mask)
