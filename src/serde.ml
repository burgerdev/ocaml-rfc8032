
(* TODO DANGER: destructs the given buffer! *)
let z_of_cstruct num_bits buf =
  (* cut additional data *)
  let rem_bit = num_bits mod 8 in
  let num_bytes = if rem_bit > 0 then num_bits / 8 + 1 else num_bits / 8 in
  let buf = Cstruct.sub buf 0 num_bytes in
  let _ = if rem_bit > 0 then
      (* mask upper bits from byte that is consumed partially *)
      Cstruct.get_uint8 buf (num_bytes - 1)
      |> (land) @@ 1 lsl rem_bit - 1
      |> Cstruct.set_uint8 buf (num_bytes - 1)
  in
  (* read Z from LE encoded string *)
  Cstruct.to_string buf |> Z.of_bits

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
