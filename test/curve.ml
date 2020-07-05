open Rfc8032.Curve.Naive25519

let equal (Point (x1, y1)) (Point (x2, y2)) = 
  x1 = x2 && y1 = y2

let pp = fun ppf -> function Point (x, y) -> 
  Format.fprintf ppf "(%a, %a)" Z.pp_print x Z.pp_print y

(* Group laws *)
let () =
  assert (equal zero zero);
  assert (not (equal zero base));
  assert (equal base (base + zero));
  assert (equal base (zero + base));
  assert (not (equal base (base + base)));
  ()

(* Edwards specials *)
let () =
  let order = Z.(shift_left one 252 + of_string "27742317777372353535851937790883648493") in
  assert (equal zero (scale order base))

(* Random assertions *)
let () =
  let five = base + base + base + base + base in
  let five' = scale Z.(~$5) base in
  assert (equal five five')

let range n =
  let rec aux k acc =
    if k < 0 then acc else aux (k-1) (k :: acc)
  in aux n []

(* Codec *)
let () =
  range 100
  |> List.map Z.of_int
  |> List.iter @@ fun n ->
  let p = scale n base in
  let p' = encode p |> decode in
  assert (equal p p')
