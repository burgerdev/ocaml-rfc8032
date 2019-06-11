(* Problem: RFC7748 specifies curve arithmetic in (u, v) coordinates, while
   RFC8032 uses Twisted Edwards parametrization. This module converts between
   the two. See https://tools.ietf.org/html/rfc7748#section-4.1. *)

(* Montgomery: v^2 = u^3 + A*u^2 + u *)
(* Edwards: -x^2 + y^2 = 1 + d*x^2*y^2 *)

let p = Z.(one lsl 255 - ~$19)

(* Euler's criterion for detecting squares *)
let is_sqrt =
  let p_minus_1_half = Z.((p - one) / ~$2) in
  fun xx ->
    Z.(powm xx p_minus_1_half p = one)

let sqrt =
  let i = Z.(powm ~$2 ((p - one) / ~$4) p) in
  let p_plus_3_by_8 = Z.((p + ~$3) / ~$8) in
  fun xx ->
    let x = Z.(powm xx p_plus_3_by_8 p) in
    let xx' = Z.(erem (x * x) p) in
    if xx' = xx then
      x
    else
      Z.(i * x)
        (*
        if Z.(p - xx') = xx then
          Z.(i * x)
        else
          raise (Invalid_argument "") *)


(* Montgomery notation omits v, *)
let v_of_u =
  let a = Z.(~$ 486662) in
  fun u ->
    let u2 = Z.(u * u) in
    let u3 = Z.(u2 * u) in
    let v2 = Z.(u3 + a*u2 + u) in
    let v = sqrt v2 in
    v
