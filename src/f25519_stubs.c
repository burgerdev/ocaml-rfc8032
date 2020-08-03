
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#include "f25519.h"

CAMLprim value caml_f25519_decode(value src, value dst) {
  f25519_decode(Caml_ba_data_val(src), Caml_ba_data_val(dst));
  return Val_unit;
}

CAMLprim value caml_f25519_encode(value src, value dst) {
  f25519_encode(Caml_ba_data_val(src), Caml_ba_data_val(dst));
  return Val_unit;
}

CAMLprim value caml_f25519_mul(value x, value y, value dst) {
  f25519_mul(Caml_ba_data_val(x), Caml_ba_data_val(y), Caml_ba_data_val(dst));
  return Val_unit;
}

CAMLprim value caml_f25519_mul_into(value x, value dst) {
  f25519_mul_into(Caml_ba_data_val(x), Caml_ba_data_val(dst));
  return Val_unit;
}
