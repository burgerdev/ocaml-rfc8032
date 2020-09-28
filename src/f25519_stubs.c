
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

int curve25519_donna(uint8_t *mypublic, const uint8_t *secret, const uint8_t *basepoint);
CAMLprim value caml_curve25519_donna(value ba_res, value ba_key, value ba_base) {
  curve25519_donna(Caml_ba_data_val(ba_res), Caml_ba_data_val(ba_key), Caml_ba_data_val(ba_base));
  return Val_unit;
}

typedef uint8_t u8;
typedef int32_t s32;
typedef int64_t limb;

void fexpand(limb *output, const u8 *input);
CAMLprim value caml_fexpand(value ba_out, value ba_in) {
  fexpand(Caml_ba_data_val(ba_out), Caml_ba_data_val(ba_in));
  return Val_unit;
}

void fcontract(u8 *output, limb *input_limbs);
CAMLprim value caml_fcontract(value ba_out, value ba_in) {
  fcontract(Caml_ba_data_val(ba_out), Caml_ba_data_val(ba_in));
  return Val_unit;
}

void fsquare(limb *output, const limb *in);

void fmul(limb *output, const limb *in, const limb *in2);
CAMLprim value caml_fmul(value ba_res, value ba_1, value ba_2) {
  fmul(Caml_ba_data_val(ba_res), Caml_ba_data_val(ba_1), Caml_ba_data_val(ba_2));
  return Val_unit;
}

void freduce_coefficients(limb *output);
void freduce_degree(limb *output);
void fsum(limb *output, const limb *in);
void fdifference(limb *output, const limb *in);
void fscalar_product(limb *output, const limb *in, const limb scalar);
void fproduct(limb *output, const limb *in2, const limb *in);


