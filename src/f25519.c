#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "f25519.h"

#define F25519_BASE (0x100000000)
#define F25519_LIMB_SIZE_BITS (32)
#define F25519_LIMB_SIZE_BYTES (4)

const uint32_t p[F25519_NUM_LIMBS] = {0xffffffed, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x7fffffff};
// XXX 
#define F25519_P (p)
const uint32_t r[F25519_NUM_LIMBS] = {0x00000026, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000};
const uint32_t r_prime[F25519_NUM_LIMBS] = {0x9435e50a, 0x435e50d7, 0x35e50d79, 0x5e50d794, 0xe50d7943, 0x50d79435, 0x0d79435e, 0x179435e5};
const uint32_t rr[F25519_NUM_LIMBS] = {0x000005a4, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000};
const uint32_t one[F25519_NUM_LIMBS] = {0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000};
const uint32_t zero[F25519_NUM_LIMBS] = {0};

// Used in redc 
const uint32_t p_prime = 0x286bca1b;
// const uint32_t p_prime[F25519_NUM_LIMBS] = {0x286bca1b, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000};

void encode(const uint32_t x[F25519_NUM_LIMBS], uint8_t out[F25519_NUM_BYTES]) {
  for (int i=0; i<F25519_NUM_LIMBS; i++) {
    uint32_t limb = x[i];
    for (int j=0; j<F25519_LIMB_SIZE_BYTES; j++) {
      out[i*F25519_LIMB_SIZE_BYTES+j] = limb & 0xff;
      limb >>= 8;
    }
  }
}

void decode(const uint8_t in[F25519_NUM_BYTES], uint32_t x[F25519_NUM_LIMBS]) {
  memset(x, 0, F25519_NUM_LIMBS*F25519_LIMB_SIZE_BYTES);
  for (int i=0; i<F25519_NUM_LIMBS; i++) {
    for (int j=0; j < F25519_LIMB_SIZE_BYTES; j++) {
      x[i] |= in[i*F25519_LIMB_SIZE_BYTES+j] << j*8;
    }
  }
}

int comp(const uint32_t x[F25519_NUM_LIMBS], const uint32_t y[F25519_NUM_LIMBS]) {
  for (int i=7; i>=0; i--) {
    if (x[i] > y[i]) return 1;
    if (x[i] < y[i]) return -1;
  }
  // This function could ignore the highest bit, but it's probably safer like this.
  return 0;
}

// Force promotion of 64 bit result type for unsigned arithmetic that would silently overflow.
#define MULT_TO_64(a, b) (((uint64_t) (a)) * (b))
#define ADD_TO_64(a, b) (((uint64_t) (a)) + (b))
#define UPPER_HALF(a) ((uint32_t) ((a) >> 32))
#define LOWER_HALF(a) ((uint32_t) ((a) % F25519_BASE))

// returns the carry
uint32_t add(const uint32_t x[F25519_NUM_LIMBS], const uint32_t y[F25519_NUM_LIMBS], uint32_t out[F25519_NUM_LIMBS]) {
  uint64_t tmp;
  for (int i=0; i<F25519_NUM_LIMBS; i++) {
    tmp = ADD_TO_64(x[i], y[i]);
    out[i] = LOWER_HALF(tmp);
    tmp >>= F25519_LIMB_SIZE_BITS;
  }
  return LOWER_HALF(tmp);
}

void mult(const uint32_t x[F25519_NUM_LIMBS], const uint32_t y[F25519_NUM_LIMBS], uint32_t out[2*F25519_NUM_LIMBS]) {
  // Clear output.
  memset(out, 0, 2*F25519_NUM_LIMBS*F25519_LIMB_SIZE_BYTES);

  uint64_t hi_acc = 0;
  uint64_t lo_acc = 0;
  uint64_t tmp = 0;
  int i, j, n, min_j, max_j;
  for (n=0; n<2*F25519_NUM_LIMBS; n++) {
    min_j = n < F25519_NUM_LIMBS ? 0 : n - F25519_NUM_LIMBS + 1;
    max_j = n < F25519_NUM_LIMBS ? n : F25519_NUM_LIMBS - 1;
    for (j=min_j; j<=max_j; j++) {
      i = n - j;
      tmp = MULT_TO_64(x[i], y[j]);
      lo_acc += LOWER_HALF(tmp);
      hi_acc += UPPER_HALF(tmp);
    }
    out[n] = LOWER_HALF(lo_acc);
    hi_acc += UPPER_HALF(lo_acc);

    // reset for next iteration
    lo_acc = LOWER_HALF(hi_acc);
    hi_acc = UPPER_HALF(hi_acc);
  }
}

void mult_into(const uint32_t x[F25519_NUM_LIMBS], uint32_t y[F25519_NUM_LIMBS]) {
  uint32_t tmp[F25519_NUM_LIMBS];
  mult(x, y, tmp);
  for (int i=0; i<F25519_NUM_LIMBS; i++) {
    y[i] = tmp[i];
  }
}

void redc(const uint32_t t[2*F25519_NUM_LIMBS], uint32_t s[F25519_NUM_LIMBS]) {
  uint32_t T[2*F25519_NUM_LIMBS+1] = {0};
  uint32_t* S = T+F25519_NUM_LIMBS;
  memcpy(T, t, 2*F25519_NUM_BYTES);

  // TODO: this is more or less from Wikipedia, consider doing the carry in one pass as in mult().
  for (int i=0; i<F25519_NUM_LIMBS; i++) {
    uint64_t x = 0;
    uint32_t c = 0;
    uint32_t m = T[i] * p_prime; // This uses the fact that the basis is 2^32 and unsigned multiplication wraps around. Otherwise, we would have to reduce by the base here.
    for (int j=0; j<F25519_NUM_LIMBS; j++) {
      x = T[i+j] + MULT_TO_64(m, F25519_P[j]) + c;
      T[i+j] = LOWER_HALF(x);
      c = UPPER_HALF(x);
    }
    for (int j=F25519_NUM_LIMBS; j<2*F25519_NUM_LIMBS+1-i; j++) {
      x = ADD_TO_64(T[i+j], c);
      T[i+j] = LOWER_HALF(x);
      c = UPPER_HALF(x);
    }
  }

  if (S[F25519_NUM_LIMBS] > 0 || comp(S, F25519_P) >= 0) {
    uint64_t carry = 0;
    for (int i=0; i<F25519_NUM_LIMBS; i++) {
      carry += F25519_P[i];
      carry = S[i] - carry;
      S[i] = LOWER_HALF(carry);
      carry = UPPER_HALF(carry) ? 1 : 0; // detects the wrap-around
    }
  }
  memcpy(s, S, F25519_NUM_BYTES);
}

void pr_num(const char* msg, const uint32_t* x, size_t n) {
  printf("%s: [", msg);
  for (size_t i = 0; i < n; i++) printf("0x%08x%s", x[i], (i+1<n) ? ", " : "]\n");
}

void f25519_decode(const uint8_t in[F25519_NUM_BYTES], uint32_t x[F25519_NUM_LIMBS]) {
  uint32_t tmp[2*F25519_NUM_LIMBS] = {0};
  decode(in, x);
  // TODO: this multiplication could be optimized for the special case that rr fits into a single limb.
  mult(x, rr, tmp);
  redc(tmp, x);
}

void f25519_encode(const uint32_t x[F25519_NUM_LIMBS], uint8_t out[F25519_NUM_BYTES]) {
  uint32_t tmp[2*F25519_NUM_LIMBS] = {0};
  uint32_t y[F25519_NUM_LIMBS] = {0};
  memcpy(tmp, x, F25519_NUM_BYTES);
  redc(tmp, y);
  encode(y, out);
}

void f25519_mul(const uint32_t x[F25519_NUM_LIMBS], const uint32_t y[F25519_NUM_LIMBS], uint32_t dst[F25519_NUM_LIMBS]) {
  uint32_t tmp[2*F25519_NUM_LIMBS] = {0};
  mult(x, y, tmp);
  redc(tmp, dst);
}

void f25519_mul_into(const uint32_t x[F25519_NUM_LIMBS], uint32_t dst[F25519_NUM_LIMBS]) {
  uint32_t tmp[2*F25519_NUM_LIMBS] = {0};
  mult(x, dst, tmp);
  redc(tmp, dst);
}
