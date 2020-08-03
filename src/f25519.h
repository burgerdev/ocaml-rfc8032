#include <stdint.h>

#ifndef _F25519_H
#define _F25519_H

#define F25519_NUM_LIMBS (8)
#define F25519_NUM_BYTES (32)

extern const uint32_t f25519_p[F25519_NUM_LIMBS];
extern const uint32_t f25519_p[F25519_NUM_LIMBS];

// TODO: doc
void f25519_encode(const uint32_t x[F25519_NUM_LIMBS], uint8_t out[F25519_NUM_BYTES]);
void f25519_decode(const uint8_t in[F25519_NUM_BYTES], uint32_t x[F25519_NUM_LIMBS]);

// comp(x, y) >= 0 <=> x >= y
// comp(x, y) == comp(y, x)
int f25519_comp(const uint32_t x[F25519_NUM_LIMBS], const uint32_t y[F25519_NUM_LIMBS]);
void f25519_add(const uint32_t x[F25519_NUM_LIMBS], const uint32_t y[F25519_NUM_LIMBS], uint32_t out[F25519_NUM_LIMBS]);
void f25519_mul(const uint32_t x[F25519_NUM_LIMBS], const uint32_t y[F25519_NUM_LIMBS], uint32_t out[F25519_NUM_LIMBS]);
void f25519_mul_into(const uint32_t x[F25519_NUM_LIMBS], uint32_t out[F25519_NUM_LIMBS]);


#endif
