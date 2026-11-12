/* Basic operations on Rounded values, each in a single unsafe foreign call.

   A Rounded value is the four fields of an mpfr_t: precision, sign, exponent and
   a ByteArray# holding the limbs.  The functions here take those fields directly,
   build the mpfr_t structs on the C stack around the limb arrays that GHC passes
   in, and call the same MPFR function, with the same precisions and rounding
   mode, as the safe code path in Numeric.Rounded.Internal (binary, binary',
   unary'' and cmp).  A result is written into a fresh limb array allocated by
   the caller; its sign is stored in *sign and its exponent is returned.

   See "Basic operations in one unsafe call" in Numeric.Rounded.Internal for why
   passing unpinned GHC arrays here is sound. */

#include <string.h>
#include <mpfr.h>

/* A read-only mpfr_t view of a Rounded value.  MPFR never writes to an input
   that is not also the output, and never reallocates it. */
static void view(mpfr_ptr x, mpfr_prec_t prec, mpfr_sign_t sign, mpfr_exp_t exp,
                 const mp_limb_t *limbs)
{
  x->_mpfr_prec = prec;
  x->_mpfr_sign = sign;
  x->_mpfr_exp = exp;
  x->_mpfr_d = (mp_limb_t *) limbs;
}

/* The output starts with sign and exponent 0, as in the safe path (out_'), so
   that a NaN result, whose sign MPFR leaves as it was, has the same fields.
   For a zero, infinite or NaN result MPFR doesn't touch the limbs; we zero them
   so that the bytes of the result don't depend on what the fresh array held. */
#define RESULT(c, prec, limbs)  view(c, prec, 0, 0, limbs)
#define FINISH(c, prec, limbs, sign)                                            \
  do {                                                                          \
    if (!mpfr_regular_p(c))                                                     \
      memset(limbs, 0, ((prec) + GMP_NUMB_BITS - 1) / GMP_NUMB_BITS             \
                       * sizeof(mp_limb_t));                                    \
    *(sign) = (c)->_mpfr_sign;                                                  \
    return (c)->_mpfr_exp;                                                      \
  } while (0)

#define BINARY(name)                                                            \
  mpfr_exp_t rounded_##name(mpfr_prec_t pc, mp_limb_t *dc, mpfr_sign_t *sc,     \
                            mpfr_prec_t pa, mpfr_sign_t sa, mpfr_exp_t ea,      \
                            const mp_limb_t *da,                                \
                            mpfr_prec_t pb, mpfr_sign_t sb, mpfr_exp_t eb,      \
                            const mp_limb_t *db,                                \
                            mpfr_rnd_t rnd)                                     \
  {                                                                             \
    mpfr_t a, b, c;                                                             \
    view(a, pa, sa, ea, da);                                                    \
    view(b, pb, sb, eb, db);                                                    \
    RESULT(c, pc, dc);                                                          \
    mpfr_##name(c, a, b, rnd);                                                  \
    FINISH(c, pc, dc, sc);                                                      \
  }

BINARY(add)
BINARY(sub)
BINARY(mul)
BINARY(div)
BINARY(min)
BINARY(max)

#define UNARY(name)                                                             \
  mpfr_exp_t rounded_##name(mpfr_prec_t pc, mp_limb_t *dc, mpfr_sign_t *sc,     \
                            mpfr_prec_t pa, mpfr_sign_t sa, mpfr_exp_t ea,      \
                            const mp_limb_t *da,                                \
                            mpfr_rnd_t rnd)                                     \
  {                                                                             \
    mpfr_t a, c;                                                                \
    view(a, pa, sa, ea, da);                                                    \
    RESULT(c, pc, dc);                                                          \
    mpfr_##name(c, a, rnd);                                                     \
    FINISH(c, pc, dc, sc);                                                      \
  }

UNARY(neg)
UNARY(abs)

#define COMPARISON(name)                                                        \
  int rounded_##name(mpfr_prec_t pa, mpfr_sign_t sa, mpfr_exp_t ea,             \
                     const mp_limb_t *da,                                       \
                     mpfr_prec_t pb, mpfr_sign_t sb, mpfr_exp_t eb,             \
                     const mp_limb_t *db)                                       \
  {                                                                             \
    mpfr_t a, b;                                                                \
    view(a, pa, sa, ea, da);                                                    \
    view(b, pb, sb, eb, db);                                                    \
    return mpfr_##name(a, b);                                                   \
  }

COMPARISON(cmp)
COMPARISON(equal_p)
COMPARISON(lessgreater_p)
COMPARISON(lessequal_p)
COMPARISON(less_p)
COMPARISON(greaterequal_p)
COMPARISON(greater_p)
COMPARISON(unordered_p)
