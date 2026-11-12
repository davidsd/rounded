# Revision history for rounded

## next

* `+`, `-`, `*`, `/`, `negate`, `abs`, `min`, `max`, `add_`, `sub_`, `mul_`,
  `div_`, `min_`, `max_` and all comparisons each make a single unsafe
  foreign call (`cbits/ops.c`) on the operands' limbs, instead of copying them
  into pinned buffers for a safe call.  Results are unchanged, bit for bit.
  At 768 to 1024 bits a multiplication takes 2.4 to 2.9 times less time, an
  addition 5 to 6 times less, and a comparison about 15 times less.  Above
  16384 bits the arithmetic still makes safe calls.
* New test suite `rounded-ops-test` (the bitwise comparison) and benchmark
  `rounded-ops-bench`.

## 1.1.1 -- 2023-10-02

* Add `gmp` to `pkgconfig-depends`.  Fixes: <https://github.com/ekmett/rounded/issues/35>.
* Bump dependency versions.  Fixes: <https://github.com/ekmett/rounded/issues/34>.

## 1.1 -- 2020-05-19

* Fixed `simplify` with `Nat` precisions by adding kind signatures.

## 1.0 -- 2019-08-28

* Require MPFR 4.0 or above.

## 0.2 -- 2019-08-28

* Wrapped a lot more of the MPFR 3.1 API.
* Fixed `wrapped_mpfr_set_ld()` FFI binding.  Previously `fromLongDouble`
  could corrupt memory or crash due to missing argument.
* Removed dependency on `singletons`.

## 0.1.0.1 -- 2018-10-31

* License changed from LGPL to BSD3.

## 0.1 -- 2018-10-30

* Bindings rewritten as a straight FFI binding without a patched `MPFR`.
* Added `Simple` module.

## 0.0.1

* Repository initialized based on older work on a package called `precision`
* Daniel Peebles was able to get a patched `MPFR` to work
