## 0.2.0

* [#48](https://github.com/brodieG/vetr/issues/48): Implement `all_bw`, a
  more efficient version of `!anyNA(.) && all(. < x) && all(. > y)`.
* [#65](https://github.com/brodieG/vetr/issues/65)
  [#51](https://github.com/brodieG/vetr/issues/51): Check expressions that
  return character vectors will have part of the first element of that vector
  included in the error message.
* [#69](https://github.com/brodieG/vetr/issues/69): Vetting expressions that
  use the symbol of the object being vetted are no longer valid.  This avoid
  confusion caused by intended standard tokens being treated as template tokens
  because they use the object symbol instead of `.` to refer to the object.
* [#64](https://github.com/brodieG/vetr/issues/64): Rewrite result handling
  for multi token expressions to avoid unnecessary slow downs
* [#43](https://github.com/brodieG/vetr/issues/43): Fix rchck, rcnst, UBSAN,
  valgrind (ht @kalibera).
* [#76](https://github.com/brodieG/vetr/issues/76): Standardize defined
  terms (e.g. Standard vs Template Tokens)
* [#77](https://github.com/brodieG/vetr/issues/77): Replace `SIZE_T_MAX`
  with `SIZE_MAX` for portability
* [#70](https://github.com/brodieG/vetr/issues/70): Feedback from Richie
  Cotton and Michel Lang re: comparison "vignette"
* [#45](https://github.com/brodieG/vetr/issues/45): Cleanup error messages
  for objects that should be NULL.
* [#73](https://github.com/brodieG/vetr/issues/73): Cleaner protection stack
  handling
* [#56](https://github.com/brodieG/vetr/issues/56): Over-aggressive
  detection of infinite recursion in symbol substitution
* [#81](https://github.com/brodieG/vetr/issues/81): Remove test that attached
  attribute to symbol (illegal in R-devel now).
* [#59](https://github.com/brodieG/vetr/issues/59): Add a `CONTRIBUTING.md`
* Assorted typos (@franknarf1, @DasonK)

## 0.1.0

Initial release.

## 0.0.2

Finalizing initial release.

* [#40](https://github.com/brodieG/vetr/issues/40): Removed `suggests`
  dependencies to ggplot, microbenchmark, and valaddin to improve travis build
  time
* Internal: formatting strings longer than `nchar.max` no longer allowed
* [#39](https://github.com/brodieG/vetr/issues/39): `type_alike` return
  values structured like `alike`, doc fixes
* [#38](https://github.com/brodieG/vetr/issues/38): Run with valgrind
* [#36](https://github.com/brodieG/vetr/issues/36): Fix INTEGER C bug
* [#34](https://github.com/brodieG/vetr/issues/34): allow substitution of
  `.` symbol when part of `..`.
* [#33](https://github.com/brodieG/vetr/issues/33): prevent infinite
  recursion with recursive symbol substitution
* [#30](https://github.com/brodieG/vetr/issues/30): allow specification of
  substitution / matching / evaluation environment
* [#28](https://github.com/brodieG/vetr/issues/28): expose alike and vetr
  setting control
* [#24](https://github.com/brodieG/vetr/issues/24): clarify use of
  `vet_token`
* [#18](https://github.com/brodieG/vetr/issues/18): better documentation for
  NSE
* [#11](https://github.com/brodieG/vetr/issues/11): segfault when validating
  language objects
