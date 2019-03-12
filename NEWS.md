## 0.2.7

* Fix new `rchk` warnings.
* Set `RNGversion()` due to changes to sampling mechanism.

## 0.2.6

* [#96](https://github.com/brodieG/vetr/issues/96) Fix r-devel test failures
  that started with r75024.
* [#94](https://github.com/brodieG/vetr/issues/94) Properly credit `vapply` for
  template concept.

## 0.2.5

* Address CRAN warnings about packages used in tests not in suggests.

## 0.2.4

* As per [#93](https://github.com/brodieG/vetr/issues/93), ensure that attribute
  comparisons are always done in the same order.  We now sort the attribute
  lists prior to comparison.  This may result in slightly different output than
  previously as which attribute is declared incorrect or missing may change as a
  result of the sort since the first such attribute is reported.  Additionally,
  there is now more explicit handling of missing attributes so the error
  reporting for them will be slightly different.
* Fix memory problems reported by valgrind.

## 0.2.3

* [#92](https://github.com/brodieG/vetr/issues/92) `vetr` evaluated expressions
  in wrong environment.
* [#89](https://github.com/brodieG/vetr/issues/89) Zero length vetting token
  results pass; this is to align with `all(logical(0))` and consequently
  `stopifnot`.
* [#88](https://github.com/brodieG/vetr/issues/88) Extra space in deparsed
  vetted language.

## 0.2.2

* Test errors on Solaris.

## 0.2.1

* Fix Solaris compilation issue.
* Fix new `rcheck` warnings.
* Change R dependency to 3.3.2 to avoid problems with CRAN osx R-devel build.

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
  time.
* Internal: formatting strings longer than `nchar.max` no longer allowed
* [#39](https://github.com/brodieG/vetr/issues/39): `type_alike` return
  values structured like `alike`, doc fixes.
* [#38](https://github.com/brodieG/vetr/issues/38): Run with valgrind
* [#36](https://github.com/brodieG/vetr/issues/36): Fix INTEGER C bug
* [#34](https://github.com/brodieG/vetr/issues/34): allow substitution of
  `.` symbol when part of `..`.
* [#33](https://github.com/brodieG/vetr/issues/33): prevent infinite
  recursion with recursive symbol substitution
* [#30](https://github.com/brodieG/vetr/issues/30): allow specification of
  substitution / matching / evaluation environment.
* [#28](https://github.com/brodieG/vetr/issues/28): expose alike and vetr
  setting control.
* [#24](https://github.com/brodieG/vetr/issues/24): clarify use of
  `vet_token`.
* [#18](https://github.com/brodieG/vetr/issues/18): better documentation for
  NSE.
* [#11](https://github.com/brodieG/vetr/issues/11): segfault when validating
  language objects.
