## 0.1.0

Initial release.

## 0.0.2

Finalizing initial release.

* [#40](https://github.com/brodieG/validate/issues/40): Removed `suggests`
  dependencies to ggplot, microbenchmark, and valaddin to improve travis build
  time
* Internal: formatting strings longer than `nchar.max` no longer allowed
* [#39](https://github.com/brodieG/validate/issues/39): `type_alike` return
  values structured like `alike`, doc fixes
* [#38](https://github.com/brodieG/validate/issues/38): Run with valgrind
* [#36](https://github.com/brodieG/validate/issues/36): Fix INTEGER C bug
* [#34](https://github.com/brodieG/validate/issues/34): allow substitution of
  `.` symbol when part of `..`.
* [#33](https://github.com/brodieG/validate/issues/33): prevent infinite
  recursion with recursive symbol substitution
* [#30](https://github.com/brodieG/validate/issues/30): allow specification of
  substitution / matching / evaluation environment
* [#28](https://github.com/brodieG/validate/issues/28): expose alike and vetr
  setting control
* [#24](https://github.com/brodieG/validate/issues/24): clarify use of
  `vet_token`
* [#18](https://github.com/brodieG/validate/issues/18): better documentation for
  NSE
* [#11](https://github.com/brodieG/validate/issues/11): segfault when validating
  language objects
