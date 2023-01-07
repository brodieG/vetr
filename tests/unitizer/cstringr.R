# Copyright (C) 2023 Brodie Gaslam
#
# This file is part of "vetr - Trust, but Verify"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

library(vetr)

unitizer_sect("Basic Tests", {

  vetr:::len_chr_len(1000L)
  vetr:::len_chr_len(1L)
  vetr:::len_chr_len(1234567890L)
  len0 <- 1234567890000000000000000
  vetr:::len_chr_len(len0)

  vetr:::len_as_chr(1000L)
  vetr:::len_as_chr(1L)
  vetr:::len_as_chr(1234567890L)
  vetr:::len_as_chr(len0)

  identical(vetr:::strmlen(lorem), nchar(lorem))
  vetr:::strmlen(lorem, 100L)

  identical(vetr:::strmcpy(lorem), lorem)
  vetr:::strmcpy("")
  vetr:::strmcpy(lorem, 20L)
  identical(nchar(vetr:::strmcpy(lorem, 20L)), 20L)

  vetr:::smprintf2("%s %s", lorem, lorem, 10L)
  vetr:::smprintf2("%s %s hello world there", lorem, lorem, 10L)

  lorem # make sure lorem unchanged

  vetr:::ucfirst("hello WORLD")
  vetr:::lcfirst("HELLO world")

  vetr:::strbullet(
    c(
      "hello world\nhow are things today",
      "once upon a time\nlived a funny duck"
    )
  )
  vetr:::strbullet("hello\nblah\n", bullet="  - ", ctd="    ")

  vetr:::strbullet(1:10) # error

  vetr:::collapse(letters[1:5])
  vetr:::collapse(letters[1:5], sep="\n")
  vetr:::collapse(character())
})
unitizer_sect("numbers as character", {
  vetr:::num_as_chr(100)
  vetr:::num_as_chr(100.01)

  # switch ot scientific

  num0 <- 1e9 + 0.1
  num1 <- -1e9 - 0.1

  # need to sub leading zeros before 'e' due to different display in
  # windows vs nix
  #
  sub("e[+-]?\\K0*", "", vetr:::num_as_chr(num0), perl=TRUE)
  sub("e[+-]?\\K0*", "", vetr:::num_as_chr(num1), perl=TRUE)

  vetr:::num_as_chr(num0, as.int=TRUE)
  vetr:::num_as_chr(num1, as.int=TRUE)

  num2 <- 1e9 - 0.1
  num3 <- -(1e9 - 0.1)

  vetr:::num_as_chr(num2)
  vetr:::num_as_chr(num3)

  # corner cases

  vetr:::num_as_chr(NA)
  vetr:::num_as_chr(NaN)
  vetr:::num_as_chr(Inf)
  vetr:::num_as_chr(-Inf)
})
unitizer_sect("smprintf6", {
  vetr:::smprintf6(
    "%s %s %s %s %s %s", "a", "bb", "ccc", "dddd", "eeeee", "ffffff"
  )
  vetr:::smprintf6(
    "%s %s %s %s %s %s", "a", "bb", "ccc", "dddd", "eeeee", "ffffff", 10L
  )
  vetr:::smprintf6(
    "%s %s %s %s %s %s", "a", "bb", "ccc", "dddd", "eeeee", "ffffff", 18L
  )
  # bad format strings
  vetr:::smprintf6(
    "%s %s %s %s", "a", "bb", "ccc", "dddd", "eeeee", "ffffff"
  )
  # bad format strings2 - this one reads memeory it shouldn't
  # vetr:::smprintf6(
  #   "%s %s %s %s %s %s %s %s", "a", "bb", "ccc", "dddd", "eeeee", "ffffff"
  # )
})
unitizer_sect("Corner Cases", {
  # test maxlen overflows

  vetr:::strbullet(c("hello world"), maxlen=5L)  # early fail
  vetr:::strbullet(c("hello world"), maxlen=12L) # fails when adding bullets
  vetr:::strbullet(c("hello world"), maxlen=14L) # works

  # these are all supposed to fail

  vetr:::test_strmcpy()
  vetr:::test_strappend()
  vetr:::test_add_szt()

  vetr:::strmlen(list(), 100L)

  # quickly confirm the other smprintfs work correctly

  vetr:::test_smprintfx()

  vetr:::test_strappend2()   # warning
})

unitizer_sect("substr", {
  vetr:::strsub(lorem.phrases, 25L, TRUE)
  vetr:::strsub(lorem.phrases, 25L, FALSE)

  # UTF8

  vetr:::strsub(lorem.tr.phrases, 25L, TRUE)
  vetr:::strsub(lorem.tr.phrases, 25L, FALSE)

  vetr:::strsub(lorem.ru.phrases, 25L, TRUE)
  vetr:::strsub(lorem.ru.phrases, 25L, FALSE)

  vetr:::strsub(lorem.cn.phrases, 25L, TRUE)
  vetr:::strsub(lorem.cn.phrases, 25L, FALSE)

  # Unfortunately something is going wrong with how out-of-BMP unicode is read
  # in by windows so we have to comment out these tests; see #82
  # vetr:::strsub(lorem.emo.phrases, 25L, TRUE)
  # vetr:::strsub(lorem.emo.phrases, 25L, FALSE)

  # Errors

  vetr:::strsub(lorem.phrases, 1:2, TRUE)
  vetr:::strsub(lorem.phrases, 25L, 1:2)
  vetr:::strsub(1:2, 25L, TRUE)

  vetr:::strsub(lorem.phrases, 2L, TRUE)
  vetr:::strsub(lorem.phrases, 3L, TRUE)  # works
})
unitizer_sect("nchar_u", {
  vetr:::nchar_u(1:10)
  vetr:::nchar_u(c("a", "ab", "abc"))
})
unitizer_sect("char_offsets", {
  vetr:::char_offsets(1:10)
  vetr:::char_offsets(c("a", "ab", "abc"))
})
unitizer_sect("UTF8 corner cases, in UTF-8", {
  # Originally we tried using `Sys.setlocale` but that isn't guaranteed to work
  # e.g. failed on windows

  utf8.kuhn <- readLines('unitizer/helper/UTF-8-test.txt', encoding='UTF-8');
  test.start <- grep("^Here come the tests:", utf8.kuhn, useBytes=TRUE)
  test.start

  utf8.test <- tail(utf8.kuhn, -test.start)
  # suppressWarnings(utf8.test)  # Solaris problems

  nchar.base <- nchar(utf8.test, allowNA=TRUE)
  untranslatable <- is.na(nchar.base)
  nchar.vetr <- vetr:::nchar_u(utf8.test)

  # Not all lines match between nchar and vetr, differences seem to be that the
  # invalid encoding starting \xf4\x90 is resolved to 1 character by base (test
  # 2.3.5), and that base resolves things in the U+D800 - U+DFFF range as 1
  # character (5.1-5.2); perhaps base implements the more forgiving versions of
  # UTF8?

  base.vetr.diff <- !is.na(nchar.base) & nchar.vetr != nchar.base
  # # this actually causes problems with `str`, in particular with `strtrim` so
  # # we can't use it in the tests
  # sprintf(
  #   "%d %d %s",
  #   nchar.base[base.vetr.diff],
  #   nchar.vetr[base.vetr.diff],
  #   utf8.test[base.vetr.diff]
  # )

  # For visual verification, according to docs all test lines should render at
  # 79 characters, but this appears to be in contravention to the UTF8
  # documentation.  This is the body of the e-mail I sent Dr. Kuhn inquiring
  # about the seeming discrepancy (which could possibly be explained by unicode
  # version differences):
  #
  # In section 3.3.3, 4-byte sequence with last byte missing, the sequence in
  # question appears to be f0 80 80 22 where 22 is the double quote ending the
  # sequence.  Furthermore, you state that I should expect to "... see only a
  # single replacement character".
  #
  # What I'm a little confused by is that the sequence "f0 80 80" becomes
  # illegal after the first byte as per table 3.7 from the Unicode Standard
  # v10.0 since the second byte is less than 90.  Then, by the definition of
  # "Maximal subpart of an ill-formed subsequence", it seems that the maximal
  # subpart starting at "f0" is actually "f0" since "f0 80" is not legal and
  # thus not a subpart.  There is a close example in the documentation (p129 of
  # the document in question):
  #
  #    Another example illustrates the application of the concept of maximal
  #    subpart for UTF-8 continuation bytes outside the allowable ranges defined
  #    in Table 3-7.  The UTF-8 sequence <41 E0 9F 80 41> is ill-formed, because
  #    <9F> is not an allowed second byte of a UTF-8 sequence commencing with
  #    <E0>. In this case, there is an unconvertible offset at <E0> and the
  #    maximal subpart at that offset is also <E0>. The subsequence <E0 9F>
  #    cannot be a maximal subpart, because it is not an initial subsequence of
  #    any well-formed UTF-8 code unit sequence.
  #
  # This is basically the same issue with "e0" substituted for "f0" in our case
  # (I think). This would suggest we should see three characters, one for the
  # maximal subpart "f0", and then also one each for the two "80" since they
  # themselves are illegal.  I think that aligns with the "recommended" policy.
  # I'm pretty new at this so I suspect I'm just misunderstanding something.  If
  # you see an obvious mistake in my reasoning I would appreciate you pointing
  # me to it.

  # # Same issue with strtrim
  # paste(
  #   ifelse(nchar.vetr > 79, sprintf("<%d>", nchar.vetr), "  "), utf8.test
  # )

  # Other examples from the Unicode 10.0 docs

  source('unitizer/helper/UTF-8-unicode-10-ex.R', local=TRUE)

  vetr:::nchar_u(unicode.10[1])
  vetr:::nchar_u(unicode.10[2])
  vetr:::nchar_u(unicode.10[3])
  vetr:::nchar_u(unicode.10[4])

  # Confirm offsets are what they should be

  vetr:::char_offsets(unicode.10[4])

  # Check all the critical cases where we transition from legal to illegal
  # sequences

  source('unitizer/helper/UTF-8-critical.R', local=TRUE)

  Map(vetr:::char_offsets, crit.1)
  Map(vetr:::char_offsets, crit.2)
  Map(vetr:::char_offsets, crit.3)
  Map(vetr:::char_offsets, crit.4)
})
unitizer_sect("UTF-8 corner cases - other encodings", {

  source('unitizer/helper/latin-1.R', local=TRUE)

  lapply(lat.1.1, vetr:::char_offsets)
  lapply(lat.1.2, vetr:::char_offsets)

  vetr:::strsub(lat.1.1, 3L, mark=FALSE)
  vetr:::strsub(lat.1.2, 3L, mark=FALSE)
})

