library(vetr)

unitizer_sect("Basic Tests", {

  vetr:::len_chr_len(1000L)
  vetr:::len_chr_len(1L)
  vetr:::len_chr_len(1234567890L)
  vetr:::len_chr_len(1234567890000000000000000)

  vetr:::len_as_chr(1000L)
  vetr:::len_as_chr(1L)
  vetr:::len_as_chr(1234567890L)
  vetr:::len_as_chr(1234567890000000000000000)

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
  vetr:::num_as_chr(100);
  vetr:::num_as_chr(100.01);

  # switch ot scientific

  vetr:::num_as_chr(1e9 + 0.1);
  vetr:::num_as_chr(-1e9 - 0.1);

  vetr:::num_as_chr(1e9 + 0.1, as.int=TRUE);
  vetr:::num_as_chr(-(1e9 + 0.1), as.int=TRUE);

  vetr:::num_as_chr(1e9 - 0.1);
  vetr:::num_as_chr(-(1e9 - 0.1));

  # corner cases

  vetr:::num_as_chr(NA);
  vetr:::num_as_chr(NaN);
  vetr:::num_as_chr(Inf);
  vetr:::num_as_chr(-Inf);
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

  vetr:::strsub(lorem.tr.phrases, 25L, TRUE)
  vetr:::strsub(lorem.tr.phrases, 25L, FALSE)

  vetr:::strsub(lorem.ru.phrases, 25L, TRUE)
  vetr:::strsub(lorem.ru.phrases, 25L, FALSE)

  vetr:::strsub(lorem.cn.phrases, 25L, TRUE)
  vetr:::strsub(lorem.cn.phrases, 25L, FALSE)

  vetr:::strsub(lorem.emo.phrases, 25L, TRUE)
  vetr:::strsub(lorem.emo.phrases, 25L, FALSE)
})

unitizer_sect("UTF8", {
  utf8.kuhn <- readLines('unitizer/helper/UTF-8-test.txt', encoding='UTF-8');
  test.start <- grep("^Here come the tests:", utf8.kuhn)
  test.start

  utf8.test <- tail(utf8.kuhn, -test.start)
  utf8.test

  nchar.base <- nchar(utf8.test, allowNA=TRUE)
  untranslatable <- is.na(nchar.base)
  nchar.vetr <- vetr:::nchar_u(utf8.test)

  # Not all lines match between nchar and vetr, differences seem to be that the
  # invalid encoding starting \xf4\x90 is resolved to 1 character by base (test
  # 2.3.5), and that base resolves things in the U+D800 - U+DFFF range as 1
  # character (5.1-5.2); perhaps base implements the more forgiving versions of
  # UTF8?

  base.vetr.diff <- !is.na(nchar.base) & nchar.vetr != nchar.base
  sprintf(
    "%d %d %s",
    nchar.base[base.vetr.diff],
    nchar.vetr[base.vetr.diff],
    utf8.test[base.vetr.diff]
  )
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

  paste(
    ifelse(nchar.vetr > 79, sprintf("<%d>", nchar.vetr), "  "), utf8.test
  )
  # Other examples from the Unicode 10.0 docs

  vetr:::nchar_u("\xC2\x41\x41")
  vetr:::nchar_u("\x61\xF1\x80\x80")
  vetr:::nchar_u("\x61\xF1\x80\x80\xE1\x80")
  vetr:::nchar_u("\x61\xF1\x80\x80\xE1\x80\xC2\x62\x80\x63\x80\xBF\x64")
})
