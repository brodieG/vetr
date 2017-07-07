library(vetr)

unitizer_sect("Basic Tests", {
  lorem <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

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

  vetr:::test1()
  vetr:::test2()
  vetr:::test3()

  vetr:::strmlen(list(), 100L)

  # quickly confirm the other smprintfs work correctly

  vetr:::test4()

  vetr:::test5()   # warning
})
