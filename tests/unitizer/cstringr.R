library(cstringr)

unitizer_sect("Basic Tests", {
  lorem <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

  len_chr_len(1000L)
  len_chr_len(1L)
  len_chr_len(1234567890L)
  len_chr_len(1234567890000000000000000)

  len_as_chr(1000L)
  len_as_chr(1L)
  len_as_chr(1234567890L)
  len_as_chr(1234567890000000000000000)

  identical(strmlen(lorem), nchar(lorem))
  strmlen(lorem, 100L)

  identical(strmcpy(lorem), lorem)
  strmcpy("")
  strmcpy(lorem, 20L)
  identical(nchar(strmcpy(lorem, 20L)), 20L)

  smprintf2("%s %s", lorem, lorem, 10L)
  smprintf2("%s %s hello world there", lorem, lorem, 10L)

  lorem # make sure lorem unchanged

  ucfirst("hello WORLD")
  lcfirst("HELLO world")

  strbullet(
    c(
      "hello world\nhow are things today",
      "once upon a time\nlived a funny duck"
    )
  )
  strbullet("hello\nblah\n", bullet="  - ", ctd="    ")

  strbullet(1:10) # error

  collapse(letters[1:5])
  collapse(letters[1:5], sep="\n")
  collapse(character())


})
unitizer_sect("Corner Cases", {
  # test maxlen overflows

  strbullet(c("hello world"), maxlen=5L)  # early fail
  strbullet(c("hello world"), maxlen=12L) # fails when adding bullets
  strbullet(c("hello world"), maxlen=14L) # works

  # these are all supposed to fail

  cstringr:::test1()
  cstringr:::test2()
  cstringr:::test3()

  strmlen(list(), 100L)

  # quickly confirm the other smprintfs work correctly

  cstringr:::test4()

  cstringr:::test5()   # warning
})
