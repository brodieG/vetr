# In own file due to Solaris issues preventing direct inclusion in unitizer
#
# Well-Formed UTF-8 Byte Sequences
#
# Code Points        | Byte 1 | Byte 2 | Byte 3 | Byte 4
# U+0000..U+007F     | 00..7F |
# U+0080..U+07FF     | C2..DF | 80..BF
# U+0800..U+0FFF     | E0     | A0..BF | 80..BF
# U+1000..U+CFFF     | E1..EC | 80..BF | 80..BF
# U+D000..U+D7FF     | ED     | 80..9F | 80..BF
# U+E000..U+FFFF     | EE..EF | 80..BF | 80..BF
# U+10000..U+3FFFF   | F0     | 90..BF | 80..BF | 80..BF
# U+40000..U+FFFFF   | F1..F3 | 80..BF | 80..BF | 80..BF
# U+100000..U+10FFFF | F4     | 80..8F | 80..BF | 80..BF

# Check all the critical cases where we transition from legal to illegal
# sequences

crit.1 <- c(
  n.0="\x7F",
  y.1="\x80"
)
Encoding(crit.1) <- "UTF-8"
crit.2 <- c(
  n.0="\xC1\x91",
  n.1="\xC2\x79",
  y.2="\xC2\x80",
  y.3="\xDF\xBF",
  n.4="\xDF\xC0"
)
Encoding(crit.2) <- "UTF-8"
crit.3 <- c(
  n.00="\xE0\x9F\x91",
  n.01="\xE0\xA0\x79",
  y.02="\xE0\xA0\x80",
  y.03="\xE0\xBF\xBF",
  n.04="\xE0\xBF\xC0",
  n.05="\xE1\x79\x91",
  y.06="\xE1\x80\x80",
  y.07="\xEC\xBF\xBF",
  n.08="\xEC\xBF\xC0",
  n.10="\xEC\xC0\xBF",
  n.11="\xED\x79\x80",
  n.12="\xED\x80\x79",
  y.13="\xED\x80\x80",
  y.14="\xED\x9F\xBF",
  n.16="\xED\x9F\xC0",
  n.17="\xED\xA0\xBF",
  n.18="\xEE\x80\x79",
  n.19="\xEE\x79\x80",
  y.20="\xEE\x80\x80",
  y.21="\xEF\xBF\xBF",
  n.22="\xEF\xBF\xC0",
  n.23="\xEF\xC0\xBF"
)
Encoding(crit.3) <- "UTF-8"
crit.4 <- c(
  n.00="\xF0\x89\x80\x80",
  n.01="\xF0\x90\x79\x80",
  n.02="\xF0\x90\x80\x79",
  y.03="\xF0\x90\x80\x80",
  y.04="\xF0\xBF\xBF\xBF",
  n.06="\xF0\xBF\xBF\xC0",
  n.07="\xF0\xBF\xC0\xBF",
  n.08="\xF0\xC0\xBF\xBF",
  n.09="\xF1\x80\x80\x79",
  n.10="\xF1\x80\x79\x80",
  n.11="\xF1\x79\x80\x80",
  y.12="\xF1\x80\x80\x80",
  y.13="\xF3\xBF\xBF\xBF",
  n.14="\xF3\xBF\xBF\xC0",
  n.15="\xF3\xBF\xC0\xBF",
  n.16="\xF3\xC0\xBF\xBF",
  n.17="\xF4\x80\x80\x79",
  n.18="\xF4\x80\x79\x80",
  n.19="\xF4\x79\x80\x80",
  y.20="\xF4\x80\x80\x80",
  y.21="\xF4\x8F\xBF\xBF",
  n.22="\xF4\x8F\xBF\xC0",
  n.23="\xF4\x8F\xC0\xBF",
  n.24="\xF4\x90\x01\x01",
  n.25="\xF5\x81\x81\x81"
)
Encoding(crit.4) <- "UTF-8"
