
lorem <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
Encoding(lorem) <- "UTF-8"
lorem.phrases <- unlist(strsplit(lorem, "[.,]\\K ", perl=TRUE))

# From the Stalin russian wiki page:
#
# <https://ru.wikipedia.org/w/index.php?title=%D0%A1%D1%82%D0%B0%D0%BB%D0%B8%D0%BD,_%D0%98%D0%BE%D1%81%D0%B8%D1%84_%D0%92%D0%B8%D1%81%D1%81%D0%B0%D1%80%D0%B8%D0%BE%D0%BD%D0%BE%D0%B2%D0%B8%D1%87&oldid=12832996>
#
# Licensed under GNU Free Documentation License since it is from before 2009

lorem.ru <- "Родился 6 (18) декабря 1878 года (по записи в метрической книге Горийской Успенской соборной церкви[6]Это подлинная дата рождения Сталина. Позже она подтверждалась в уведомлении Санкт-Петербургского жандармского управления и самим Сталиным в ответе на анкету шведской газеты «Folkets Dagblad Politiken» в 1920 г.[7]) в Грузии в городе Гори, хотя начиная с 1929 года[источник?] днём его рождения официально считалось 9 (21) декабря 1879. Был третьим сыном в семье, первые двое умерли в младенчестве. Его родным языком был грузинский, русский язык Сталин выучил позже, но всегда говорил с заметным грузинским акцентом [8]. Согласно утверждениям дочери Светланы, Сталин, однако, пел по-русски практически без акцента."
Encoding(lorem.ru) <- "UTF-8"
lorem.ru.phrases <- unlist(strsplit(lorem.ru, "[.,]\\K ", perl=TRUE))

# From the Stalin turkish wiki page:
#
# <https://tr.wikipedia.org/w/index.php?title=Josef_Stalin&oldid=4719847>
#
# Licensed under GNU Free Documentation License since it is from before 2009

lorem.tr <- "Bu tartışmalı tarihsel dönemle ilgili olarak, Stalin'e düşman veya Stalin'den yana olan her iki tarafın da farklı tezleri vardır. Stalin karşıtlarının tezlerine göre, Hitlerle aralarındaki açıklanmayan gizli protokole bağlı olarak Finlandiya, Estonya, Letonya, Litvanya, Romanya ve Polonya'nin Naziler veya Sovyetler tarafından işgalinin yolu açılmıştır. Stalin'in doğru yaptığını savunanlara göre ise, 1937'deki Münih görüşmelerinde açıkça ortaya çıktığı gibi, İngiliz ve Fransız emperyalistleri ve dolaylı olarak da Amerikalılar, Nazileri kışkırtıyorlardı ve onların Sovyetler Birliği'ne saldırısının önünü açmaya çalışıyorlardı. Bu amaçla Avusturya'nın Almanya'ya katılmasına (Anschluss) ve Çekoslovakya'nın işgaline göz yummuş ve onaylamışlardı.Ne var ki, özellikle Çekoslovakya'nın işgalinden sonra Sovyetler Birliği'nin İngiltere ve Fransa ile ilişki kurma çabalarına rağmen bu iki ülke Nazi tehdidini birlikte ortadan kaldırma girişimini reddetti. Böylece Sovyetler Birliği, kendi sınırlarını güvence altına almak için bu protokolü imzaladı. Stalin'in amaçlarına göre, Polonya ve Baltık ülkelerinde oluşturulacak tampon bölgeler, Nazilerin Sovyetler Birliği'ne ulaşmasını engelleyecekti. Böylece 1939 yılında Nazi işgalinden sonra Sovyetler Polonya'nın kalan yarısını işgal edip Estonya, Litvanya ve Letonya'yı sınırlarına kattı. Finlandiya'ya saldırdı ve büyük kayıplar vermesine rağmen Mart 1940'da \"kış savaşı' olarak bilinen bu savaşı da kazandı. 1941'de Hitler'in Sovyetlere saldırması üzerine Stalin bu sefer müttefiklerin yanında yer aldı. II. Dünya Savaşı'nın en ağır bedeli ödeyen güç olarak (24 milyon ölü) müttefiklerin yanında Nazi Almanyası'na karşı kazandığı zafer uluslararası alanda gücünü artırdı."
Encoding(lorem.tr) <- "UTF-8"
lorem.tr.phrases <- unlist(strsplit(lorem.tr, "[.,]\\K ", perl=TRUE))

# From the PRC chinese wiki page:
#
# <https://zh.wikipedia.org/w/index.php?title=%E4%B8%AD%E5%8D%8E%E4%BA%BA%E6%B0%91%E5%85%B1%E5%92%8C%E5%9B%BD&oldid=8944743>
#
# Licensed under GNU Free Documentation License since it is from before 2009

lorem.cn <- "中華人民共和國是單一制的多民族國家。全國劃分為23個省（其中台灣省并沒有實際管辖）、5個自治區、4個直轄市和2個根據一國兩制設立的特別行政區，均直屬於中央人民政府。中华人民共和国跨越五个地理时区，但全国均使用北京时间（UTC+8，东八区）作为标准时间。中華人民共和國官方認定的民族現有56個，其中最大民族汉族佔總人口的91.59%，其餘55族統稱為少数民族，所有民族統稱為中華民族；除回族外，其他54個少数民族如壮族、维吾尔族、滿族、蒙古族、藏族、朝鲜族等也多使用自己的語言與文字。主要宗教有佛教、道教、基督教（多指新教）、天主教和伊斯兰教等，但過半人口無特定宗教信仰。中华人民共和国的通用语言是汉语普通話，當中在中國大陸通行的漢字為簡體字，而在港澳台地區通行的漢字則為繁體字。"
Encoding(lorem.cn) <- "UTF-8"
cn.split <-  "[、。]\\K"
Encoding(cn.split) <- "UTF-8"
lorem.cn.phrases <- unlist(strsplit(lorem.cn, cn.split, perl=TRUE))

# Emoji from Unicode Site <http://unicode.org/emoji/charts/full-emoji-list.html>

emoji <- c(
  "\U0001f600", "\U0001f619", "\U0001f61a", "\U0001f642", "\U0001f92f",
  "\U0001f62c", "\U0001f630", "\U0001f631", "\U0001f633", "\U0001f92a",
  "\U0001f635", "\U0001f637", "\U0001f912", "\U0001f915", "\U0001f922",
  "\U0001f92e", "\U0001f927", "\U0001f607", "\U0001f920", "\U0001f921",
  "\U0001f925", "\U0001f92b", "\U0001f92d", "\U0001f9d0", "\U0001f913",
  "\U0001f608", "\U0001f4a9", "\U0001f63e",
  "\U0001f469\U0001f3ff\U0000200d\U0001f3eb",
  "\U0001f468\U0001f3fb\U0000200d\U00002696\U0000FE0F", "\U0001f46b",
  "\U0001f469\U0000200D\U00002764\U0000200d\U0001f48b\U0000200d\U0001f468",
  "\U0001f468\U0000200d\U0001f468\U0000200d\U0001f467",
  "\U0001f468\U0000200d\U0001f468\U0000200d\U0001f467\U0000200d\U0001f466",
  "\U0001f984", "\U0001f36b", "\U0001f1e6\U0001f1f7", "\U0001f1ed\U0001f1f0",
  "\U0001f1ef\U0001f1f5", "\U0001f1f0\U0001f1f5", "\U0001f1fa\U0001f1f8",
  "\U0001f3f4"
)
Encoding(emoji) <- "UTF-8"
lorem.emo <- paste(
  sample(
    c(emoji, rep(", ", 3), rep(". ", 3), rep(" ", 40)), 450, replace=TRUE
  ),
  collapse=""
)
lorem.emo.phrases <- unlist(strsplit(lorem.emo, "[,.]\\K", perl=TRUE))

# trick sequences
