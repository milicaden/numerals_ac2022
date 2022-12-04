library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rje)

Folder = "../data/"
####################################
#Natural language data
####################################

natural = c()

###Languages

#Useful
# %% returns the remainder of the division
#%/% integer division operator

###########################
#
# Type 1: base-10, morphemes for 1-10
#
###########################

###########################
# Type 1A
###########################

#Mandarin
#Acoma (aco), Albanian (alb), Arabic Egyptian (aeg), Archi (arc), Armenian (arm), 
#Aymara (aym), Bagirmi (bag), Basaa (bas), Berber (bma), Bribri (bri), Burmese(brm), Chamorro (cha)
#Chuukese (cuu), Comanche (cmn), Damana (dam), Evenki (eve), Ewe (ewe), Fijian (fij), Finnish (fin), German (ger)
#Goajiro (goa), Hausa (hau), Hebrew (heb), Hupa (hup), Igbo (igb), Irawq (irq), Japanese (jpn), Jaqaru (jaq)
#Kabardian (kab), Kanuri (knr), Khalkha (kha),  Korean (kor)
#Koyraboro Seni (kse), Lak (lak), Lakhota (lkt), Lango (lan), Latvian (lat), Lavukaleve (lav)
#Lega (leg), Malagasy (mal), Mandardin (mnd), Mapudungun, (map), Khoekhoe (kho), Ndyuka ("ndy"), Navajo (nav), Nkore-Kiga (nko), Oneida (ond), Oromo Harar (orh)
#Persian (prs), Pohnpeian (poh), Quechua-Imbabura (qim), Sorbian-Upper (sou), Spanish (spa), Telugu (tel), Thai (tha)
#Tuareg (tug), Zulu (zul)
for(y in c("mnd", "aco", "alb", "aeg", "arc", "arm", "aym", "bag", "bas", "bma", "bri", "brm", "cha",
           "cuu", "cmn", "dam", "eve", "ewe", "fij", "fin", "ger", "goa",  "hau", "heb", "hup", "igb", "irq", "jpn", "jaq",
           "kab", "knr", "kha", "kor", "kse", "lak", "lkt", "lan", "lat", "lav",
           "leg", "mal", "map", "kho", "nav", "nez", "nko", "ond", "orh", "prs", "poh", "qim", "sou", "spa", "tel", "tha", "tug", "zul",
           "mao", "niv", "rap", "swa", "vie", "ndy")){
  assign(y, c())
  for(x in c(1:10)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(20,30,40,50,60,70,80,90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
  }
  for(x in c(21:29,31:39,41:49,51:59,61:69,71:79,81:89,91:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
  }
  natural = c(natural, eval(parse(text = y)))
  
}

#Maori (mao), Nivkh (niv), Rapanui (rap), Swahili (swa), Vietnamese (vie), like Mandarin, but 100 = 1*100,  Nez Perce (nez) 100 = 10*100



###########################
# Type 1B
###########################

# Like Type 1A, with the quirk that between 10-20 10 = 1*10
# Abun (abu), Batak Karo (bkr), Kiribati (krb), Paiwan (pai), Sango (san), Sahu (sah), Taba-East Makian (tab), Tukang Besi ("tuk")
for(y in c("abu", "bkr", "krb", "pai", "sah", "san", "tab", "tuk")){
  assign(y, c())
  for(x in c(1:9)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(10,20,30,40,50,60,70,80,90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
  }
  for(x in c(11:19, 21:29,31:39,41:49,51:59,61:69,71:79,81:89,91:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
  }
  natural = c(natural, eval(parse(text = y)))
}



###########################
# Type 1C
###########################
#Indonesian (ind), Sapuan (sap), Tagalog (tag)
#Like Type 1A, with the quirk than 10 = 1*10, but only for 10, (for sap and tag 100 = 1*100)
for(y in c("ind", "sap", "tag")){
  assign(y,  c())
  for(x in c(1:9)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:19)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(10,20,30,40,50,60,70,80,90)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
  }
  for(x in c(21:29,31:39,41:49,51:59,61:69,71:79,81:89,91:99)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
  }
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 1-Hindi
###########################
#Hindi
# Decimal, but 9 quirky everywhere apart from 9, 89 and 99
hin = c()
for(x in c(1:10)){
  hin = c(hin, list(c("hin", paste0("word", x), x)))
}
for(x in c(11:18)){
  hin = c(hin, list(c("hin", paste0("word", x), paste0("10+", x%%10))))
}
for(x in c(19, 29, 39, 49, 59, 69, 79)){
  hin = c(hin, list(c("hin", paste0("word", x), paste0((x+1)/10, "*10-1" ))))
}
for(x in c(20,30,40,50,60,70,80,90)){
  hin = c(hin, list(c("hin", paste0("word", x), paste0(x/10, "*10"))))
}
for(x in c(21:28,31:38,41:48,51:58,61:68,71:78,81:89,91:99)){
  hin = c(hin, list(c("hin", paste0("word", x), paste0(x%/%10, "*10+", x%%10))))
}
natural = c(natural, hin)




###########################
#
# Type 2: base-10, morphemes for 1-10 and some other numbers
#
###########################

###########################
# Type 2A: morphemes for 1-10 and 20
###########################

#Bambara (bam), Bawm (baw), Greek (grk), Hungarian (hun), Hunzib (hzb), Koromfe (kfe), #Hmong Njua (hmo),  Nubian Dongolese (nbd) (hmo and nbd 100 = 1*100)
for(y in c("bam", "baw", "grk", "hun", "hzb", "kfe", "hmo", "nbd")){
  assign(y,  c())
  for(x in c(1:10, 20)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:19)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(30,40,50,60,70,80,90)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
  }
  for(x in c(31:39,41:49,51:59,61:69,71:79,81:89,91:99)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
  }
  for(x in c(21:29)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
  }
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 2-Garo: morphemes for 1-10 and 20, but 30 = 20+10
###########################

#Garo (gar)
for(y in c("gar")){
  assign(y,  c())
  for(x in c(1:10)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:19)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(20,40,50,60,70,80,90)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
  }
  for(x in c(21:29, 41:49,51:59,61:69,71:79,81:89,91:99)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
  }
  for(x in c(30)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10")))))
  }
  for(x in c(31:39)){
    assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+", x%%10)))))
  }
  natural = c(natural, eval(parse(text = y)))
}
###########################
# Type 2-Yakut: morphemes for 1-10 and 20, 30
###########################

#Yakut (ykt)
for(y in c("ykt")){
  assign(y,  c())
for(x in c(1:10, 20, 30)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
}
for(x in c(11:19)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
}
for(x in c(40,50,60,70,80,90)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
}
for(x in c(41:49,51:59,61:69,71:79,81:89,91:99)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
}
for(x in c(21:29)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
}
for(x in c(31:39)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("30+", x%%10)))))
}
  natural = c(natural, eval(parse(text = y)))
}
###########################
# Type 2-Chuvash: morphemes for 1-10 and 20,30,40,50
###########################

#Chuvash (chv)
for(y in c("chv", "tur")){
  assign(y,  c())
  
for(x in c(1:10, 20, 30, 40, 50)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
}
for(x in c(11:19)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
}
for(x in c(60,70,80,90)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
}
for(x in c(21:29)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
}
for(x in c(31:39)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("30+", x%%10)))))
}
for(x in c(41:49)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("40+", x%%10)))))
}
for(x in c(51:59)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("50+", x%%10)))))
}
for(x in c(61:69,71:79,81:89,91:99)){
  assign(y,  c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
}
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 2-English: morphemes for 1-10 and 11
###########################

# English
for(y in c("eng")){
  assign(y, c())
for(x in 1:11){
  assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
}
for(x in 12:19){
  assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x-10)))))
}
for(x in c(20,30,40,50,60,70,80,90)){
  assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10*",x/10)))))
}
#nb: %% remainder operator, %/% quotient operator
for(x in c(21:29, 31:39, 41:49, 51:59, 61:69, 71:79, 81:89, 91:99)){
  assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10*",x%/%10, "+", x%%10)))))
  
}
natural = c(natural, eval(parse(text = y)))

}

###########################
# Type 2-English: morphemes for 1-10 and 40
###########################

#Russian (rus)
#Like Mandarin, but 40 monomorphemic
for(y in c("rus")){
  assign(y, c())
  for(x in c(1:10, 40)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(20,30,50,60,70,80,90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
  }
  for(x in c(21:29,31:39,51:59,61:69,71:79,81:89,91:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
  }
  for(x in c(41:49)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("40+", x%%10)))))
  }
  natural = c(natural, eval(parse(text = y)))
  
}

###########################
#
# Type 3: base-10, but not all morphemes for 1-9
#
###########################

###########################
# Type 3A: prototype, numerals for 6-9 with addition
###########################

#Cahuilla (cah), Fulfulde (fum), Guarani (gua), Noon (noo), Yagua (yag)

for(y in c("cah", "fum", "gua", "noo", "yag")){
  assign(y, c())
  for(x in c(1:5, 10)){
    assign(y, c( eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in 6:9){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("5+", x-5)))))
  }
  for(x in 11:15){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x-10)))))
  }
  for(x in 16:19){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+5+", x%%5)))))
  }
  for(x in c(20, 30, 40, 50)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10,"*10")))))
  }
  for(x in c(60,70,80,90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("(5+", x/10-5, ")*10")))))
  }
  for(x in c(21:25, 31:35, 41:45, 51:55)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10,"*10+", x%%10)))))
  }
  for(x in c(61:65, 71:75, 81:85, 91:95)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("(5+", x%/%10-5, ")", "*10+",x%%10)))))
  }
  for(x in c(26:29, 36:39, 46:49, 56:59)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10,"*10+5+", x%%5)))))
  }
  for(x in c(66:69, 76:79, 86:89, 96:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("(5+", x%/%10-5, ")", "*10+5+",x%%5)))))
  }
 
  natural = c(natural, eval(parse(text = y)))
  
}

###########################
# Type 3-Kannada: numerals for 1-8 and 10, 9 = 10-1
###########################

#Kannada (knd)
# Decimal, but  9 = 10-1
for(y in c("knd")){
  assign(y, c())
  
  for(x in c(1:8,10)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(9)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "10-1"))))
  }
  for(x in c(11:18)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "10+10-1"))))
  }
  for(x in c(20,30,40,50,60,70,80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
  }
  
  for(x in c(90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("(10-1)", "*10")))))
  }
  for(x in c(21:28,31:38,41:48,51:58,61:68,71:78, 81:88)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
  }
  for(x in c(29,39,49,59,69,79,89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+(10-1)")))))
  }
  
  for(x in c(91:98)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("(10-1)*10+", x%%10)))))
  }
  for(x in c(99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "(10-1)*10+(10-1)"))))
  }
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 3-Kayahli: numerals for 1-5 and 10, building 6-9 with multiplication
###########################

#Kayah Li (kyl)
#Decimal, but building 6-9 with multiplication
kyl = c()
for(x in c(1:5, 10)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), x)))
}
for(x in c(6,8)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0(x/2, "*2"))))
}
for(x in c(7,9)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0(x%/%2, "*2+1"))))
}
for(x in c(11:15)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0("10+",x%%10))))
}
for(x in c(21:25, 31:35, 41:45, 51:55)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0(x%/%10, "*10+",x%%10))))
}
for(x in c(16, 18)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0("10+",(x%%10)/2, "*2"))))
}
for(x in c(17, 19)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0("10+",(x%%10)%/%2, "*2+1"))))
}
for(x in c(26, 28, 36, 38, 46, 48, 56, 58)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0(x%/%10, "*10+",(x%%10)/2, "*2"))))
}
for(x in c(27, 29, 37, 39, 47, 49, 57, 59)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0(x%/%10, "*10+",(x%%10)%/%2, "*2+1"))))
}
for(x in c(61:65, 81:85)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0((x%/%10)/2, "*2", "*10+",x%%10))))
}
for(x in c(71:75, 91:95)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0("(", (x%/%10)%/%2, "*2 +1)", "*10+",x%%10))))
}
for(x in c(66, 68, 86, 88)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0((x%/%10)/2, "*2", "*10+",(x%%10)/2, "*2"))))
}
for(x in c(67, 69, 87, 89)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0((x%/%10)/2, "*2", "*10+",(x%%10)%/%2, "*2+1"))))
}
for(x in c(76, 78, 96, 98)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0("(", (x%/%10)%/%2, "*2 +1)", "*10+",(x%%10)/2, "*2"))))
}
for(x in c(77, 79, 97, 99)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0("(", (x%/%10)%/%2, "*2 +1)", "*10+",(x%%10)%/%2, "*2+1"))))
}
for(x in c(20, 30, 40, 50)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0(x/10, "*10"))))
}
for(x in c(60, 80)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0((x%/%10)/2, "*2", "*10"))))
}
for(x in c(70, 90)){
  kyl = c(kyl, list(c("kyl", paste0("word", x), paste0("(", (x%/%10)%/%2, "*2 +1)", "*10"))))
}
natural = c(natural, kyl)

###########################
# Type 3-Quileute: numerals for 1-5 and 10, building 6-7 with +, 8 = 10-2 and 9 = 10-1
###########################

#Quileute (qui)
#Decimal, but building 6-7 with +, 8 = 10-2 and 9 = 10-1
qui = c()
for(x in c(1:5, 10)){
  qui = c(qui, list(c("qui", paste0("word", x), x)))
}
for(x in c(6:7)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("5+", x%%5))))
}
for(x in c(8:9)){
  qui = c(qui, list(c("qui", paste0("word", x),  paste0("10-", 10-x))))
}
for(x in c(11:15)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("10+",x%%10))))
}
for(x in c(21:25, 31:35, 41:45, 51:55)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0(x%/%10, "*10+",x%%10))))
}
for(x in c(61:65, 71:75)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("(5+", x%/%10-5, ")", "*10+",x%%10))))
}
for(x in c(16:17)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("10+5+",x%%5))))
}
for(x in c(18:19)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("10+10-", 20-x))))
}
for(x in c(26:27, 36:37, 46:47, 56:57)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0(x%/%10, "*10+5+",x%%5))))
}
for(x in c(28, 29, 38, 39, 48,49,58,59)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0(x%/%10, "*10+10-", 10-x%%10))))
}
for(x in c(66:67, 76:77)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("(5+", x%/%10-5, ")", "*10+5+",x%%5))))
}
for(x in c(68, 69, 78, 79)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("(5+", x%/%10-5, ")", "*10+10-", 10-x%%10))))
}
for(x in c(20, 30, 40, 50)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0(x/10, "*10"))))
}
for(x in c(60, 70)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("(5+", x/10-5, ")*10"))))
}
for(x in c(80, 90)){
  qui = c(qui, list(c("qui", paste0("word", x),  paste0("(10-", 10-x/10,")*10"))))
}
for(x in c(81:85, 91:95)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("(10-", 10-x%/%10,")*10+",x%%10))))
}
for(x in c(86:87, 96:97)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("(10-", 10-x%/%10,")*10+5+",x%%5))))
}
for(x in c(88:89, 98:99)){
  qui = c(qui, list(c("qui", paste0("word", x), paste0("(10-", 10-x%/%10,")*10+10-", 10-x%%10))))
}
natural = c(natural, qui)



###########################
# Type 3-Khanty: 9 quirky everywhere apart from 89 and 99 (like in Cree), but 20 a sinlge morpheme
###########################
#Khanty
# 9 quirky everywhere apart from 89 and 99 (like in Cree), but 20 a sinlge morpheme
kty = c()
for(x in c(1:8,10, 20)){
  kty = c(kty, list(c("kty", paste0("word", x), x)))
}
for(x in c(9)){
  kty = c(kty, list(c("kty", paste0("word", x), "10-1")))
}
for(x in c(11:18)){
  kty = c(kty, list(c("kty", paste0("word", x), paste0("10+", x%%10))))
}
for(x in c(19)){
  kty = c(kty, list(c("kty", paste0("word", x), "20-1")))
}
for(x in c(30,40,50,60,70,80)){
  kty = c(kty, list(c("kty", paste0("word", x), paste0(x/10, "*10"))))
}
for(x in c(90)){
  kty = c(kty, list(c("kty", paste0("word", x), paste0("(10-1)", "*10"))))
}
for(x in c(21:28)){
  kty = c(kty, list(c("kty", paste0("word", x), paste0("20+", x%%10))))
}
for(x in c(31:38,41:48,51:58,61:68,71:78,81:88)){
  kty = c(kty, list(c("kty", paste0("word", x), paste0(x%/%10, "*10+", x%%10))))
}
for(x in c(29)){
  kty = c(kty, list(c("kty", paste0("word", x), "3*10-1")))
}
for(x in c(39)){
  kty = c(kty, list(c("kty", paste0("word", x), "4*10-1")))
}
for(x in c(49)){
  kty = c(kty, list(c("kty", paste0("word", x), "5*10-1")))
}
for(x in c(59)){
  kty = c(kty, list(c("kty", paste0("word", x), "6*10-1")))
}
for(x in c(69)){
  kty = c(kty, list(c("kty", paste0("word", x), "7*10-1")))
}
for(x in c(79)){
  kty = c(kty, list(c("kty", paste0("word", x), "8*10-1")))
}
for(x in c(89)){
  kty = c(kty, list(c("kty", paste0("word", x), "8*10+(10-1)")))
}
for(x in c(91:98)){
  kty = c(kty, list(c("kty", paste0("word", x), paste0("(10-1)*10+", x%%10))))
}
for(x in c(99)){
  kty = c(kty, list(c("kty", paste0("word", x), "(10-1)*10+(10-1)")))
}
natural = c(natural, kty)

###########################
# Type 3B: 8 = 2x4, 9 = 10-1
###########################
#Nenets (ntu), Tarahumara Western (twe)
# Decimal, but 8 = 2x4, 9 = 10-1
for(y in c("ntu", "twe")){
  assign(y, c())
  
  for(x in c(1:7,10)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(8)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "2*4"))))
  }
  for(x in c(9)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "10-1"))))
  }
  for(x in c(11:17)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(18)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", "2*4")))))
  }
  for(x in c(19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "10+10-1"))))
  }
  for(x in c(20,30,40,50,60,70)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
  }
  for(x in c(80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("(2*4)", "*10")))))
  }
  for(x in c(90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("(10-1)", "*10")))))
  }
  for(x in c(21:27,31:37,41:47,51:57,61:67,71:77)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
  }
  for(x in c(28,38,48,58,68,78)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+2*4")))))
  }
  for(x in c(29,39,49,59,69,79)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+(10-1)")))))
  }
  for(x in c(81:87)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("(2*4)*10+", x%%10)))))
  }
  for(x in c(88)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "(2*4)*10+2*4"))))
  }
  for(x in c(89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "(2*4)*10+(10-1)"))))
  }
  for(x in c(91:97)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("(10-1)*10+", x%%10)))))
  }
  for(x in c(98)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "(10-1)*10+2*4"))))
  }
  for(x in c(99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "(10-1)*10+(10-1)"))))
  }
  natural = c(natural, eval(parse(text = y)))
}



###########################
# Type 3-Kunama: building 6-8 with + and 9 = 10-1
###########################
#Kunama (knm)
#Decimal, but building 6-8 with + and 9 = 10-1
knm = c()
for(x in c(1:5, 10)){
  knm = c(knm, list(c("knm", paste0("word", x), x)))
}
for(x in c(6:8)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0("5+", x%%5))))
}
for(x in c(9)){
  knm = c(knm, list(c("knm", paste0("word", x), "10-1")))
}
for(x in c(11:15)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0("10+",x%%10))))
}
for(x in c(21:25, 31:35, 41:45, 51:55)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0(x%/%10, "*10+",x%%10))))
}
for(x in c(61:65, 71:75, 81:85)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0("(5+", x%/%10-5, ")", "*10+",x%%10))))
}
for(x in c(16:18)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0("10+5+",x%%5))))
}
for(x in c(19)){
  knm = c(knm, list(c("knm", paste0("word", x), "10+10-1")))
}
for(x in c(26:28, 36:38, 46:48, 56:58)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0(x%/%10, "*10+5+",x%%5))))
}
for(x in c(29,39,49,59)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0(x%/%10, "*10+", "10-1"))))
}
for(x in c(66:68, 76:78, 86:88)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0("(5+", x%/%10-5, ")", "*10+5+",x%%5))))
}
for(x in c(69, 79, 89)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0("(5+", x%/%10-5, ")", "*10+10-1"))))
}
for(x in c(20, 30, 40, 50)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0(x/10, "*10"))))
}
for(x in c(60, 70, 80)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0("(5+", x/10-5, ")*10"))))
}
for(x in c(90)){
  knm = c(knm, list(c("knm", paste0("word", x), "(10-1)*10")))
}
for(x in c(91:95)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0("(10-1)*10+",x%%10))))
}
for(x in c(96:98)){
  knm = c(knm, list(c("knm", paste0("word", x), paste0("(10-1)*10+5+",x%%5))))
}
for(x in c(99)){
  knm = c(knm, list(c("knm", paste0("word", x), "(10-1)*10+10-1")))
}
natural = c(natural, knm)

###########################
#
# Type 4: base-20
#
###########################

###########################
# Type 4A: prototype
###########################

#Georgian (geo), Basque (bsq), Burushaski (bur), Chinantec (Lealao) (cle), Irish (iri), Tsez (tsz)
for(y in c("geo", "bsq", "bur", "cle", "iri", "tsz")){
  assign(y, c())
  for(x in c(1:10, 20)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(21:29)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
  }
  for(x in c(30)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+10"))))
  }
  for(x in c(31:39)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+", x%%10)))))
  }
  for(x in c(40, 60, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(41:49, 61:69, 81:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(50, 70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10")))))
  }
  for(x in c(51:59, 71:79, 91:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+",x%%10)))))
  }
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 4-Ingush
###########################

#Ingush (ing)
for(y in c("ing")){
  assign(y, c())
  for(x in c(1:10, 20)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:18)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20-1")))))
  }
  for(x in c(21:29)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
  }
  for(x in c(30)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+10"))))
  }
  for(x in c(31:38)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+", x%%10)))))
  }
  for(x in c(39)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+20-1")))))
  }
  for(x in c(40, 60, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(41:49, 61:69, 81:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(50, 70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10")))))
  }
  for(x in c(51:58, 71:78, 91:98)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+",x%%10)))))
  }
  for(x in c(59, 79, 99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+20-1")))))
  }
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 4-abkhaz
###########################
#Abhkaz
#20 = 2*10, vigesimal.
abk = c()
for(x in c(1:10)){
  abk = c(abk, list(c("abk", paste0("word", x), x)))
}
for(x in c(11:19)){
  abk = c(abk, list(c("abk", paste0("word", x), paste0("10+", x%%10))))
}
for(x in c(20)){
  abk = c(abk, list(c("abk", paste0("word", x), "2*10")))
}
for(x in c(21:29)){
  abk = c(abk, list(c("abk", paste0("word", x), paste0("2*10+", x%%10))))
}
for(x in c(30)){
  abk = c(abk, list(c("abk", paste0("word", x), "2*10+10")))
}
for(x in c(31:39)){
  abk = c(abk, list(c("abk", paste0("word", x), paste0("2*10+10+", x%%10))))
}
for(x in c(40, 60, 80)){
  abk = c(abk, list(c("abk", paste0("word", x), paste0(x/20, "*2*10"))))
}
for(x in c(41:49, 61:69, 81:89)){
  abk = c(abk, list(c("abk", paste0("word", x), paste0(x%/%20, "*2*10+",x%%10))))
}
for(x in c(50, 70, 90)){
  abk = c(abk, list(c("abk", paste0("word", x), paste0(x%/%20, "*2*10+10"))))
}
for(x in c(51:59, 71:79, 91:99)){
  abk = c(abk, list(c("abk", paste0("word", x), paste0(x%/%20, "*2*10+10+",x%%10))))
}
natural = c(natural, abk)


###########################
# Type 4-Huave
###########################
#Huave
#Like Georgian, except 20 = 1*20
hve = c()
for(x in c(1:10)){
  hve = c(hve, list(c("hve", paste0("word", x), x)))
}
for(x in c(11:19)){
  hve = c(hve, list(c("hve", paste0("word", x), paste0("10+", x%%10))))
}
for(x in c(20)){
  hve = c(hve, list(c("hve", paste0("word", x), "1*20")))
}
for(x in c(21:29)){
  hve = c(hve, list(c("hve", paste0("word", x), paste0("1*20+", x%%10))))
}
for(x in c(30)){
  hve = c(hve, list(c("hve", paste0("word", x), "1*20+10")))
}
for(x in c(31:39)){
  hve = c(hve, list(c("hve", paste0("word", x), paste0("1*20+10+", x%%10))))
}
for(x in c(40, 60, 80)){
  hve = c(hve, list(c("hve", paste0("word", x), paste0(x/20, "*20"))))
}
for(x in c(41:49, 61:69, 81:89)){
  hve = c(hve, list(c("hve", paste0("word", x), paste0(x%/%20, "*20+",x%%10))))
}
for(x in c(50, 70, 90)){
  hve = c(hve, list(c("hve", paste0("word", x), paste0(x%/%20, "*20+10"))))
}
for(x in c(51:59, 71:79, 91:99)){
  hve = c(hve, list(c("hve", paste0("word", x), paste0(x%/%20, "*20+10+",x%%10))))
}
natural = c(natural, hve)

###########################
# Type 4-Lezgian
###########################
#Lezgian. Like Georgian, but 40 morphologically simple
for(y in c("lez")){
  assign(y, c())
  for(x in c(1:10, 20, 40)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(21:29)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
  }
  for(x in c(41:49)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("40+", x%%10)))))
  }
  for(x in c(30)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+10"))))
  }
  for(x in c(31:39)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+", x%%10)))))
  }
  for(x in c(50)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "40+10"))))
  }
  for(x in c(51:59)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("40+10+", x%%10)))))
  }
  for(x in c(60, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(61:69, 81:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10")))))
  }
  for(x in c(71:79, 91:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+",x%%10)))))
  }
  natural = c(natural, eval(parse(text = y)))
}



###########################
# Type 4-MixtecA
###########################
#Mixtec (Atatlahuca)
#Vigesimal, but with the quirk that 15 is single morpheme 
for(y in c("mxa")){
  assign(y, c())
  for(x in c(1:10, 15, 20)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:14)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(16:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("15+", x%%5)))))
  }
  for(x in c(21:29)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
  }
  for(x in c(30)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+10"))))
  }
  for(x in c(31:34)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+", x%%10)))))
  }
  for(x in c(35)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+15"))))
  }
  for(x in c(36:39)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+15+",x%%5 )))))
  }
  for(x in c(40, 60, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(41:49, 61:69, 81:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(50, 70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10")))))
  }
  for(x in c(51:54, 71:74, 91:94)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+",x%%10)))))
  }
  for(x in c(55, 75, 95)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+15")))))
  }
  for(x in c(56:59, 76:79, 96:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+15+",x%%5)))))
  }
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 4-MixtecC
###########################
#Mixtec (Chalcatongo)
#Vigesimal, but with the quirk that 15 is single morpheme, and 60 = 2*20 + 20
for(y in c("mxc")){
  assign(y, c())
  for(x in c(1:10, 15, 20)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:14)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(16:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("15+", x%%5)))))
  }
  for(x in c(21:29)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
  }
  for(x in c(30)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+10"))))
  }
  for(x in c(31:34)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+", x%%10)))))
  }
  for(x in c(35)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+15"))))
  }
  for(x in c(36:39)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+15+",x%%5 )))))
  }
  for(x in c(40, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(41:49, 81:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(60)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "2*20+20"))))
  }
  for(x in c(61:69)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("2*20+20+",x%%10)))))
  }
  for(x in c(50, 70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10")))))
  }
  for(x in c(51:54, 71:74, 91:94)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+",x%%10)))))
  }
  for(x in c(55, 75, 95)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+15")))))
  }
  for(x in c(56:59, 76:79, 96:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+15+",x%%5)))))
  }

  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 4-Yucatec
###########################
#Yucatec
#Like Georgian, but 11 quirky monomorphemic, + 20 = 1*20,10 = 1*10
yct = c()
for(x in c(1:9, 11)){
  yct = c(yct, list(c("yct", paste0("word", x), x)))
}
for(x in c(12:19)){
  yct = c(yct, list(c("yct", paste0("word", x), paste0("10+", x%%10))))
}
for(x in c(10,20)){
  yct = c(yct, list(c("yct", paste0("word", x), paste0("1*", x))))
}
for(x in c(21:29)){
  yct = c(yct, list(c("yct", paste0("word", x), paste0("1*20+", x%%10))))
}
for(x in c(30)){
  yct = c(yct, list(c("yct", paste0("word", x), "1*20+1*10")))
}
for(x in c(31)){
  yct = c(yct, list(c("yct", paste0("word", x), "1*20+11")))
}
for(x in c(32:39)){
  yct = c(yct, list(c("yct", paste0("word", x), paste0("1*20+10+", x%%10))))
}
for(x in c(40, 60, 80)){
  yct = c(yct, list(c("yct", paste0("word", x), paste0(x/20, "*20"))))
}
for(x in c(41:49, 61:69, 81:89)){
  yct = c(yct, list(c("yct", paste0("word", x), paste0(x%/%20, "*20+",x%%10))))
}
for(x in c(50, 70, 90)){
  yct = c(yct, list(c("yct", paste0("word", x), paste0(x%/%20, "*20+1*10"))))
}
for(x in c(52:59, 72:79, 92:99)){
  yct = c(yct, list(c("yct", paste0("word", x), paste0(x%/%20, "*20+10+",x%%10))))
}
for(x in c(51)){
  yct = c(yct, list(c("yct", paste0("word", x), "2*20+11")))
}
for(x in c(71)){
  yct = c(yct, list(c("yct", paste0("word", x), "3*20+11")))
}
for(x in c(91)){
  yct = c(yct, list(c("yct", paste0("word", x), "4*20+11")))
}
natural = c(natural, yct)


###########################
# Type 4-French
###########################
#French
fre = c()
for(x in c(1:10, 20)){
  fre = c(fre, list(c("fre", paste0("word", x), x)))
}
for(x in 11:19){
  fre = c(fre, list(c("fre", paste0("word", x), paste0("10+", x%%10))))
}
for(x in c(30,40,50,60)){
  fre = c(fre, list(c("fre", paste0("word", x), paste0(x/10,"*10"))))
}
for(x in c(70)){
  fre = c(fre, list(c("fre", paste0("word", x), "6*10 +10")))
}
for(x in c(80)){
  fre = c(fre, list(c("fre", paste0("word", x), "4*20")))
}

for(x in c(90)){
  fre = c(fre, list(c("fre", paste0("word", x), "4*20 + 10")))
}
for(x in c(31:39,41:49,51:59,61:69)){
  fre = c(fre, list(c("fre", paste0("word", x), paste0(x%/%10,"*10+", x%%10))))
}
for(x in 21:29){
  fre = c(fre, list(c("fre", paste0("word", x), paste0("20+", x%%10))))
}

for(x in c(71:79)){
  fre = c(fre, list(c("fre", paste0("word", x), paste0("6*10 +10 +", x%%10))))
}

for(x in c(81:89)){
  fre = c(fre, list(c("fre", paste0("word", x), paste0("4*20 +", x%%10))))
}
for(x in c(91:99)){
  fre = c(fre, list(c("fre", paste0("word", x), paste0("4*20 + 10 +", x%%10))))
}
natural = c(natural, fre)


###########################
#
# Type 4: base-20, but not
#
###########################

###########################
# Type 5A: prototype
###########################

#Otomi Mezquital (otm)
#Vigesimal, like Gola, but 20 = 1*20 across the board
for(y in c("otm")){
  assign(y, c())
  for(x in c(1:5, 10)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(6:9)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("5+", x%%5)))))
  }
  for(x in c(11:15)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(16:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+5+", x%%5)))))
  }
  for(x in c(20, 40, 60, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(21:25, 41:45, 61:65, 81:85)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(26:29, 46:49, 66:69, 86:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+5+",x%%5)))))
  }
  for(x in c(30, 50, 70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10")))))
  }
  for(x in c(31:35, 51:55, 71:75, 91:95)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+",x%%10)))))
  }
  for(x in c(36:39, 56:59, 76:79, 96:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+5+",x%%5)))))
  }
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 5-Gola
###########################

#Gola, 
#Vigesimal, building 5-9
for(y in c("gol")){
  assign(y, c())
  for(x in c(1:5, 10)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(6:9)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("5+", x%%5)))))
  }
  for(x in c(11:15)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(16:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+5+", x%%5)))))
  }
  for(x in c(21:25)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
  }
  for(x in c(26:29)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+5+", x%%5)))))
  }
  for(x in c(30)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+10"))))
  }
  for(x in c(31:35)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+", x%%10)))))
  }
  for(x in c(36:39)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+5+",x%%5 )))))
  }
  for(x in c(20, 40, 60, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(41:45, 61:65, 81:85)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(46:49, 66:69, 86:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+5+",x%%5)))))
  }
  for(x in c(50, 70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10")))))
  }
  for(x in c(51:55, 71:75, 91:95)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+",x%%10)))))
  }
  for(x in c(56:59, 76:79, 96:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+5+",x%%5)))))
  }
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 5-Arawak
###########################
#Arawak (ara)
#Vigesimal, like Gola, but 10 = 2*5, 15 = 2*5 + 5 and 5=1*5 
for(y in c("ara")){
  assign(y, c())
  for(x in c(1:4)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(6:9)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("5+", x%%5)))))
  }
  for(x in c(5)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "1*5"))))
  }
  for(x in c(10)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "2*5"))))
  }
  for(x in c(11:15)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("2*5+", x%%10)))))
  }
  for(x in c(16:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("2*5+5+", x%%5)))))
  }
  for(x in c(20, 40, 60, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(21:24, 41:44, 61:64, 81:84)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(25, 45, 65, 85)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+1*",x%%10)))))
  }
  for(x in c(26:29, 46:49, 66:69, 86:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+5+",x%%5)))))
  }
  for(x in c(30, 50, 70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+2*5")))))
  }
  for(x in c(31:35, 51:55, 71:75, 91:95)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+2*5+",x%%10)))))
  }
  for(x in c(36:39, 56:59, 76:79, 96:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+2*5+5+",x%%5)))))
  }
  natural = c(natural, eval(parse(text = y)))
}


###########################
# Type 5-Kana
###########################


#Kana (kan)
#Vigesimal, but building 7-9 (similar to Gola, which builds 6-9)
kan = c()
for(x in c(1:6, 10, 20)){
  kan = c(kan, list(c("kan", paste0("word", x), x)))
}
for(x in c(7:9)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0("5+", x%%5))))
}
for(x in c(11:16)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0("10+", x%%10))))
}
for(x in c(17:19)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0("10+5+", x%%5))))
}
for(x in c(21:26)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0("20+", x%%10))))
}
for(x in c(27:29)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0("20+5+", x%%5))))
}
for(x in c(30)){
  kan = c(kan, list(c("kan", paste0("word", x), "20+10")))
}
for(x in c(31:36)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0("20+10+", x%%10))))
}
for(x in c(37:39)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0("20+10+5+",x%%5 ))))
}
for(x in c(40, 60, 80)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0(x/20, "*20"))))
}
for(x in c(41:46, 61:66, 81:86)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0(x%/%20, "*20+",x%%10))))
}
for(x in c(47:49, 67:69, 87:89)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0(x%/%20, "*20+5+",x%%5))))
}
for(x in c(50, 70, 90)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0(x%/%20, "*20+10"))))
}
for(x in c(51:56, 71:76, 91:96)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0(x%/%20, "*20+10+",x%%10))))
}
for(x in c(57:59, 77:79, 97:99)){
  kan = c(kan, list(c("kan", paste0("word", x), paste0(x%/%20, "*20+10+5+",x%%5))))
}
natural = c(natural, kan)


###########################
# Type 5-Nahuatl
###########################
#Nahuatl - Sierra de Zacapoaxtla (nsz)
#Vigesimal, but with the quirk that 15 is single morpheme, and 6-9 compound, like Diola; unlike Diola, 20 = 1*20
for(y in c("nsz")){
  assign(y, c())
  for(x in c(1:5, 10, 15)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(6:9)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("5+", x%%5)))))
  }
  for(x in c(11:14)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(16:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("15+", x%%5)))))
  }
  for(x in c(20, 40, 60, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(21:25, 41:45, 61:65, 81:85)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(26:29, 46:49, 66:69, 86:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+5+",x%%5)))))
  }
  for(x in c(30, 50, 70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10")))))
  }
  for(x in c(31:34, 51:54, 71:74, 91:94)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+",x%%10)))))
  }
  for(x in c(35, 55, 75, 95)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+15")))))
  }
  for(x in c(36:39, 56:59, 76:79, 96:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+15+",x%%5)))))
  }
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 5-Diola
###########################
#Diola-Fogny (dio)
#Vigesimal, but with the quirk that 15 is single morpheme, and 6-9 compound
for(y in c("dio")){
  assign(y, c())
  for(x in c(1:5, 10, 15, 20)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(6:9)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("5+", x%%5)))))
  }
  for(x in c(11:14)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(16:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("15+", x%%5)))))
  }
  for(x in c(21:25)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
  }
  for(x in c(26:29)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+5+", x%%5)))))
  }
  for(x in c(30)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+10"))))
  }
  for(x in c(31:34)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+", x%%10)))))
  }
  for(x in c(35)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+15"))))
  }
  for(x in c(36:39)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+15+",x%%5 )))))
  }
  for(x in c(40, 60, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(41:45, 61:65, 81:85)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(46:49, 66:69, 86:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+5+",x%%5)))))
  }
  for(x in c(50, 70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10")))))
  }
  for(x in c(51:54, 71:74, 91:94)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+",x%%10)))))
  }
  for(x in c(55, 75, 95)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+15")))))
  }
  for(x in c(56:59, 76:79, 96:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+15+",x%%5)))))
  }
  natural = c(natural, eval(parse(text = y)))
}

###########################
# Type 5-Supyire
###########################
#Supyire (sup)
#Vigesimal, building 5-9, 80 monomorphemic
for(y in c("sup")){
  assign(y, c())
  for(x in c(1:5, 10, 20, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(6:9)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("5+", x%%5)))))
  }
  for(x in c(11:15)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(16:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+5+", x%%5)))))
  }
  for(x in c(21:25)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+", x%%10)))))
  }
  for(x in c(26:29)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+5+", x%%5)))))
  }
  for(x in c(30)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "20+10"))))
  }
  for(x in c(31:35)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+", x%%10)))))
  }
  for(x in c(36:39)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("20+10+5+",x%%5 )))))
  }
  for(x in c(40, 60)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(41:45, 61:65)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(81:85)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("80+",x%%10)))))
  }
  for(x in c(46:49, 66:69)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+5+",x%%5)))))
  }
  for(x in c(86:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("80+5+",x%%5)))))
  }
  for(x in c(50, 70)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10")))))
  }
  for(x in c(90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "80+10"))))
  }
  for(x in c(51:55, 71:75)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+",x%%10)))))
  }
  for(x in c(91:95)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("80+10+",x%%10)))))
  }
  for(x in c(56:59, 76:79)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+10+5+",x%%5)))))
  }
  for(x in c(96:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("80+10+5+",x%%5)))))
  }
  natural = c(natural, eval(parse(text = y)))
}



###########################
# Type 5-Drehu
###########################

#Drehu (dre)
#Vigesimal, with a bunch of quirks
dre = c()
for(x in c(1:4)){
  dre = c(dre, list(c("dre", paste0("word", x), x)))
}
for(x in c(5)){
  dre = c(dre, list(c("dre", paste0("word", x), "1*5")))
}
for(x in c(6:9)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0("5+", x%%5))))
}
for(x in c(10)){
  dre = c(dre, list(c("dre", paste0("word", x), "2*5")))
}
for(x in c(11:14)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0("10+", x%%10))))
}
for(x in c(15)){
  dre = c(dre, list(c("dre", paste0("word", x), "3*5")))
}
for(x in c(16:19)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0("15+", x%%5))))
}
for(x in c(20)){
  dre = c(dre, list(c("dre", paste0("word", x), "1*20")))
}
for(x in c(21:24)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0("1*20+", x%%10))))
}
for(x in c(25)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0("1*20+","1*5"))))
}
for(x in c(26:29)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0("1*20+5+", x%%5))))
}
for(x in c(30)){
  dre = c(dre, list(c("dre", paste0("word", x), "1*20+2*5")))
}
for(x in c(31:34)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0("1*20+10+", x%%10))))
}
for(x in c(35)){
  dre = c(dre, list(c("dre", paste0("word", x), "1*20+15")))
}
for(x in c(36:39)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0("1*20+15+",x%%5 ))))
}
for(x in c(40, 60, 80)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0(x/20, "*20"))))
}
for(x in c(41:44, 61:64, 81:84)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0(x%/%20, "*20+",x%%10))))
}
for(x in c(45, 65, 85)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0(x%/%20, "*20+1*5"))))
}
for(x in c(46:49, 66:69, 86:89)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0(x%/%20, "*20+5+",x%%5))))
}
for(x in c(50, 70, 90)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0(x%/%20, "*20+2*5"))))
}
for(x in c(51:54, 71:74, 91:94)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0(x%/%20, "*20+10+",x%%10))))
}
for(x in c(55, 75, 95)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0(x%/%20, "*20+15"))))
}
for(x in c(56:59, 76:79, 96:99)){
  dre = c(dre, list(c("dre", paste0("word", x), paste0(x%/%20, "*20+15+",x%%5))))
}
natural = c(natural, dre)

###########################
# Type 5-Mangap-Mbula
###########################
#Mangap-Mbula 
for(y in c("mmb")){
  assign(y, c())
  for(x in c(1:4)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(6:9)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("1*5+", x%%5)))))
  }
  for(x in c(11:14)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("2*5+", x%%10)))))
  }
  for(x in c(5, 10)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/5, "*5")))))
  }
  for(x in c(15)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("(2+1)","*5")))))
  }
  for(x in c(16:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("3*5+", x%%5)))))
  }
  for(x in c(20, 40, 60, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/20, "*20")))))
  }
  for(x in c(21:24, 41:44, 61:64, 81:84)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+",x%%10)))))
  }
  for(x in c(25, 45, 65, 85)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+1*",x%%10)))))
  }
  for(x in c(26:29, 46:49, 66:69, 86:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+1*5+",x%%5)))))
  }
  for(x in c(30, 50, 70, 90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+2*5")))))
  }
  for(x in c(31:34, 51:54, 71:74, 91:94)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+2*5+",x%%10)))))
  }
  for(x in c(35, 55, 75, 95)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+(2+1)*5")))))
  }
  for(x in c(36:39, 56:59, 76:79, 96:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%20, "*20+(2+1)*5+",x%%5)))))
  }
  natural = c(natural, eval(parse(text = y)))
}



###########################
#
# Type 6: other bases
#
###########################


###########################
# Type 6-Haida
###########################
#Haida
# 9 = 10-1. Mix of decimal and vigesimal
hai = c()
for(x in c(1:8,10)){
  hai = c(hai, list(c("hai", paste0("word", x), x)))
}
for(x in c(9)){
  hai = c(hai, list(c("hai", paste0("word", x), "10-1")))
}
for(x in c(11:18)){
  hai = c(hai, list(c("hai", paste0("word", x), paste0("10+", x%%10))))
}
for(x in c(19)){
  hai = c(hai, list(c("hai", paste0("word", x), "10+(10-1)")))
}
for(x in c(20,40,60,80)){
  hai = c(hai, list(c("hai", paste0("word", x), paste0(x/20, "*20"))))
}
for(x in c(30,50,70)){
  hai = c(hai, list(c("hai", paste0("word", x), paste0(x/10, "*10"))))
}
for(x in c(90)){
  hai = c(hai, list(c("hai", paste0("word", x), paste0("(10-1)", "*10"))))
}
for(x in c(21:28,41:48,61:68,81:88)){
  hai = c(hai, list(c("hai", paste0("word", x), paste0(x%/%20, "*20+", x%%10))))
}
for(x in c(31:38,51:58,71:78)){
  hai = c(hai, list(c("hai", paste0("word", x), paste0(x%/%10, "*10+", x%%10))))
}
for(x in c(29)){
  hai = c(hai, list(c("hai", paste0("word", x), "1*20+(10-1)")))
}
for(x in c(39)){
  hai = c(hai, list(c("hai", paste0("word", x), "3*10+(10-1)")))
}
for(x in c(49)){
  hai = c(hai, list(c("hai", paste0("word", x), "2*20+(10-1)")))
}
for(x in c(59)){
  hai = c(hai, list(c("hai", paste0("word", x), "5*10+(10-1)")))
}
for(x in c(69)){
  hai = c(hai, list(c("hai", paste0("word", x), "3*20+(10-1)")))
}
for(x in c(79)){
  hai = c(hai, list(c("hai", paste0("word", x), "7*10+(10-1)")))
}
for(x in c(89)){
  hai = c(hai, list(c("hai", paste0("word", x), "4*20+(10-1)")))
}
for(x in c(91:98)){
  hai = c(hai, list(c("hai", paste0("word", x), paste0("(10-1)*10+", x%%10))))
}
for(x in c(99)){
  hai = c(hai, list(c("hai", paste0("word", x), "(10-1)*10+(10-1)")))
}
natural = c(natural, hai)

###########################
# Type 6-Kilivila
###########################
#Kilivila (klv)
#Decimal, but building 6-9, 10 and 20 single morphemes, 100 = 1*100
klv = c()
for(x in c(1:5)){
  klv = c(klv, list(c("klv", paste0("word", x), x)))
}
for(x in c(6:9)){
  klv = c(klv, list(c("klv", paste0("word", x), paste0("5+", x%%5))))
}
for(x in c(11:15, 21:25, 31:35, 41:45, 51:55)){
  klv = c(klv, list(c("klv", paste0("word", x), paste0(x%/%10, "*10+",x%%10))))
}
for(x in c(16:19, 26:29, 36:39, 46:49, 56:59)){
  klv = c(klv, list(c("klv", paste0("word", x), paste0(x%/%10, "*10+5+",x%%5))))
}
for(x in c(10, 20, 30, 40, 50)){
  klv = c(klv, list(c("klv", paste0("word", x), paste0(x/10, "*10"))))
}
for(x in c(60, 70, 80, 90)){
  klv = c(klv, list(c("klv", paste0("word", x), paste0("5*10+", (x-50)/10, "*10"))))
}
for(x in c(61:65, 71:75, 81:85, 91:95)){
  klv = c(klv, list(c("klv", paste0("word", x), paste0("5*10+", (x - x%%10 -50)/10, "*10+",x%%10))))
}
for(x in c(66:69, 76:79, 86:89, 96:99)){
  klv = c(klv, list(c("klv", paste0("word", x), paste0("5*10+", (x - x%%10 -50)/10, "*10+5+",x%%5))))
}
natural = c(natural, klv)





###########################
# Type 6-Tommo So
###########################

#Tommo So (tms)
#Like Mandarin up to 80, then 80+10 = 90, 100 = 80 +20 etc
for(y in c("tms")){
  assign(y, c())
  for(x in c(1:10, 80)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), x))))
  }
  for(x in c(11:19)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("10+", x%%10)))))
  }
  for(x in c(20,30,40,50,60,70)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x/10, "*10")))))
  }
  for(x in c(90)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), "80+10"))))
  }
  for(x in c(21:29,31:39,41:49,51:59,61:69,71:79)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0(x%/%10, "*10+", x%%10)))))
  }
  for(x in c(81:89)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("80+", x%%10)))))
  }
  for(x in c(91:99)){
    assign(y, c(eval(parse(text = y)), list(c(y, paste0("word", x), paste0("80+10+", x%%10)))))
  }
  natural = c(natural, eval(parse(text = y)))
  
}



###########################
# Type 6-Khalaj
###########################

#Khalaj (khl)
khl = c()
for(x in c(1:10, 20,30,40,50)){
  khl = c(khl, list(c("khl", paste0("word", x), x)))
}
for(x in c(11:19)){
  khl = c(khl, list(c("khl", paste0("word", x), paste0("10+", x%%10))))
}
for(x in c(21:29)){
  khl = c(khl, list(c("khl", paste0("word", x), paste0("20+", x%%10))))
}
for(x in c(31:39)){
  khl = c(khl, list(c("khl", paste0("word", x), paste0("30+", x%%10))))
}
for(x in c(41:49)){
  khl = c(khl, list(c("khl", paste0("word", x), paste0("40+", x%%10))))
}
for(x in c(51:59)){
  khl = c(khl, list(c("khl", paste0("word", x), paste0("50+", x%%10))))
}
for(x in c(60,70,80,90)){
  khl = c(khl, list(c("khl", paste0("word", x), paste0("50+", x-50))))
}
for(x in c(61:69,71:79,81:89,91:99)){
  khl = c(khl, list(c("khl", paste0("word", x), paste0("50+", (x%/%10-5)*10, "+", x%%10))))
}
natural = c(natural, khl)


# Create a df from a list of vectors
natural_df = as.data.frame(do.call(rbind, natural))
colnames(natural_df) <- c("language", "word", "morphology")
length(unique(natural_df$language))

natural_df$extension = 0

for(i in 1:length(natural_df$morphology)){
  natural_df[['extension']][i] = eval(parse(text = as.character(natural_df[['morphology']][i])))
}

# Generate fake languages and their items
natural_filename = paste0(Folder, "natural_languages.csv")
write.csv(natural_df, natural_filename, row.names=FALSE)