library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rje)
library(rlist)
library(stringr)
library(purrr)

####################################
# Useful functions
####################################
# Useful version of as.numeric (as.numeric annoyingly can change values if applied to factors...)
#Count number-denoting morphemes in an expression
morpheme_count = function(x){
  complexity = 0
  for(n in 1:99){#nb: condition around n makes sure e.g. 1s are not counted in 11
    k = lengths(regmatches(x, gregexpr(paste0('(?<![0-9])', n, '(?![0-9])'), x, perl = TRUE)))
    complexity = complexity + k
  }
  return(complexity)
  
}

#Find morphemes in an expression
morpheme_found = function(x){
  morphemes = c()
  for(n in 1:99){#nb: condition around n makes sure e.g. 1s are not counted in 11
    found = unlist(regmatches(x, gregexpr(paste0('(?<![0-9])', n, '(?![0-9])'), x, perl = TRUE))) #kad ne nadje match, vrati listu sa empty character vector, zbog toga unlist
    if(length(found) > 0){
    morphemes = c(morphemes, found)}
  }
  return(morphemes)
  
}

#Count morphemes in a language
language_morphemes = function(x){
language_id = subset(natural_lang, language == x)
all_morphemes = c() 
for(i in language_id$morphology){
  morphemes_i = morpheme_found(i)
  all_morphemes = c(all_morphemes, morphemes_i)
}
all_morphemes = unique(all_morphemes)
word_n = length(all_morphemes)
return(word_n)
}

# Compute the prior for a given number: power-law basic
prior_sum = 0 #for normalization of prior probas
for(i in 1:99){
  prior_sum = prior_sum + i**(-2)
}

probaf <- function(state){
  number = as.numeric(state)
  return((number**(-2))/prior_sum)
}



####################################
# Number of morphemes in each expression in each natural languages
####################################
natural_lang = read.csv('../data/natural_languages.csv', header=TRUE)
natural_lang$complexity = 2*morpheme_count(natural_lang$morphology)-1 #number_morpheme_count + number of arithmetic operators

####################################
# Total morpheme number within natural languages
####################################
languages = unique(natural_lang["language"])
languages$word_n = lapply(languages$language, language_morphemes)

natural_lang = merge(natural_lang, languages, by = c("language"))

####################################
# Average utterance length within natural languages
####################################
natural_lang$weighted_complexity = probaf(as.character(natural_lang$extension))*natural_lang$complexity

natural_lang_complexity = natural_lang%>%
  group_by(language, word_n) %>% 
  summarise(expected_complexity = sum(weighted_complexity))


####################################
#Does average utterance length decrease with lexicon size?
####################################

natural_lang_complexity$word_n = as.numeric(as.character(natural_lang_complexity$word_n))
trade_off <- lm(expected_complexity ~ word_n, data=natural_lang_complexity)
summary(trade_off)

####################################
#Save natural language complexity file
####################################
natural_lang_complexity <- apply(natural_lang_complexity,2,as.character)
write.csv(natural_lang_complexity,'../data/natural_lang_complexity_measures.csv', row.names=FALSE)

