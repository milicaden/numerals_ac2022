options(scipen = 999)


####################################
# Import languages and LoT descriptions
####################################
Folder = "../data/"
for(i in 0:100){
  assign(paste0("evo_full_generation_",i), read.csv(paste0("../data/evo_full_generation_",i, ".csv"), header = TRUE))
}

artificial_languages = evo_full_generation_0
for(i in 1:100){
  artificial_languages = rbind(artificial_languages, eval(parse(text = paste0("evo_full_generation_",i))))
}

length(unique(artificial_languages$language))
artificial_languages = artificial_languages[c("language", "word_n", "expected_complexity")]
artificial_languages = unique(artificial_languages)
artificial_languages$type = "artificial"

natural_languages = read.csv("../data/natural_lang_complexity_measures.csv", header = TRUE)
natural_languages$type = "natural"


all_languages = rbind(artificial_languages, natural_languages)

####################################
# PARETO FRONT
####################################
# Import the estimated dominant languages file
dominant = read.csv(paste0(Folder, "pareto_dominant.csv"), header = TRUE)
dominant$type ="dominant"

smoothPareto <- data.frame(
  with(dominant, 
       spline(word_n, expected_complexity, xout = seq(0.1, 30, by = 0.001))
  ),
  method = "spline()"
)
names(smoothPareto)[names(smoothPareto) == 'x'] <- 'word_n'
names(smoothPareto)[names(smoothPareto) == 'y'] <- 'expected_complexity'

####################################
# Plots
####################################
### Main plot
png("Natural-artificial-final.png", width = 140, height = 90, units='mm', res = 300)

p <- ggplot(all_languages, aes(x=word_n, y=expected_complexity)) +
  scale_color_manual(name="Language", values=c('#00ba92','#ff5a1d'))+
  scale_shape_manual(name="Language", values=c(17, 16))+
  geom_point(aes(shape=type, color=type), position = "jitter", alpha = 0.7, size = 1.5) +  xlab("Lexicon size (as number of lexicalized concepts)") + ylab("Average utterance length")+
  xlim(0,30) + ylim(0.5,10)
print(p)

dev.off()

# Add Pareto front to the plot
png("Pareto-morphosyntax-final.png", width = 140, height = 90, units='mm', res = 300)
p2 <- p + 
  geom_line(data = smoothPareto, size = 0.5) 
print(p2)
dev.off()



### First_n analysis plot

#Load the data
multiplicatives_test_languages = read.csv(paste0("../data/evo_full_generation_","the_sample", ".csv"), header = TRUE, colClasses = "character")
multiplicatives_test_languages = multiplicatives_test_languages[c("language", "word_n", "expected_complexity", "digs", "multis")]
multiplicatives_test_languages = unique(multiplicatives_test_languages)
multiplicatives_test_languages$word_n = as.numeric(multiplicatives_test_languages$word_n)
multiplicatives_test_languages$expected_complexity = as.numeric(multiplicatives_test_languages$expected_complexity)
multiplicatives_test_languages$type = "artificial_first_n"

all_languages_analysis2 = rbind(all_languages, multiplicatives_test_languages[, c("language", "word_n", "expected_complexity", "type")])
all_languages_analysis2$type = as.factor(all_languages_analysis2$type)
levels(all_languages_analysis2$type)[levels(all_languages_analysis2$type)=="artificial"] <- "artificial_evo_alg"

p <- ggplot(all_languages_analysis2  %>% arrange(type), aes(x=word_n, y=expected_complexity)) +
  scale_color_manual(name="Language", values=c('#00ba92', '#4D62CB', '#ff5a1d'))+
  scale_shape_manual(name="Language", values=c(17, 15, 16))+
  geom_point(aes(shape=type, color=type), position = "jitter", alpha = 0.7, size = 1.5, order=order) +  xlab("Number of lexicalized concepts") + ylab("Expected morphosyntactic complexity")+
  xlim(0,30) + ylim(0.5,10)
print(p)

#Add Pareto there as well
png("Pareto-morphosyntax-multiplicatives.png", width = 140, height = 90, units='mm', res = 300)
p2 <- p + 
  geom_line(data = smoothPareto, size = 0.5) 
print(p2)
dev.off()



