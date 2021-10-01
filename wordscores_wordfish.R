## Get the packages running
library(quanteda)
library(readtext)
library(ggplot2)

## Change the working directory, read in the texts, create a corpus, and look at it
setwd("~/Desktop/txt files/yearly")
myspeech <- readtext("*.txt", docvarsfrom="filenames")
myCorpus <- corpus(myspeech)
summary(myCorpus)
ntoken(myCorpus)

sum(ntoken(myCorpus))
sum(nsentence(myCorpus))


## Create dfm's, removing numbers, stemming, removing standard English stopwords and two Senate-specific phrases
speechDfm <- dfm(myCorpus, remove_numbers=TRUE, stem=TRUE, tolower = TRUE, remove_punct =T,
                 remove = c(stopwords("english"), "china", "chinese", "beijing", "peking", "will", "can", "must"))

sum(ntoken(speechDfm))
sum(ntype(speechDfm))

# frequency plots
features_dfm <- textstat_frequency(speechDfm, n = 20)

# Sort by reverse frequency order
features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))

ggplot(features_dfm, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# wordclout
textplot_wordcloud(speechDfm)

# lexical dispersion plot
# visualisatoin
textplot_xray(
  kwic(myCorpus, pattern = "peace"),
  kwic(myCorpus, pattern = "free"),
  kwic(myCorpus, pattern = "communist")
)

# absolute scale
textplot_xray(
  kwic(myCorpus, pattern = "peace"),
  kwic(myCorpus, pattern = "free"),
  kwic(myCorpus, pattern = "communist"),
  scale = "absolute"
)

## keyness
# Calculate keyness and determine Trump as target group
result_keyness <- textstat_keyness(speechDfm)

# Plot estimated word keyness
textplot_keyness(result_keyness, show_reference = F) 



############# Wordscore ##############
## Run Wordscore model, assigning Santorum's first speech a score of 1 and Wellstone's a score of -1
wsFitted <- textmodel_wordscores(speechDfm, c(-1.00, rep(NA, 34), 1))
?rep
summary(wsFitted)

## Show the Wordscores; note that the reference texts have been rescaled down closer to 0
# pred <- predict(wsFitted)
# pred
# pred2 <- predict(wsFitted, rescaling = "lbg")
# pred2

pred3 <- predict(wsFitted, rescaling = "mv")
pred3

# The following code is doing this whole process with smoothing argument added to the modelling (it is preferable to just simply add the smoothing argument to your text modelling. It just means that if the smoothing assumptions are held you will be able to get more infomration and have more flexible and robust estimations.) So let's run the wordscore model just like we did above but this time with smoothing. 
wsFittedSm <- textmodel_wordscores(speechDfm, c(-1.00, rep(NA, 34), 1), smooth = 1)
summary(wsFittedSm)

predsm <- predict(wsFittedSm, rescaling = "mv")
predsm

# Then with the code below you can also check the correlation of our predictions using smoothing or not.:
cor(pred3, predsm)
#As you can see it is almost 1, which means they are almost the same. 

# Visualisatoin
textplot_scale1d(predsm, doclabels = years)

textplot_scale1d(wsFittedSm)
textplot_scale1d(wsFitted)

textplot_scale1d(pred3)
textplot_scale1d(predsm)

## change x-axis and y-axis
df <- data.frame(pred3, row.names = c(1950, 1951, 1952, 1957, 1958, 1960, 1961, 1969, 1971, 1972, 1973, 1974, 
                  1975, 1977, 1978, 1979, 1984, 1986, 1989, 1990, 1991, 1993, 1996, 1997, 1998, 1999, 
                 2000, 2003, 2006, 2010, 2011, 2013, 2014, 2015, 2016, 2017), check.rows = TRUE)

years <- rep(row.names.data.frame(df))
document_position <- rep(pred3)

p <- ggplot(df, aes(x=years, y= document_position))
p +geom_bar(stat = "identity")



############# Wordfish ################
wf <- textmodel_wordfish(speechDfm, dir = c(6,5))
summary(wf)

wf2 <- textmodel_wordfish(speechDfm, dir = c(6,5), dispersion = ("quasipoisson"))
summary(wf2)

doclab <- paste(docvars(speechDfm))
textplot_scale1d(wf, doclabels = years)
textplot_scale1d(wf)
textplot_scale1d(wf2, doclabels = years)

textplot_wordcloud(speechDfm)

textplot_scale1d(wf2, margin = "features", 
                 highlighted = c("communist", "world", "people", 
                                 "economy", "friend", "enemy"), 
                 highlighted_color = "red")


## change x-axis and y-axis
df_wf <- data.frame(wf[["se.theta"]], row.names = c(1950, 1951, 1952, 1957, 1958, 1960, 1961, 1969, 1971, 1972, 1973, 1974, 
                                      1975, 1977, 1978, 1979, 1984, 1986, 1989, 1990, 1991, 1993, 1996, 1997, 1998, 1999, 
                                      2000, 2003, 2006, 2010, 2011, 2013, 2014, 2015, 2016, 2017), check.rows = TRUE)

years_wf <- rep(row.names.data.frame(df_wf))
Estimated_theta <- rep(wf[["se.theta"]])

p <- ggplot(df, aes(x=years_wf, y= Estimated_theta))
p +geom_bar(stat = "identity")

## estimated theta?
df_wf2 <- data.frame(wf[["theta"]], row.names = c(1950, 1951, 1952, 1957, 1958, 1960, 1961, 1969, 1971, 1972, 1973, 1974, 
                                                    1975, 1977, 1978, 1979, 1984, 1986, 1989, 1990, 1991, 1993, 1996, 1997, 1998, 1999, 
                                                    2000, 2003, 2006, 2010, 2011, 2013, 2014, 2015, 2016, 2017), check.rows = TRUE)

years_wf <- rep(row.names.data.frame(df_wf))
estimated_theta <- rep(wf[["theta"]])

p <- ggplot(df_wf2, aes(x=years_wf, y= estimated_theta))
p +geom_bar(stat = "identity")

################ Topic Modelling ##################
library(quanteda)
library(readtext)
library(ggplot2)
library(tm)

## Change the working directory, read in the texts, create a corpus, and look at it
myspeech_tm <- readtext("*.txt", docvarsfrom="filenames")
myCorpus_tm <- corpus(myspeech_tm)
summary(myCorpus_tm)

## Create dfm's, removing numbers, stemming, removing standard English stopwords and two Senate-specific phrases
dfmat_speech <- dfm(myCorpus_tm, remove_numbers=TRUE, stem=TRUE, remove_punct = TRUE, 
                    remove = c(stopwords("english"), "america", "american", "can", "will", "must", "us", "now"))

dfmat_speech <- dfmat_speech[ntoken(dfmat_speech) > 0,]

## quanteda does not implement topic models, but you can easily access LDA() 
## from the topicmodel package through convert(). 
## k = 10 specifies the number of topics to be discovered. 
## This is an important parameter and you should try a variety of values.
dtm <- convert(dfmat_speech, to = "topicmodels")
lda <- LDA(dtm, k = 10)
# lda2 <- LDA(dtm, k = 2)

# You can extract the most important terms for each topic from the model using terms().
terms(lda, 10)
# terms(lda2, 10)

# You can then obtain the most likely topics using topics() and save them as a document-level variable.
docvars(dfmat_speech, 'topic') <- topics(lda)
head(topics(lda), 20)


#### determining k number of topics

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

k <- 10
burnin <- 500
iter <- 500
keep <- 50
fitted <- topicmodels::LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )
## assuming that burnin is a multiple of keep
logLiks <- fitted@logLiks[-c(1:(burnin/keep))]

library(Rmpfr)

## This returns the harmomnic mean for k = 10 topics.
harmonicMean(logLiks)

## To find the best value for k for our corpus, we do this over a sequence of topic models with different vales for k. 
# This will generate numerous topic models with different numbers of topics, creating a vector to hold the k values. 
# We will use a sequence of numbers from 2 to 50, stepped by one
seqk <- seq(2, 50, 1)
burnin <- 500
iter <- 500
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(dtm, k = k,
                                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                                                     iter = iter, keep = keep) )))
# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
library(Rmpfr)
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# Now that is have calculated the harmonic means of the models for 2 - 100 topics, 
# you can plot the results using ggplot2 package.

which.max(hm_many)

ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  #  annotate("The optimal number of topics is", seqk[which.max(hm_many)]) +
  #  annotate("text", x = 10, y = -120000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of NEN LLIS", atop(italic("How many distinct topics in the speeches?")))))
ldaplot


which.max(hm_many)


# visualisation using 10 topics
# word-topic probabilities 
lda <- LDA(dtm, k = 10)
library(tidytext)

ap_topics <- tidy(lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# This visualization lets us understand the two topics that were extracted from the articles.
# The most common words in topic 1 include “percent”, “million”, “billion”, and “company”, 
# which suggests it may represent business or financial news. 
# Those most common in topic 2 include “president”, “government”, and “soviet”, suggesting that 
# this topic represents political news. One important observation about the words in each topic is that some words, 
# such as “new” and “people”, are common within both topics. 
# This is an advantage of topic modeling as opposed to “hard clustering” methods: 
# topics used in natural language could have some overlap in terms of words.


############# fluctuations of topics based on time periods ################
head(kwic(myCorpus, pattern = c("peace", "world", "communist", "forc*"), window = 3, valuetype = "glob"))
?kwic



