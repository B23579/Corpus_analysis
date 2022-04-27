
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# load packages
library(knitr) 
library(kableExtra) 
library(DT)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(tidyverse)
library(flextable)
library(qdapRegex) 
# activate klippy for copy-to-clipboard button
klippy::klippy()
library("spacyr")
spacy_initialize(model = "it_core_news_sm")
setwd("D:/University of Trieste/project/Corpus_analysis")
#install.packages("stopwords")
#head(stopwords::stopwords("Italian"), 20) 



setwd("D:/University of Trieste/project/Corpus_analysis")
#install.packages("stopwords")
#head(stopwords::stopwords("Italian"), 20) 

file_name <- "data/carteggio.svevo3.csv"
corpus <- read.csv(file_name, sep=';', encoding = "UTF-8")
corpus %>%
  str
#view(corpus)

corpus$mainLanguage<-as.factor(corpus$mainLanguage)
corpus$sender<-as.factor(corpus$sender)
corpus$year<-as.factor(corpus$year)

# let's check the number of language used 
levels(corpus$mainLanguage)
# Let's check the number of senders
levels(corpus$sender)
#number of year
levels(corpus$year)

# Let's count the number of letter's send in each lenguage

sum(corpus$mainLanguage == 'ENG')

sum(corpus$mainLanguage == "FRE")

sum(corpus$mainLanguage == "GER")
sum(corpus$mainLanguage == "ITA")

# There more text writing in Itlian implied have more inside than order text. So we will study text writing
# in italian and then check those writing in french.  

# Let's filter the dataset
corpus_it<- dplyr::filter(corpus, mainLanguage %in%c("ITA"))

#view(corpus_it)

# Let's select the the main feature that will be used
names(corpus_it)

fin_corpus<- select(corpus_it, year, text, sender)
#fin_corpus<-filter(fin_corpus, !(year %in% c("1921","1924")))
fin_corpus <-droplevels(fin_corpus)

# new way 
# to useful files for italians stopwords and proper nouns in italian language
load("itastopwords.rda")
load("vocabolarioNomiPropri.rda")
corp<- fin_corpus %>%select(year,text)%>%
  group_by(year) %>%
  paste0()


ll<-Corpus(VectorSource(fin_corpus$text))
print(ll)
inspect(ll[1:2])

#clining 

corpus_t<-tm_map(ll,tolower)

# remouve number
corpus_t<-tm_map(corpus_t,removeNumbers)
#remove punctuation
corpus_t<-tm_map(corpus_t,removePunctuation,preserve_intra_word_dashes = TRUE)
#remouve all pucntion which is not remouved by remouve puctuation
corpus_t <- tm_map(corpus_t, removeWords,c("d'","l'","un'", "—'", "«»", "«","»" ))
# delete white spaces which originate from the removed strings
corpus_t <- tm_map(corpus_t , stripWhitespace)
# remove words which are useless in the bigrams
corpus_t <- tm_map(corpus_t,removeWords,c("devono","moglie","_","-","»","trieste", "essere", "u","qui", "fffd", "ancora", "volta", "tre", "due", "anni", "dopo", "aver","ultimi", "vuol","dire", "dovrebbe","qualche","giorno", "p", "vista", "punto", "n","mesi", "pochi", "migliaia", "milioni","piazza", "troppo", "tempo","streaming","stato","fatto","fare", "fra","poco","detto"))
# remove stopwords from "itastopwords.rda" file
corpus_t <- tm_map(corpus_t, removeWords, itastopwords)
# remove default R stopwords for italian language
corpus_t <- tm_map(corpus_t, removeWords, stopwords("italian"))
# remove proper nouns
corpus_t <- tm_map(corpus_t, removeWords, row.names(vocabolarioNomiPropri))
inspect(corpus_t[1])



library(RWeka)
install.packages("ngram")

library(ngram)





# Functions
BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=2, max=2))}
ThreegramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=3, max=3))}
FourgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=4, max=4))}

# Bigrams
options(mc.cores=1)
dtm.docs.2g <- DocumentTermMatrix(corpus_t, control=list(tokenize=BigramTokenizer))

#Threegrams
options(mc.cores=1)
dtm.docs.3g <- DocumentTermMatrix(corpus_t, control=list(tokenize=ThreegramTokenizer))

#Fourgrams
options(mc.cores=1)
dtm.docs.4g <- DocumentTermMatrix(corpus_t, control=list(tokenize=FourgramTokenizer))
# freqTerms.4g.docs <- findFreqTerms(dtm.docs.4g,20,Inf)




#Topic Modelling step


# create models with different number of topics
result <- ldatuning::FindTopicsNumber(
  dtm.docs.2g,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# number of topics
K <- 8
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(dtm.docs.2g, K, method="Gibbs", control=list(iter = 2000, verbose = 25))


# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)
tmResult$topics
terms(topicModel, 5)

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)
tmResult$topics
terms(topicModel, 50)

topic = 1
words = posterior(topicModel)$terms[topic, ]
topwords = head(sort(words, decreasing = T), n=50)
head(topwords)

library(wordcloud)
wordcloud(names(topwords), topwords)

#We can also look at the topics per document, to find the top documents per topic:

#Topic proportions over time

tmResult <- posterior(topicModel)
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel, 2), 2, paste, collapse = " ")  # reset topicnames

topicNames

k<-5 # number of topic

# append decade information for aggregation
#fin_corpus$year <- paste0(substr(fin_corpus$year, 0, 3), "0")
# get mean topic proportions per decade
topic_proportion_per_decade <- aggregate(theta, by = list(decade = fin_corpus$year),FUN=mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_decade)[2:(k+1)] <- topicNames
# reshape data frame
vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame, aes(x=decade, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

theta
view(vizDataFrame)

view(topic_proportion_per_decade)

heatmap(as.matrix(topic_proportion_per_decade[-1]))

fin_corpus$year
as.Date(as.numeric(fin_corpus$year))

as.integer()
fin_corpus$year

# append decade information for aggregation
#fin_corpus$year <- paste0(substr(fin_corpus$year, 0, 3), "0")
# get mean topic proportions per decade
topic_proportion_per_sender <- aggregate(theta, by = list(decade = fin_corpus$sender),FUN=mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_sender)[2:(k+1)] <- topicNames
# reshape data frame
vizDataFrame <- melt(topic_proportion_per_sender, id.vars = "decade")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame, aes(x=decade, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# re-rank top topic terms for topic names
topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
# What are the most probable topics in the entire collection?
topicProportions <- colSums(theta) / nDocs(dtm1)  # mean probablities over all paragraphs
names(topicProportions) <- topicNames     # assign the topic names we created before
sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order

soP <- sort(topicProportions, decreasing = TRUE)
paste(round(soP, 5), ":", names(soP))


countsOfPrimaryTopics <- rep(0, K)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:nDocs(dtm1)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)

so <- sort(countsOfPrimaryTopics, decreasing = TRUE)
paste(so, ":", names(so))


