<<<<<<< HEAD
# install packages
#install.packages("tm")
#install.packages("topicmodels")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("wordcloud")

#install.packages("pals")
#install.packages("SnowballC")
#install.packages("lda")
#install.packages("ldatuning")
# install klippy for copy-to-clipboard button in code chunks
#remotes::install_github("rlesur/klippy")

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
library(RcmdrPlugin.temis)
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

l <-as.character(paste0(fin_corpus$text))
parsedtxt <- gsub("[^[:alnum:]]", " ", l) %>%
  tolower() %>% rm_number()
fin_corpus$text<-parsedtxt

library(data.table)
fwrite(fin_corpus, "sentimental_analysis/half_clean_corpus.csv")

# From here, I can import the data save from sentimental analysis 
# for the simplicity after the topic modeling. But we can use decide to contineous 
# with the the previous data. 

file_n <- "data/emotion.csv"
fin_corpus <- read.csv(file_n, sep=',', encoding = "UTF-8")
#view(fin_corpus)

ll<-Corpus(VectorSource(fin_corpus$text))
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

dtm1=DocumentTermMatrix(corpus_t)
inspect(dtm1)

#Topic Modelling step

# using the correspondance analysis, we identify 5 topic of 
# discusion, so in the next step, we will use that number


# create models with different number of topics
#result <- ldatuning::FindTopicsNumber(
#  dtm1,
 # topics = seq(from = 2, to = 20, by = 1),
#  metrics = c("CaoJuan2009",  "Deveaud2014"),
 # method = "Gibbs",
#  control = list(seed = 77),
#  verbose = TRUE
#)

#FindTopicsNumber_plot(result)

# number of topics
K <- 5
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(dtm1, K, method="Gibbs", control=list(iter = 2000, verbose = 25))


# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)
tmResult$topics
terms(topicModel, 30)

# have a look a some of the results (posterior distributions)
#tmResult <- posterior(topicModel)
# format of the resulting object
#attributes(tmResult)
#tmResult$topics
#terms(topicModel, 50)

#### Word-topic probabilities
library(tidytext)

ap_topics <- tidy(topicModel, matrix = "beta")


ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

p<-ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()+
  labs(title = "Top 10 terms in each LDA topic",
       x = expression(beta), y = NULL) 
p

ggsave(p, filename = "Plot/top_10_word.png")

#####

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

topicNames <- c("Travel","Daily life","Work","Opinion","Severo Book")


k<-5 # number of topic

# append decade information for aggregation
#fin_corpus$year <- paste0(substr(fin_corpus$year, 0, 3), "0")
# get mean topic proportions per decade
topic_proportion_per_year <- aggregate(theta,by = list(year = as.factor(fin_corpus$year)),FUN=mean)
# set topic names to aggregated columns

colnames(topic_proportion_per_year)[2:(k+1)] <- topicNames
# reshape data frame
vizDataFrame <- melt(topic_proportion_per_year, id.vars = "year")
# plot topic proportions evolution per year as bar plot
ppp<-ggplot(vizDataFrame, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(10), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ppp

ggsave(ppp, filename = "Plot/topic_proportion_evolution_per_yer.png")

#theta
#view(vizDataFrame)


# append decade information for aggregation
#fin_corpus$year <- paste0(substr(fin_corpus$year, 0, 3), "0")
# get mean topic proportions per decade
topic_proportion_per_sender <- aggregate(theta, by = list(year = as.factor(fin_corpus$sender)),FUN=mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_sender)[2:(k+1)] <- topicNames
# reshape data frame
vizDataFrame <- melt(topic_proportion_per_sender, id.vars = "year")
# plot topic proportions per decade as bar plot
pp<-ggplot(vizDataFrame, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

pp

ggsave(pp, filename = "Plot/topic_proportion_by_writer.png")

## evolution of topic sentiment in year.  

# get mean topic proportions per sentiment
topic_proportion_per_sentiment <- aggregate(theta, by = list(sentiment = as.factor(fin_corpus$sentiment)),FUN=mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_sentiment)[2:(k+1)] <- topicNames
# reshape data frame
vizDataFrame_sentiment <- melt(topic_proportion_per_sentiment, id.vars = "sentiment")
# plot topic proportions per decade as bar plot
pp<-ggplot(vizDataFrame_sentiment, aes(x=sentiment, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
pp
ggsave(pp, filename = "Plot/sentiment_proportion_by_topic.png")

## evolution of topic sentiment in year.  

# get mean topic proportions per emotion
topic_proportion_per_emotion <- aggregate(theta, by = list(emotion = as.factor(fin_corpus$emotion)),FUN=mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_emotion)[2:(k+1)] <- topicNames
# reshape data frame
vizDataFrame_emotion <- melt(topic_proportion_per_emotion, id.vars = "emotion")
# plot topic proportions per decade as bar plot
pp<-ggplot(vizDataFrame_emotion, aes(x=emotion, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
pp
ggsave(pp, filename = "Plot/emotion_proportion_by_topic.png")

### Let's plot the evolution of emotion and sentiment in year

lp <- aggregate(fin_corpus,by=list(year=fin_corpus$year,emotion=as.factor(fin_corpus$emotion)),FUN=mean)
# Find Duplicate Column Names
duplicated_names <- duplicated(colnames(lp))

# Remove Duplicate Column Names
lp<-lp[!duplicated_names]
lp$year<-as.factor(lp$year)
lp$emotion<-as.factor(lp$emotion)
lll<-ggplot(lp, aes(x=year, y=X, fill=emotion)) + 
  geom_bar(stat = "identity") + ylab("count") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Emotion expression over time")
lll
ggsave(lll, filename = "Plot/Emotion_expression_over_time.png")


### Let's plot the evolution of emotion and sentiment in year

lp <- aggregate(fin_corpus,by=list(year=fin_corpus$year,emotion=as.factor(fin_corpus$emotion)),FUN=mean)
# Find Duplicate Column Names
duplicated_names <- duplicated(colnames(lp))

# Remove Duplicate Column Names
lp<-lp[!duplicated_names]
lp$year<-as.factor(lp$year)
lp$emotion<-as.factor(lp$emotion)
lll<-ggplot(lp, aes(x=year, y=X, fill=emotion)) + 
  geom_bar(stat = "identity") + ylab("count") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Emotions") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Emotion expression over time")
lll
ggsave(lll, filename = "Plot/Emotion_expression_over_time.png")


### Let's plot the evolution of sentiment in year

lp <- aggregate(fin_corpus,by=list(year=fin_corpus$year,sentiment=as.factor(fin_corpus$sentiment)),FUN=mean)
# Find Duplicate Column Names
duplicated_names <- duplicated(colnames(lp))

# Remove Duplicate Column Names
lp<-lp[!duplicated_names]
lp$year<-as.factor(lp$year)
lp$sentiment<-as.factor(lp$sentiment)
lll<-ggplot(lp, aes(x=year, y=X, fill=sentiment)) + 
  geom_bar(stat = "identity") + ylab("count") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "sentiment") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Emotion expression over time")
lll
ggsave(lll, filename = "Plot/sentiment_expression_over_time.png")

## Let's check to whom the sentiment is more related 


p<-ggplot(data = fin_corpus) + 
  geom_bar(mapping = aes(x = sender, fill = sentiment), position = "dodge")+
  labs(title = "Sentiment associate to each sender")
p
ggsave(p, filename = "Plot/sentiment_associate.png")

p<-ggplot(data = fin_corpus) + 
  geom_bar(mapping = aes(x = sender, fill = emotion), position = "dodge")+
  labs(title = "Emotion associate to each sender")
p
ggsave(p, filename = "Plot/emotion_associate.png")


=======
# install packages
#install.packages("tm")
#install.packages("topicmodels")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("wordcloud")

#install.packages("pals")
#install.packages("SnowballC")
#install.packages("lda")
#install.packages("ldatuning")
# install klippy for copy-to-clipboard button in code chunks
#remotes::install_github("rlesur/klippy")

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

l <-as.character(paste0(fin_corpus$text))
parsedtxt <- gsub("[^[:alnum:]]", " ", l) %>%
  tolower() %>% rm_number()
fin_corpus$text<-parsedtxt

library(data.table)
fwrite(fin_corpus, "sentimental_analysis/half_clean_corpus.csv")

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

dtm1=DocumentTermMatrix(corpus_t)
inspect(dtm1)


#Topic Modelling step


# create models with different number of topics
result <- ldatuning::FindTopicsNumber(
  dtm1,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# number of topics
K <- 5
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(dtm1, K, method="Gibbs", control=list(iter = 2000, verbose = 25))


# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)
tmResult$topics
terms(topicModel, 30)

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
topicNames <- apply(, 2, paste, collapse = " ")  # reset topicnames

topicNames <- c("Work","Travel","Severo Book","Opinion","Daily life")
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

view(vizDataFrame)
>>>>>>> 977fbd805679428e0e77a197bcf6fc429b2427fe
