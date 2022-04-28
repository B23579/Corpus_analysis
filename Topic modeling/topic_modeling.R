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

file_name <- "data/carteggio.svevo3.csv"
corpus <- read.csv(file_name, sep=';', encoding = "UTF-8")
corpus %>%
  str
#view(corpus)

corpus$mainLanguage<-as.factor(corpus$mainLanguage)
corpus$sender<-as.factor(corpus$sender)
corpus$year<-as.factor(corpus$year)



# There more text writing in Itlian implied have more inside than order text. So we will study text writing
# in italian and then check those writing in french.  

# Let's filter the dataset
corpus_it<- dplyr::filter(corpus, mainLanguage %in%c("ITA"))

fin_corpus<- select(corpus_it, year, text, sender)
#fin_corpus<-filter(fin_corpus, !(year %in% c("1921","1924")))
fin_corpus <-droplevels(fin_corpus)
gt<-levels(fin_corpus$year)

l <-as.character(paste0(fin_corpus$text))
parsedtxt <- gsub("[^[:alnum:]]", " ", l) %>%
  tolower() %>% rm_number()

length(parsedtxt)
fin_corpus$text<-parsedtxt



# Let's transforme the text to tible 
library(stm)
library(quanteda)
#install.packages("tidytext")
library(tidytext)
text_df <- tibble(line = fin_corpus$year, text = parsedtxt)
#view(text_df)
processedCorpus <- tm_map(text_df , content_transformer(tolower))

tweets_byusers_corpus <- iconv(text_df$text,"UTF-8")
view(tweets_byusers_corpus)
corpus <- Corpus(VectorSource(tweets_byusers_corpus))
corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, removePunctuation) 
corpus <- tm_map(corpus,removeWords,stopwords::stopwords("Italian")) 
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus,stripWhitespace)
dtm <- DocumentTermMatrix(corpus)

rowTotals<-apply(dtm,1,sum) #running this line takes time
empty.rows<-dtm[rowTotals==0,]$dimnames[1][[1]] 
corpus<-corpus[-as.numeric(empty.rows)]
dtm <- DocumentTermMatrix(corpus)  

#the line above will convert corpus to DTM. But we need to run the next four lines
#to remove empty document from DTM to prevent potential errors.
#A DTM is a matrix, with documents in the rows and terms in the columns.
inspect(dtm) 

dtm.mx <- as.matrix(dtm)
frequency <- colSums(dtm.mx)
frequency <- sort(frequency, decreasing=TRUE)
frequency[1:25]

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

k <- 5 #find 5 topics
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("topic_model",k,"TopicsToTerms.csv"))
ldaOut.terms





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
gt<-levels(fin_corpus$year)

gt
compt <-0 

for (annee in gt){
  
  
  yer<-filter(fin_corpus,year %in% c(annee))                    
  l <-paste0(yer$text)
  
  # remove punctuations, lower case, remove number,  Lemmatize_string an tokinization
  #and Lemmatize
  
  parsedtxt <- gsub("[^[:alnum:]]", " ", l) %>%
    tolower() %>% rm_number()%>%
    spacy_parse()
  
  # remove stoping word and contruct the lexical profile
  o<-parsedtxt$lemma %>%
    delete.stop.words( stop.words = stopwords::stopwords("Italian"))%>%paste0()
  
  # remove words which are useless in the bigrams
  
  
  f <-rep(annee,length(o))
  
  # Lexical profile 
  
  if(compt==0){
    Corpus <- data.frame(word=o,year=f)
    compt<-1
  }
  else{
    Corpus <-rbind(Corpus,data.frame(word=o,year=f))  
  }
}
Corpus$word<-as.factor(Corpus$word)

Corpus$year<-as.factor(Corpus$year)
view(Corpus)

tab<-table(Corpus)%>%
  print 
m<-tidy(tab)
m %>%
  cast_dtm(year, word, n)


ldaOu1t <-LDA(m,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("topic_model",k,"TopicsToTerms.csv"))
ldaOut.terms[1:6,]

ap_lda <- LDA(m, k = 5, control = list(seed = 1234))
ap_lda


names(fin_corpus)

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
corpus_t <- tm_map(corpus_t, removeWords,c("d'","l'","un'", "—'" ))
# delete white spaces which originate from the removed strings
corpus_t <- tm_map(corpus_t , stripWhitespace)
# remove words which are useless in the bigrams
corpus_t <- tm_map(corpus_t,removeWords,c("devono","—","-","trieste", "essere", "u","qui", "fffd", "ancora", "volta", "tre", "due", "anni", "dopo", "aver","ultimi", "vuol","dire", "dovrebbe","qualche","giorno", "p", "vista", "punto", "n","mesi", "pochi", "migliaia", "milioni","piazza", "troppo", "tempo","streaming","stato","fatto","fare", "fra","poco","detto"))
# remove stopwords from "itastopwords.rda" file
corpus_t <- tm_map(corpus_t, removeWords, itastopwords)
# remove default R stopwords for italian language
corpus_t <- tm_map(corpus_t, removeWords, stopwords("italian"))
# remove proper nouns
corpus_t <- tm_map(corpus_t, removeWords, row.names(vocabolarioNomiPropri))

dtm1=DocumentTermMatrix(corpus_t)
inspect(dtm1)

#Modeling step
# remove sparse terms 

dtm2.sparse<-removeSparseTerms(dtm1, 1-(100/826))
inspect(dtm2.sparse)

# UPPER-BOUND: remove the most frequent words (i.e. those whose percentile is greater than 0.975)
mat_dtm<-as.matrix(dtm2.sparse)


dtm_lda1=LDA(dtm1,k=5,control = list(seed = 122234))

ldaOut.terms <- as.matrix(terms(dtm_lda1,6))
ldaOut.terms


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
terms(topicModel, 50)

topic = 1
words = posterior(topicModel)$terms[topic, ]
topwords = head(sort(words, decreasing = T), n=50)
head(topwords)

library(wordcloud)
wordcloud(names(topwords), topwords)

#We can also look at the topics per document, to find the top documents per topic:

topic.docs = posterior(topicModel)$topics[, topic] 
topic.docs = sort(topic.docs, decreasing=T)
head(topic.docs)

docs = docvars(dfm)[match(rownames(dtm), docnames(dfm)),]
tpp = aggregate(posterior(m)$topics, by=docs["President"], mean)
rownames(tpp) = tpp$President
heatmap(as.matrix(tpp[-1]))
