library(tidyverse)
library(dplyr)
library(tidytext)
library(purrr)
library(reshape2)
library(tm)
library(qdapRegex) # to remove number in the text

library(stopwords)
library(tokenizers)
setwd("D:/University of Trieste/project/Corpus_analysis")
#install.packages("stopwords")
head(stopwords::stopwords("Italian"), 20)

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
view(corpus)
corpus_it<- dplyr::filter(corpus, mainLanguage %in%c("ITA"))

view(corpus_it)

# Let's select the the main feature that will be used
names(corpus_it)

fin_corpus<- select(corpus_it, year, text, sender)
view(fin_corpus)

yer<-filter(fin_corpus,year %in% c("1922"))                    
l <-paste0(yer$text)
  
# remove punctuations

l <- gsub('[[:punct:] ]+',' ',l) 
 

# Lower case, remove number, tokenization  and remove stoping word 

l<-tolower(l) %>% rm_number() %>%
  tokenize_words( stopwords = stopwords::stopwords("Italian"))%>%
  map(unlist)

 # corpus %>% str
l <- melt(l)
names(l) <- c("word", "year")
l<-l %>%
  count(word, sort = TRUE)
names(l) <- c("word", "year")
view(l)
str(l)
