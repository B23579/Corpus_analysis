library(tidyverse)
library(tidytext)
library(purrr)
library(reshape2)
library(qdapRegex) # to remove number in the text
library(textstem) # this will be use for lemmatize string 
library(stopwords)
library("spacyr")
library(stylo)
library(data.table)
spacy_initialize(model = "it_core_news_sm")
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


gt<-levels(fin_corpus$year)

gt
compt <-0 

for (annee in gt){
  
  
  yer<-filter(fin_corpus,year %in% c(annee))                    
  l <-paste0(yer$text)
  
  # remove punctuations, lower case, remove number,  Lemmatize_string an tokinization
  #and Lemmatize
  
  parsedtxt <- gsub('[[:punct:] ]+',' ',l) %>%
    tolower() %>% rm_number()%>%
    spacy_parse()
  
  # remove stoping word and contruct the lexical profile
  o<-parsedtxt$lemma %>%
    delete.stop.words( stop.words = stopwords::stopwords("Italian"))%>%
    melt()%>%
    count(value, sort = TRUE)
  names(o) <- c("word", annee)
  
  # Lexical profile 
  
  if(compt==0){
   Corpus <- o 
   compt<-1
  }
  else{
    Corpus <-full_join(Corpus,o, by = "word",copy = FALSE)  
  }
}
view(Corpus)

# replacing NA values in data frame
Corpus[is.na(Corpus)] = 0
view(Corpus)


# Let's save the text profile 
fwrite(Corpus, "lexical_profile_from_corpus/lexical_profil.csv")

corpus<-read.csv("lexical_profile_from_corpus/lexical_profil.csv", sep=",",encoding = "UTF-8")

view(corpus)
# Let's convert our dataset as table
#install.packages("data.table")           # Install and load data.table
library("data.table")

#corresponding analysis 
Corpus$word<-as.factor(Corpus$word)
setDT(Corpus) 
out <- as.data.table(Corpus)
view(out)
library(ca)
out<-ca(out)

