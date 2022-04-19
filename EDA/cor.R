library(tidyverse)
library(dplyr)
library(tidytext)
library(purrr)
library(reshape2)
library(tm)
library(qdapRegex) # to remove number in the text

library(stopwords)
library(tokenizers)

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
  
  # remove punctuations
  
  l <- gsub('[[:punct:] ]+',' ',l) 
  
  
  # Lower case, remove number, tokenization  and remove stoping word 
  
  l<-tolower(l) %>% rm_number() %>%
    tokenize_words( stopwords = stopwords::stopwords("Italian"))%>%
    map(unlist)
  
  # Lemmetized 
  
  # corpus %>% str
  l <- melt(l)
  names(l) <- c("word", annee)
  if(compt==0){
   Corpus <- l %>%
     count(word, sort = TRUE)
  names(Corpus) <- c("word", annee)
   compt<-1
  }
  else{
    l<-l %>%
      count(word, sort = TRUE)
    
    names(l) <- c("word",annee )
    
    Corpus <-full_join(Corpus,l, by = "word",copy = FALSE)  
  }
}
view(Corpus)

# replacing NA values in data frame
Corpus[is.na(Corpus)] = 0
view(Corpus)

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
str(out)
levels(out$word)
