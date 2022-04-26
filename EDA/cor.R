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
    delete.stop.words( stop.words = stopwords::stopwords("Italian"))
  
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

# Let's consider word with a certain number of frequency
temp <- table(Corpus$word) %>%
  print

wrd <- temp[temp >= 200] %>%
  names
Corpus <- Corpus %>%
  filter(word %in% wrd)
str(Corpus)
#view(Corpus)

spacy_finalize()

#corresponding analysis 






library(ca)
library(FactoMineR)

ou<-table(Corpus)%>%
  print


out2 <- CA(unclass(ou))

plot(out2,what = c("all","none"))


pca1<-prcomp(t(ou), rank. = 2,center=TRUE, scale=FALSE)
summary(pca1)


# Frequency plot 

temp <- Corpus %>%group_by(year) %>% count(word, sort = TRUE)

df<-filter(temp, n>5)
view(df)

df<-select(df,word, year)
t <- table(df)%>% 
  print  
ou <- CA(unclass(t))
ca(t)
Corpus %>%group_by(year) %>% count(word, sort =TRUE)%>%
  slice_max(n, n = 10) %>% 
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word, fill = year)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~year, scales = "free_y") +
    labs(x = "Contribution to sentiment",
         y = NULL)
library(wordcloud)
wordcloud(df$word,max.words = 1000)

