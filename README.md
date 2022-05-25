# Corpus_analysis
Analysis of Svevo’s letters corpus, machine learning assignment 

# Problem Statement 
Italo Svevo, a pioneer of the psychological novel in Italy and one of the greatest Italian novelists, wrote and received letters in multiple languages during the twentieth century. The letters were recorded and stored in a database. Thus, the purpose of this project is to analyze those epistolary corpora to gain insight into topics and sentiments expressed (positive or negative) in his letters. This approach intends to extract information from the corpora by looking at the relationships between subjects, people, and emotions, as well as how those interactions change over time.

# Algorithms use to Approch the problem 
The number of topics discussed in letters will be determine by using a Correspondance Analysis (CA). A visualization technique for identifying and displaying the relationship between categories. For the topic modeling, Latent Dirichlet allocation (LDA) method  will be apply for fitting topic model. 

To find the emotion and sentiment express in each letter, the [FEEL-IT](https://github.com/MilaNLProc/feel-it) (Emotion and Sentiment Classification for Italian Language) model building in python release in 2021 will be used.

# Data Description

The Svevo letter corpus dataset contains a total of 894 letters written by Italo Svevo including 826 letters written in Italian, 28 letters written in German, 30 in French, and 10 in English all written between 1885 and 1928. Data includes information about: the name of the corpus section, the index of the letter in the section, the date of the letter, the year of the letter, the sender of the letter, the sender's location, the recipient's location, languages used in the letter, the main language, and the text of the letter. There are 12 variables in total. Letters were mostly written in Italian with 816 sending by Ettore Schmitz, 30 by Eugenio Montale, 15 by Marieanne Crémieux Comnène, 11 by James Joyce, 8 by Benjamin Crémieux, 8 by Valerio Jahier, 5 by Valéry Larbaud, and 1  by Benjamin Larbaud to Svevo and his wife. We noticed an unbalanced distribution of data.  

## Result 
report will be publish in juin

![fg]([./PCA.png](https://github.com/B23579/Corpus_analysis/blob/main/emotion_proportion_by_topic.png)) 
