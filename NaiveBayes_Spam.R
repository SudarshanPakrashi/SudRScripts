library(tidyverse)
sms_raw <- read.csv("https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/sms_spam.csv",stringsAsFactors = FALSE)
sms_raw$type = factor(sms_raw$type)
library(tm)

sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)

#Lower Case
sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower))
#Remove Numbers
sms_corpus_clean <- tm_map(sms_corpus_clean,removeNumbers)
#Remove Stop Words
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords,stopwords())
#Remove Punctuations
replacePunctutation <- function(x){gsub("[[:punct:]]+"," ",x)}
sms_corpus_clean <- tm_map(sms_corpus_clean,content_transformer(replacePunctutation))
#Stemming
library(SnowballC)
sms_corpus_clean <- tm_map(sms_corpus_clean,stemDocument)
#Strip Whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean,stripWhitespace)
#Tokenize
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
#Split Train and Test sets
sms_dtm_train <- sms_dtm[1:4573,]
sms_dtm_test <- sms_dtm[4574:5574,]
sms_dtm_train_lbls <- sms_raw[1:4573,]$type
sms_dtm_test_lbls <- sms_raw[4574:5574,]$type
#Find Frequent Words
sms_freq_words <- findFreqTerms(sms_dtm_train,5)

sms_freq_train <- sms_dtm_train[,sms_freq_words]
sms_freq_test <- sms_dtm_test[,sms_freq_words]

#Convert to category matrix
convert_counts <- function(x){x <- ifelse(x > 0,"Yes","No")}

sms_train <- apply(sms_freq_train,MARGIN = 2,convert_counts)
sms_test <- apply(sms_freq_test,MARGIN = 2,convert_counts)
#Naive Bayes Classifier
library(e1071)
sms_classifier <- naiveBayes(sms_train,sms_dtm_train_lbls)
#Predict
sms_predict <- predict(sms_classifier,sms_test)
#Confusion Matrix
library(gmodels)
CrossTable(sms_predict,sms_dtm_test_lbls,prop.chisq = FALSE,prop.t = FALSE,dnn = c("Predicted","Actual"))
