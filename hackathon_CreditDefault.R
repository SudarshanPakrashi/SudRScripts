credit <- read.csv("C:\\Users\\sudarshan.pakrashi\\OneDrive - Accenture\\Accenture\\Hackathon\\default-of-credit-card-clients-dataset\\UCI_Credit_Card.csv")

#Convert into factors
#Gender
credit[3] <- as.data.frame(lapply(credit[3],function(x){ifelse(x == 1,"Male","Female")}))
#Education
credit[4] <- as.data.frame(lapply(credit[4],function(x){ifelse(x == 1,"Graduate",ifelse(x == 2,"University",ifelse(x == 3,"HighSchool",ifelse(x == 4,"Others",ifelse(x == 5,"Education5",ifelse(x == 6,"Education6",ifelse(x == 7,"Education7",ifelse(x == 0,"Education0",x))))))))}))
#Marriage
credit[5] <- as.data.frame(lapply(credit[5],function(x){ifelse(x == 1,"Married",ifelse(x == 2,"Single",ifelse(x == 3,"Others",ifelse(x == 0,"Unknown",x))))}))

#Convert default into categorical data
credit[25] <- as.data.frame(lapply(credit[25],function(x){ifelse(x == 1,"Yes","No")}))

credit[26] <- as.data.frame(lapply(credit[7],function(x){ifelse(x <= 0,"Good",ifelse(x <= 1,"Okay","Bad"))}))
colnames(credit)[26] <- "PAY0"
credit[27] <- as.data.frame(lapply(credit[8],function(x){ifelse(x <= 0,"Good",ifelse(x <= 1,"Okay","Bad"))}))
colnames(credit)[27] <- "PAY2"
credit[28] <- as.data.frame(lapply(credit[9],function(x){ifelse(x <= 0,"Good",ifelse(x <= 1,"Okay","Bad"))}))
colnames(credit)[28] <- "PAY3"
credit[29] <- as.data.frame(lapply(credit[10],function(x){ifelse(x <= 0,"Good",ifelse(x <= 1,"Okay","Bad"))}))
colnames(credit)[29] <- "PAY4"
credit[30] <- as.data.frame(lapply(credit[11],function(x){ifelse(x <= 0,"Good",ifelse(x <= 1,"Okay","Bad"))}))
colnames(credit)[30] <- "PAY5"
credit[31] <- as.data.frame(lapply(credit[12],function(x){ifelse(x <= 0,"Good",ifelse(x <= 1,"Okay","Bad"))}))
colnames(credit)[31] <- "PAY6"

credit$Outstanding <- (credit$BILL_AMT1 + credit$BILL_AMT2 + credit$BILL_AMT3 + credit$BILL_AMT4 + credit$BILL_AMT5 + credit$BILL_AMT6 - credit$PAY_AMT1 - credit$PAY_AMT2 - credit$PAY_AMT3 - credit$PAY_AMT4 - credit$PAY_AMT5 - credit$PAY_AMT6)
credit[33] <- as.data.frame(lapply(credit[32],function(x){ifelse(x <= 100000,"Low",ifelse(x <= 500000,"Medium","High"))}))
colnames(credit)[33] <- "OutstandingGrade"
credit[34] <- as.data.frame(lapply(credit[2],function(x){ifelse(x <= 100000,"Low",ifelse(x <= 500000,"Medium","High"))}))
colnames(credit)[34] <- "LimitGrade"

credit$age_grouping[credit$AGE <=18] <- "Below 18"
credit$age_grouping[credit$AGE <=28 & credit$AGE >= 19] <- "19 - 28"
credit$age_grouping[credit$AGE <=38 & credit$AGE >= 29] <- "29 - 38"
credit$age_grouping[credit$AGE <=55 & credit$AGE >= 39] <- "39 - 55"
credit$age_grouping[credit$AGE <=65 & credit$AGE >= 56] <- "56 - 65"
credit$age_grouping[credit$AGE >= 66] <- "Above 65"

#Take subset of columns required for Decision Tree
credit_sub <- credit[,c("SEX","EDUCATION","MARRIAGE","age_grouping","default.payment.next.month","PAY0","PAY2","PAY3","PAY4","PAY5","PAY6","OutstandingGrade","LimitGrade")]


#Randomly split the test and train data
set.seed(123456)
train_sample <- sample(30000,25000)

credit_train <- credit_sub[train_sample,]
credit_test <- credit_sub[-train_sample,]
#credit_train_jrip <- credit_sub[train_sample,]
#credit_test_jrip <- credit_sub[-train_sample,]

credit_train_lbls <- credit_train$default.payment.next.month
credit_train <- credit_train[-5]
credit_test_lbls <- credit_test$default.payment.next.month
credit_test <- credit_test[-5]

#Cost Matrix
matrix_dimensions <- list(c("No", "Yes"), c("No", "Yes"))
names(matrix_dimensions) <- c("predicted", "actual")
error_cost <- matrix(c(0, 3, 10, 0), nrow = 2,dimnames = matrix_dimensions)

#Decision Tree Model
library(C50)
credit_model <- C5.0(credit_train,credit_train_lbls,costs = error_cost,rules = TRUE)

#Model Performance
credit_prediction <- predict(credit_model,credit_test)

library(gmodels)
CrossTable(credit_test_lbls,credit_prediction,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn = c("Actual","Predicted"))

library(ggplot2)
library(scales)
library(caret)

ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
                   "Kappa", percent_format()(m$overall[2]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Actual, y = Predicted)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue",guide = TRUE) +
    geom_text(aes(x = Actual, y = Predicted, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

cm1 <- confusionMatrix(credit_test_lbls,credit_prediction,dnn = c("Actual","Predicted"))

#Ripper Model
#library(RWeka)
#credit_jrip <- JRip(default.payment.next.month ~ .,data = credit_train_jrip)
#credit_predict_jrip <- predict(credit_jrip,credit_test_jrip)
#CrossTable(credit_test_jrip$default.payment.next.month,credit_predict_jrip,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn = c("Actual","Predicted"))


#Analyse Using Naive Bayes
library(e1071)
credit_classifier <- naiveBayes(credit_train,credit_train_lbls)

#Predict
credit_predict <- predict(credit_classifier,credit_test)
#Confusion Matrix
library(gmodels)
bayesConfusion <- CrossTable(credit_test_lbls,credit_predict,prop.chisq = FALSE,prop.t = FALSE,dnn = c("Actual","Predicted"))

library(caret)
cm <- confusionMatrix(credit_test_lbls,credit_predict,dnn = c("Actual","Predicted"))
fourfoldplot(bayesConfusion$prop.tbl)

ggplotConfusionMatrix(cm1)
ggplotConfusionMatrix(cm)
