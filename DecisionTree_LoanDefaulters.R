credit <- read.csv("http://invidio.drl.pl/files/german_credit.csv")

#Convert default into categorical data
credit[1] <- as.data.frame(lapply(credit[1],function(x){ifelse(x == 1,"Yes","No")}))

#Randomly split the test and train data
set.seed(123)
train_sample <- sample(1000,900)

credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

#Train the model
library(C50)
credit_model <- C5.0(credit_train[-1],credit_train$default)

#Model Performance
credit_prediction <- predict(credit_model,credit_test)

library(gmodels)
CrossTable(credit_test$default,credit_prediction,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn = c("Actual","Predicted"))

#Boost 10
credit_model10 <- C5.0(credit_train[-1],credit_train$default,trials = 10)
credit_prediction_boosted10 <- predict(credit_model10,credit_test)
CrossTable(credit_test$default,credit_prediction_boosted10,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn = c("Actual","Predicted"))

#Cost Matrix
matrix_dimensions <- list(c("No", "Yes"), c("No", "Yes"))
names(matrix_dimensions) <- c("predicted", "actual")
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,dimnames = matrix_dimensions)

credit_cost <- C5.0(subset(credit_train,select = -c(1)),credit_train$default,costs = error_cost,rules = TRUE)
credit_prediction_cost <- predict(credit_cost,credit_test)
CrossTable(credit_test$default,credit_prediction_cost,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn = c("Actual","Predicted"))

#JRip Model
library(RWeka)
credit_jrip <- JRip(default ~ .,data = credit_train)
credit_predict_jrip <- predict(credit_jrip,credit_test)
CrossTable(credit_test$default,credit_predict_jrip,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn = c("Actual","Predicted"))
