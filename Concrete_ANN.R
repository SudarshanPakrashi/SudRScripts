concrete <- read.csv("C:\\Sudarshan\\RScripts\\concrete.csv")
colnames(concrete)[1] <- "Cement"

#Normalize
normalize <- function(x){return ((x - min(x))/(max(x) - min(x)))}
concrete_norm <- as.data.frame(lapply(concrete,normalize))

#Separate Train and Test datasets
concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]

#Train Model
library(neuralnet)
concrete_model <- neuralnet(ConcreteCompressiveStrength ~ Cement+BlastFurnaceSlag+FlyAsh+Water+Superplasticizer+CoarseAggregate+FineAggregate+Age,data = concrete_train,hidden = 5)
#Evaluate Model
model_result <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_result$net.result
cor(predicted_strength, concrete_test$ConcreteCompressiveStrength)

#Using nnet
library(nnet)
concrete_model_nnet <- nnet(ConcreteCompressiveStrength ~ Cement+BlastFurnaceSlag+FlyAsh+Water+Superplasticizer+CoarseAggregate+FineAggregate+Age,data = concrete_train,size = 5)
model_nnet <- predict(concrete_model_nnet,concrete_test[1:8])
cor(model_nnet, concrete_test$ConcreteCompressiveStrength)
