wisc <- read.csv("http://www3.nd.edu/~steve/computing_with_data/Data/wisc_bc_data.csv")
wisc <- wisc[-1]
normalize <- function(x){return ((x - min(x))/(max(x) - min(x)))}
wisc_n <- as.data.frame(lapply(wisc[-1],normalize))
wisc_train <- wisc_n[1:469,]
wisc_test <- wisc_n[470:569,]
wisc$diagnosis <- factor(wisc$diagnosis,levels = c("B","M"),labels = c("Benign","Malignant"))
wisc_train_diag <- wisc[1:469,1]
wisc_test_diag <- wisc[470:569,1]
library(class)
wbc_pred <- knn(train = wisc_train,test = wisc_test,cl = wisc_train_diag,k = 21)

library(gmodels)
CrossTable(x = wisc_test_diag,y = wbc_pred,prop.chisq = FALSE)

wisc_z <- as.data.frame(scale(wisc[-1]))
wisc_train_z <- wisc_z[1:469,]
wisc_test_z <- wisc_z[470:569,]

wbc_pred_z <- knn(train = wisc_train_z,test = wisc_test_z,cl = wisc_train_diag,k = 21)
CrossTable(x = wisc_test_diag,y = wbc_pred_z,prop.chisq = FALSE)