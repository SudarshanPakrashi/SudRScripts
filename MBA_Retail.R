library(arules)
groceries <- read.transactions("https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/groceries.csv",sep = ",")

#Model
groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))

#Sort
inspect(sort(groceryrules, by = "lift")[1:5])
#Subset
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)
