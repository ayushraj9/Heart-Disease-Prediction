# Reading Dataset
data <- read.csv(file.choose(), header = T)
#print(data)

# Factor variables
data$num <- as.factor(data$num)
#str(data)

# Partition data (train 70%, test 30%)
set.seed(511)
ind <-sample(2,nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind ==1,]
test <- data[ind ==2,]

# Implementing tree model
#install.packages("tree")
library(tree)
#install.packages("rpart")
library(rpart)
tree_model <- rpart(num ~ ., train)
plot(tree_model)
text(tree_model, pretty = 0)

# Confusion matrix (test data)
p2 <- predict(tree_model, test, type = 'class')

(tab1 <- table(predicted = p2, Actual = test$num))
# Error
(1 - sum(diag(tab1))/sum(tab1)) * 100
#Accuracy
(acc<-(sum(diag(tab1))/sum(tab1)) * 100)


# Pruning the tree
pruned_model = prune.misclass(tree_model, best = 10)
plot(pruned_model)
text(pruned_model, pretty = 0)

# Prediction (Confusion matrix test data)
p2 <- predict(pruned_model, test, type = 'class')
(tab2 <- table(predicted = p2, Actual = test$num))
# Error
(1 - sum(diag(tab2))/sum(tab2)) * 100
# Accuracy
(sum(diag(tab2))/sum(tab2)) * 100

#install.packages('pROC')
library(pROC)
Predictionwithprobs <- predict(tree_model,test,type = 'prob')
auc<-auc(test$num,Predictionwithprobs[,2])
plot(roc(test$num,Predictionwithprobs[,2]))


