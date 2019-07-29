# Reading Dataset
data <- read.csv(file.choose(), header = T)

# Normalize(min-max)
hist(data$age)
data[,-ncol(data)] <- (data[,-ncol(data)] - min(data[,-ncol(data)], na.rm=TRUE))/(max(data[,-ncol(data)],na.rm=TRUE)
                                                                                  - min(data[,-ncol(data)], na.rm=TRUE))
hist(data$age)
#head(data)
# Partition data (train 70%, test 30%)
set.seed(222)
ind <- sample(2, nrow(data), replace =T, prob = c(0.7,0.3))
train <-data[ind==1,]
test <- data[ind ==2,]

# Neural network model
#install.packages("neuralnet")
library(neuralnet)
#nmodel <- neuralnet(num ~., data = train, hidden = 4, stepmax=1e6)
nmodel <- neuralnet(num ~., data = train, hidden = c(2,1))
plot(nmodel)

# Prediction (train)
#print(train[,-ncol(train)])
pred <- compute(nmodel,train[,-ncol(train)])
#pred
head(pred$net.result)
head(train)
#  Confusion matrix (train data)
p5 <- pred$net.result
p5 <- ifelse(p5>0.5,1,0)
(tab5 <- table(predicted=p5, Actual = train$num))
# Error
(1- sum(diag(tab5)/sum(tab5))) * 100
# Accuracy
(sum(diag(tab5)/sum(tab5))) * 100

# Prediction (test)
pred <- compute(nmodel,test[,-ncol(test)])
# Confusion matrix (test data)
p6 <- pred$net.result
p6 <- ifelse(p6>0.5,1,0)
(tab6 <- table(predicted=p6, Actual = test$num))
# Error
(1- sum(diag(tab6)/sum(tab6))) * 100
# Accuracy
(sum(diag(tab6)/sum(tab6))) * 100
