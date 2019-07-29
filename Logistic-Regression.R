# Reading Dataset
data <- read.csv(file.choose(), header = T)

# Factor variables
data$num <- as.factor(data$num)

# Partition data (train 70%, test 30%)
set.seed(600)
ind <-sample(2,nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind ==1,]
test <- data[ind ==2,]

#install.packages("caret")
library(caret)
lrmodel <- glm(num ~., data = train, family = "binomial")
summary(lrmodel)

# Confusion matrix (train data)
p3 <- predict(lrmodel, train, type = "response")
p3 <- ifelse(p3>0.5,1,0)
(tab3 <- table(predicted = p3, Actual = train$num))
# Error
(1-sum(diag(tab3))/sum(tab3)) * 100
# Accuracy
(sum(diag(tab3))/sum(tab3)) * 100

# Prediction (Confusion matrix test data)
p4 <- predict(lrmodel, test, type = "response")
p4 <- ifelse(p4>0.5,1,0)
(tab4 <- table(predicted = p4, Actual = test$num))
# Error
(1-sum(diag(tab4))/sum(tab4)) * 100
# Accuracy
(sum(diag(tab4))/sum(tab4)) * 100

# Considering attributes which have significant value
lrmodel <- glm(num ~ + sex  + exang + ca + trestbps + thal, data = train, family = "binomial")
#summary(lrmodel)

# Confusion matrix (train data)
p3 <- predict(lrmodel, train, type = "response")
head(p3)
head(train)
p3 <- ifelse(p3>0.5,1,0)
tab3 <- table(predicted = p3, Actual = train$num)
tab3
# Error
(1-sum(diag(tab3))/sum(tab3)) * 100
# Accuracy
(sum(diag(tab3))/sum(tab3)) * 100

# Prediction (Confusion matrix test data)
p4 <- predict(lrmodel, test, type = "response")
p4 <- ifelse(p4>0.5,1,0)
tab4 <- table(predicted = p4, Actual = test$num)
tab4
# Error
(1-sum(diag(tab4))/sum(tab4)) * 100
# Accuracy
(sum(diag(tab4))/sum(tab4)) * 100