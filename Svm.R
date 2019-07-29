#install.packages("dplyr")
library(dplyr)

svm1 <- function(region){

  Mode <- function(x){
    x <- na.omit(x)
    ux <- unique(x)
    ux[which.max(tabulate(match(x,ux)))]
  }

  dataset <- read.csv(file="Dataset_Heart.csv", header = T)

  # Preprocessing
  a <- mean(dataset$trestbps,na.rm = TRUE)

  for(i in 1:NROW(dataset))
  {
    if(is.na(dataset$trestbps[i])==TRUE)
    {
      dataset$trestbps[i] <- a
    }
  }
  a <- mean(dataset$chol,na.rm=TRUE)

  for(i in 1:NROW(dataset))
  {
    if(is.na(dataset$chol[i])==TRUE)
    {
      dataset$chol[i] <- a
    }
  }
  a <- Mode(dataset$fbs)

  for(i in 1:NROW(dataset))
  {
    if(is.na(dataset$fbs[i])==TRUE)
    {
      dataset$fbs[i] <- a
    }
  }
  a <- Mode(dataset$restecg)

  for(i in 1:NROW(dataset))
  {
    if(is.na(dataset$restecg[i])==TRUE)
    {
      dataset$restecg[i] <- a
    }
  }
  a <- Mode(dataset$thalach)

  for(i in 1:NROW(dataset))
  {
    if(is.na(dataset$thalach[i])==TRUE)
    {
      dataset$thalach[i] <- a
    }
  }
  a <- Mode(dataset$exang)

  for(i in 1:NROW(dataset))
  {
    if(is.na(dataset$exang[i])==TRUE)
    {
      dataset$exang[i] <- a
    }
  }
  a <- mean(dataset$oldpeak,na.rm=TRUE)

  for(i in 1:NROW(dataset))
  {
    if(is.na(dataset$oldpeak[i])==TRUE)
    {
      dataset$oldpeak[i] <- a
    }
  }
  a <- Mode(dataset$slope)

  for(i in 1:NROW(dataset))
  {
    if(is.na(dataset$slope[i])==TRUE)
    {
      dataset$slope[i] <- a
    }
  }
  a <- Mode(dataset$ca)

  for(i in 1:NROW(dataset))
  {
    if(is.na(dataset$ca[i])==TRUE)
    {
      dataset$ca[i] <- a
    }
  }
  a <- Mode(dataset$thal)

  for(i in 1:NROW(dataset))
  {
    if(is.na(dataset$thal[i])==TRUE)
    {
      dataset$thal[i] <- a
    }
  }
  # Preprocessing ends


  dataset <- dataset[dataset[, "region"] == region,]
  dataset<-dataset[,-ncol(dataset)]
  #print(dataset)
  library(caret)
  dataset$num<-as.factor(dataset$num)
  levels(dataset$num) <- c("NotDisease","Disease")

  # Cross validation
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 10,
                             # Estimate class probabilities
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)
  svmModel <- train(num ~ ., data = dataset,
                    method = "svmRadial",
                    trControl = fitControl,
                    tuneLength = 8,
                    metric = "ROC")


  newtest <- read.csv(file.choose(), header = T)
  newtest=newtest[,-1]
  names(newtest) <- c("age", "sex","cp","trestbps","chol","fbs",
                      "restecg","thalach","exang","oldpeak","slope","ca","thal")
  newtest<-tail(newtest,1)
  #print(newtest)

  svmPrediction <<- predict(svmModel,newtest)
  svmPredictionprob <- predict(svmModel,newtest, type='prob')[2]

  result=svmPredictionprob[1, "Disease"]*100
  result=round(result,digits = 2)
  print(paste0(result,"%"))

  }



#a,d,t represents asthama, diabetes and thyroid values respectively which are passed from shiny ui
recommend <- function(a,d,t){
  
  options(digits=4)
  data <- read.csv(file="Lifestyle.csv", header = T)
  datanew <- select (data,-c(id,items))
  
  h=ifelse(svmPrediction=="NotDisease",0,1)
  i=ifelse(a==0,0,1)
  j=ifelse(d==0,0,1)
  k=ifelse(t==0,0,1)
  tes<-c(h,i,j,k)
  
  Cosinefun <- function(x,y)
  {
    this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(this.cosine)
  }
  
  similarity<-matrix(NA, nrow = 84, ncol = 2)
  
  for(i in 1:nrow(datanew)) {
    similarity[i,]= Cosinefun(tes,datanew[i,])
  }
  
  similarity[,1]<-c(1:84)
  similarity<-similarity[order(similarity[,2],decreasing = TRUE),]
  index<-t(c(similarity[1:8,1]))
  for(k in 1:ncol(index))
  {
    for (l in 1:nrow(data))
    {
      if(index[1,k]==data$id[l])
      {
        print(data$items[l], max.levels = 0)
      }
    }
  }
  
}
