library(plyr)
library(dplyr)
library(caret)
library(rattle)
library(rpart.plot)
library(gbm)
training <- read.csv("pml-training.csv",na.strings = c(NA,""))
testing <- read.csv("pml-testing.csv",na.strings = c(NA,""))



a <- colMeans(is.na(training)) < .97

train <- training[, a]
test <- testing[,a]


train <- select(train, -X)
train <- select(train, -user_name)
train <- select(train, -cvtd_timestamp)
train <-select(train, -new_window)
train <- select(train,-num_window)

# Drop columns that don't have relevance from the testing set
test <- select(test, -X)
test <- select(test, -user_name)
test <- select(test, -cvtd_timestamp)
test <-select(test, -new_window)
test <- select(test,-num_window)

value <- length(names(train)) -1
for( i in 1: value ){
  train[,i] <- as.numeric(train[,i])
  train[,i] <- (train[,i] - mean(train[,i]))/sd(train[,i])
}

# Testing set
value <- length(names(test)) -1
for( i in 1: value ){
  test[,i] <- as.numeric(test[,i])
  test[,i] <- (test[,i] - mean(test[,i]))/sd(test[,i])
}

dim(train)
dim(test)

folds <- createFolds(train$classe,3)
str(folds)

s <- lapply(folds, function(ind, dat) dat[ind,], dat = train)

modFit <- train(classe ~ ., method="rf",data= split_up$Fold1)

val <- predict(modFit,newdata=s$Fold3)
confusionMatrix(val,s$Fold3$classe)

unlist(lapply(split_up, nrow))

modFit <- train(classe ~ ., method="rpart",data= train)

modFit <- train(classe ~ ., method="gbm",data= train,verbose=FALSE)
predict(modFit,newdata=test)

val <- predict(modFit,newdata=s$Fold2)
confusionMatrix(val,s$Fold2$classe)
