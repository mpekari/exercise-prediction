library(plyr)
library(dplyr)
library(caret)
library(rattle)
library(rpart.plot)
library(gbm)

# Read the training and test data
training <- read.csv("pml-training.csv",na.strings = c(NA,""))
testing <- read.csv("pml-testing.csv",na.strings = c(NA,""))


# Check for columns for which is.na <0.97
a <- colMeans(is.na(training)) < .97

# Remove these columns from the testing and training set
train <- training[, a]
test <- testing[,a]

# Remove irrelevant columns
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

# Normalize train data
value <- length(names(train)) -1
for( i in 1: value ){
  train[,i] <- as.numeric(train[,i])
  train[,i] <- (train[,i] - mean(train[,i]))/sd(train[,i])
}

# Normalize test data
value <- length(names(test)) -1
for( i in 1: value ){
  test[,i] <- as.numeric(test[,i])
  test[,i] <- (test[,i] - mean(test[,i]))/sd(test[,i])
}

dim(train)
dim(test)

# Create 3 folds for cross validation
folds <- createFolds(train$classe,3)
str(folds)

# Split up the data
split <- lapply(folds, function(ind, dat) dat[ind,], dat = train)

# Use Rpart
modRpart <- train(classe ~ ., method="rpart",data= train)

# Check accuracy of model
val <- predict(modRpart,newdata=split$Fold3)
confusionMatrix(val,split$Fold3$classe)

# Use gbm for fitting the model
modFit <- train(classe ~ ., method="gbm",data= train,verbose=FALSE)
modFit

# Check accuracy
val <- predict(modFit,newdata=split$Fold3)
confusionMatrix(val,split$Fold3$classe)

# Pick gbm and use to predict test data
predict(modFit,newdata=test)


