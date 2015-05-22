library(plyr)
library(dplyr)
library(caret)
library(rattle)
library(rpart.plot)
library(randomForest)
set.seed(6396)

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
train <- select(train,-raw_timestamp_part_1)
train <- select(train,-raw_timestamp_part_2)

# Drop columns that don't have relevance from the testing set
test <- select(test, -X)
test <- select(test, -user_name)
test <- select(test, -cvtd_timestamp)
test <-select(test, -new_window)
test <- select(test,-num_window)
test <- select(test,-raw_timestamp_part_1)
test <- select(test,-raw_timestamp_part_2)

# Normalize train data
value <- length(names(train)) -1
for( i in 1: value ){
  train[,i] <- as.numeric(train[,i])
  train[,i] <- scale(train[,i],center=TRUE,scale=TRUE)
}

# Normalize test data
value <- length(names(test)) -1
for( i in 1: value ){
  test[,i] <- as.numeric(test[,i])
  train[,i] <- scale(train[,i],center=TRUE,scale=TRUE)
}

dim(train)
dim(test)

# Split data into training and cross-validation sets
inTrain <- createDataPartition(y=train$classe,p=0.75,list=FALSE)
mytrain <- train[inTrain,]
myvalidation <- train[-inTrain,]

dim(mytrain)
dim(myvalidation)

# Use Rpart
modRpart <- train(classe ~ ., method="rpart",data= train)

# Check accuracy of model
val <- predict(modRpart,newdata=split$Fold3)
confusionMatrix(val,split$Fold3$classe)

dim(mytrain)
dim(myvalidation)

# Use Generalized Random Forest Model model
modFit <- train(classe ~ ., method="rf",data= mytrain,verbose=FALSE)
modFit

val <- predict(modFit,newdata=myvalidation)
confusionMatrix(val,myvalidation$classe)

# Pick random forest and use to predict test data
predict(modFit,newdata=test)


