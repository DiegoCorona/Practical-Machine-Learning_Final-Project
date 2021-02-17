##### Practical Machine Learning #####
#### Project ####
library(dplyr)
library(caret)
library(randomForest)
library(e1071)
library(rpart)

training <- read.csv('/cloud/project/pml-training.csv', header = T)
testing <- read.csv('/cloud/project/pml-testing.csv', header = T)
training <- mutate(training, classe = as.factor(classe))
###checking NA
sapply(training, function(x) sum(is.na(x)))
### delete NA
col_to_delete <- which(colMeans(is.na(training))>0)
training[,col_to_delete] <- NULL
x <- c()
for(i in 1:length(training[1,])){
  if(class(training[1, i])== "character"){
    x[i] <- i
  }
}
x <- x[!is.na(x)]
training <- training[, -x]
training <- training[, -c(1, 2,3,4)]####### divide


set.seed(3636)
inTrain <- createDataPartition(y=training$classe, p = 0.7, list = FALSE)
train <- training[inTrain, ] ### train
validation <- training[-inTrain, ] ### validation 

dim(train); dim(validation)


### Modelo 1
set.seed(12345)
modFitDecTree <- rpart(classe ~ ., data=train, method="class")
fancyRpartPlot(modFitDecTree)
predictDecTree <- predict(modFitDecTree, newdata=validation, type="class")
confMatDecTree <- confusionMatrix(table(predictDecTree, validation$classe))
confMatDecTree ### Accuracy : 0.7482 

nearZeroVar(train, saveMetrics = TRUE)
### Modelo 2
training_1 <- training
training_1$gyros_belt_x <- NULL
training_1$gyros_belt_y <- NULL
training_1$gyros_belt_z <- NULL
training_1$gyros_arm_x <- NULL
training_1$gyros_arm_y <- NULL
training_1$gyros_arm_z <- NULL
training_1$gyros_dumbbell_x <- NULL
training_1$gyros_dumbbell_y <- NULL
training_1$gyros_dumbbell_z <- NULL
training_1$gyros_forearm_x <- NULL
training_1$gyros_forearm_y <- NULL
training_1$gyros_forearm_z <- NULL

set.seed(3632)
inTrain <- createDataPartition(y=training_1$classe, p = 0.7, list = FALSE)
train <- training_1[inTrain, ] ### train
validation <- training_1[-inTrain, ] ### validacion 

set.seed(999)
modFitDecTree <- rpart(classe ~ ., data=train, method="class")
fancyRpartPlot(modFitDecTree)
predictDecTree <- predict(modFitDecTree, newdata=validation, type="class")
confMatDecTree <- confusionMatrix(table(predictDecTree, validation$classe))
confMatDecTree ### Accuracy : 0.737

#### svm
set.seed(766)
svm_fit <- svm(classe~., data = train)
pred <- predict(svm_fit, newdata=validation, type="class")
confM <- confusionMatrix(table(pred, validation$classe))
confM  ### Accuracy : 0.9251 


test <- testing[, names(training_1)[1:length(names(training_1))-1]]
predictions <- predict(svm_fit, newdata = test)  #### TODAS SON CORRECTAS 
predictions
