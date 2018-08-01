library(tidyverse)
library(randomForest)
library(rpart)
library(caret)
library(glmnet)
library(lda)
library(datasets)
library(corrplot)

generateSample <- function(x)
{
  y <- x[sample(nrow(x), 276,replace=T), ]
  return(y)
}

#Attaching dataset
data <- datasets::iris



#Splitting data
sample <- sample.int(n = nrow(data), size = floor(.70*nrow(data)), replace = F)

train_data <- data[sample,] #70% of original dataset
test_data <- data[-sample,]

##First model on original train data
rf1 <- train(x=train_data[,1:4], y=train_data[,5], method = "rf", metric = "Accuracy")


## All subsequent models will have random sample sets from train data to reduce variance
train_sample2 <- generateSample(train_data)
rf2 <- train(x=train_sample2[,1:4], y=train_sample2[,5], method = "rf", metric = "Accuracy")

train_sample3 <- generateSample(train_data)
rp1 <- train(x=train_sample3[,1:4], y=train_sample3[,5], method = "rpart", metric = "Accuracy")

train_sample4 <- generateSample(train_data)
rp2 <- train(x=train_sample4[,1:4], y=train_sample4[,5], method = "rpart", metric = "Accuracy")


confusionMatrix(predict(rf1,test_data),test_data$Species)
confusionMatrix(predict(rf2,test_data),test_data$Species)
confusionMatrix(predict(rp1,test_data),test_data$Species)
confusionMatrix(predict(rp2,test_data),test_data$Species)


## Let'sPredict using majority votes

rf1_p <- predict(rf1,test_data)
rf2_p <- predict(rf2,test_data)
rp1_p <- predict(rp1,test_data,type = "raw")
rp2_p <- predict(rp2,test_data,type = "raw")

combined <- cbind(test_data,rf1_p,rf2_p,rp1_p,rp2_p)

votes <- combined[,c(6,7,8,9)]
majority <- apply(votes,1,function(x) names(which.max(table(x))))

combined <- cbind(combined,majority)

combined[which(combined$majority!=combined$Species),]


