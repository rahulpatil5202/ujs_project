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
  y <- x[sample(nrow(x), 500,replace=T), ]
  return(y)
}

#Attaching dataset
data <- datasets::iris



#Splitting data
sample <- sample.int(n = nrow(data), size = floor(.70*nrow(data)), replace = F)

train_data <- data[sample,] #70% of original dataset
test_data <- data[-sample,]

rf1 <- randomForest(factor(Species)~Sepal.Length+Petal.Length+Petal.Width,data=train_data)


#train_data2 <- generateSample(train_data)
rf2 <- randomForest(factor(Species)~Sepal.Length+Petal.Length+Petal.Width,data=generateSample(train_data))

rp1 <- rpart(factor(Species)~.,data=generateSample(train_data))
rp2 <- rpart(factor(Species)~.,data=generateSample(train_data))


rf1$confusion
rf2$confusion

confusionMatrix(predict(rp1,test_data,type = "class"),test_data$Species)
confusionMatrix(predict(rp2,test_data,type = "class"),test_data$Species)


p1 <- predict(rf1,test_data)
p2 <- predict(rf2,test_data)
p3 <- predict(rp1,test_data,type = "class")
p4 <- predict(rp2,test_data,type = "class")

combined <- cbind(test_data,p1,p2,p3,p4)

votes <- combined[,c(6,7,8,9)]
majority <- apply(votes,1,function(x) names(which.max(table(x))))

combined <- cbind(combined,majority)

combined[which(combined$majority!=combined$Species),]


