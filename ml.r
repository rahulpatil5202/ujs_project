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
  y <- x[sample(nrow(x), nrow(x),replace=T), ]
  return(y)
}

#Attaching dataset
data <- datasets::iris

#Converting factors to characters
data$Species <- as.character(data$Species)
str(data)

#Check
head(data)


#Visualizing relationship between variables - combination of 2

ggplot(data, aes(Sepal.Length, Sepal.Width, color = Species, shape = Species))+
  geom_point()

ggplot(data, aes(Petal.Length, Petal.Width, color = Species, shape = Species))+
  geom_point()

ggplot(data, aes(Sepal.Length, Petal.Length, color = Species, shape = Species))+
  geom_point()

ggplot(data, aes(Sepal.Width, Petal.Width, color = Species, shape = Species))+
  geom_point()

ggplot(data, aes(Sepal.Length, Petal.Width, color = Species, shape = Species))+
  geom_point()

ggplot(data, aes(Sepal.Width, Petal.Length, color = Species, shape = Species))+
  geom_point()


#Visualize complete combination co-rrelation

crm <- cor(data[,-5])

corrplot(crm, method = 'color', type = 'lower')

#Chart proves all variables are interrelated but sepal width has little co-rellation


#Splitting data
sample <- sample.int(n = nrow(data), size = floor(.70*nrow(data)), replace = F)

train_data <- data[sample,] #70% of original dataset
test_data <- data[-sample,]

rf1 <- randomForest(factor(Species)~.,data=train_data)


#train_data2 <- generateSample(train_data)
rf2 <- randomForest(factor(Species)~.,data=generateSample(train_data))
rf3 <- randomForest(factor(Species)~.,data=generateSample(train_data))
rf4 <- randomForest(factor(Species)~.,data=generateSample(train_data))

rf1$confusion
rf2$confusion
rf3$confusion
rf4$confusion

p1 <- predict(rf1,test_data)
p2 <- predict(rf2,test_data)
p3 <- predict(rf3,test_data)
p4 <- predict(rf4,test_data)

combined <- cbind(test_data,p1,p2,p3,p4)

votes <- combined[,c(6,7,8,9)]
majority <- apply(votes,1,function(x) names(which.max(table(x))))

combined <- cbind(combined,majority)

combined[which(combined$majority!=combined$Species),]


rp1 <- rpart(factor(Species)~.,data=generateSample(train_data))
p5 <- predict(rp1,test_data, type="class")
p5 <- as.data.frame(p5)
combined <- cbind(combined,p5)
combined[which(combined$majority!=combined$Species),]
combined[which(combined$p5!=combined$Species),]

