library(tidyverse)
library(randomForest)
library(rpart)
library(caret)
library(glmnet)
library(lda)
library(datasets)
library(corrplot)

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

fit1 <- randomForest(factor(Species)~.,data=train_data,ntree=100,proximity=TRUE)

fit1$importance

fit1$confusion

fit1$oob.times