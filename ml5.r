library(tidyverse)
library(randomForest)
library(rpart)
library(caret)
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

##Data visualization





### Test repeatedly all below code




##First model on original train data
rf1 <- randomForest(Species~., train_data)


## All subsequent models will have random sample sets from train data to reduce variance
train_sample2 <- generateSample(train_data)
rf2 <- randomForest(Species~., train_sample2)

train_sample3 <- generateSample(train_data)
rp1 <- rpart(Species~., train_sample3, method = "class")

train_sample4 <- generateSample(train_data)
rp2 <- rpart(Species~., train_sample4, method = "class")

train_sample5 <- generateSample(train_data)
ld1 <- train(x=train_sample5[,1:4], y=train_sample5[,5],method = "lda",metric = "Accuracy")

train_sample6 <- generateSample(train_data)
ld2 <- train(x=train_sample6[,1:4], y=train_sample6[,5],method = "lda",metric = "Accuracy")

train_sample7 <- generateSample(train_data)
ld3 <- train(x=train_sample7[,1:4], y=train_sample7[,5],method = "lda",metric = "Accuracy")

rf1_stat <- confusionMatrix(predict(rf1,train_data),train_data$Species)
rf2_stat <- confusionMatrix(predict(rf2,train_sample2),train_sample2$Species)
rp1_stat <- confusionMatrix(predict(rp1,train_sample3,type = "class"),train_sample3$Species)
rp2_stat <- confusionMatrix(predict(rp2,train_sample4,type = "class"),train_sample4$Species)
ld1_stat <- confusionMatrix(predict(ld1,train_sample5,type = "raw"),train_sample5$Species)
ld2_stat <- confusionMatrix(predict(ld2,train_sample6,type = "raw"),train_sample6$Species)
ld3_stat <- confusionMatrix(predict(ld3,train_sample7,type = "raw"),train_sample7$Species)


rf1_stat$overall[1:2]
rf2_stat$overall[1:2]
rp1_stat$overall[1:2]
rp2_stat$overall[1:2]
ld1_stat$overall[1:2]
ld2_stat$overall[1:2]
ld3_stat$overall[1:2]

## Let'sPredict using majority votes

rf1_p <- predict(rf1,test_data)
rf2_p <- predict(rf2,test_data)
rp1_p <- predict(rp1,test_data,type = "class")
rp2_p <- predict(rp2,test_data,type = "class")
ld1_p <- predict(ld1,test_data, type = "raw")
ld2_p <- predict(ld2,test_data, type = "raw")
ld3_p <- predict(ld3,test_data)

combined <- cbind(test_data,rf1_p,rf2_p,rp1_p,rp2_p,ld1_p,ld2_p,ld3_p)

votes <- combined[,6:12]
majority <- apply(votes,1,function(x) names(which.max(table(x))))

combined <- cbind(combined,majority)

cat("\n\nMajority vote predictions are \n\n")
combined


## Fetching unmatched results with majority voted predictions. Error cases


voteMissed <- combined[which(combined$majority!=combined$Species),]
voteMissed


vote_model_error_rate <- (nrow(voteMissed)/nrow(combined))*100

paste("Accuracy of majority voting method ","%f",(100-vote_model_error_rate))

## Let's Also pick best model rather than majority voting

all_results <- rbind(rf1_stat$overall,rf2_stat$overall,rp1_stat$overall,rp2_stat$overall,ld1_stat$overall,ld2_stat$overall,ld3_stat$overall)
all_results <- as.data.frame(all_results)
all_results$model_name <- c("rf1","rf2","rp1","rp2","ld1","ld2","ld3")
all_results$pred_index <- c(6,7,8,9,10,11,12)

all_results <- plyr::arrange(all_results,desc(Accuracy),desc(Kappa))
all_results

best_model <- all_results[1,8]

cat(paste("\n\nBest model out of all is ",best_model,"\n"))
cat(paste("\n\nStats for best model ",best_model," are\n"))
all_results[1,]

best_pred_index <- all_results[1,9]

best_predict <- combined[,c(1:5,best_pred_index)]

cat(paste("Results of ",best_model," predictions as below \n\n"))
best_predict

cat(paste("\n\nBest models ",best_model,"mismatch case\n\n"))
bestMissed <- best_predict[which(best_predict$Species!=best_predict[,6]),]
bestMissed

best_model_error_rate <- (nrow(bestMissed)/nrow(combined))*100

paste("Accuracy of best model selection ","%f",(100-best_model_error_rate))
