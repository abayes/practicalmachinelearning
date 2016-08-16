setwd("//cvgfnp004/cincinnati/home/MyDocs/DataScienceCert/08-Practical Machine Learning/Course Project")
library(caret)
library(ggplot2)
library(dplyr)
library(rattle)
library(parallel)
library(doParallel)

data <- read.csv("pml-training.csv")
set.seed(100)
inTrain <- createDataPartition(y = data$classe, p = .60, list = F)

training <- data[inTrain,]
testing <- data[-inTrain,]

str(training, list.len = 160)

# Make the columns numeric
training[8:159] <- apply(training[8:159], 2, function(x) as.numeric(as.character(x)))
# Remove all the columns with NA values
col_keep <- apply(training, 2, function(x) !any(is.na(x)))
# Remove identifier columns and class
col_keep[c(1:7, 160)] <- FALSE
col_keep <- which(col_keep)

training[8:159] <- apply(training[8:159], 2, function(x) as.numeric(as.character(x)))

x <- training[,col_keep]
y <- training[,160]


cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

fitControl <- trainControl(method = "cv",
                           number = 10,
                           allowParallel = TRUE)

mod <- train(x,y, method="rf",data=training,trControl = fitControl)

stopCluster(cluster)

mod
mod$resample
confusionMatrix.train(mod)





