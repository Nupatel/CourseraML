setwd("C:/Users/Neil/SkyDrive/DataScience/MachineLearning")

#### Reading in data ####
trainurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
dest.training <- "data.training.csv"
testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
dest.testing <- "data.testing.csv"
download.file(trainurl,dest.training); download.file(testurl,dest.testing)

data <- read.csv("data.training.csv", na.strings = c("NA", ""))
#### ####

library(caret)
library(gbm)
set.seed(0)
inTrain <- createDataPartition(y = data$classe, p = 0.7, list = FALSE)
training <- data[inTrain, ]
testing <- data[-inTrain, ]

## Manage NAs
nzv <- nearZeroVar(training)
training <- training[, -nzv]
testing <- testing[, -nzv]

NAcols <- sapply(training, function(x) mean(is.na(x))) > 0.7
training <- training[,NAcols==FALSE]
testing <- testing[,NAcols == FALSE]

training <- training[,-c(1:5)]
testing <- testing[,-c(1:5)]




##Random Forest
fitRF <- train(classe~., data = training, method = "rf")

predRF <- predict(fitRF, newdata = testing)
confusionMatrix(testing$classe, predRF)

predRF <- as.character(predRF)

data.testing <- read.csv("data.testing.csv")
predRF.testing <- as.character(predict(fitRF, newdata = data.testing))



pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

setwd("C:/Users/Neil/SkyDrive/DataScience/MachineLearning/submitFiles")

pml_write_files(predRF.testing)
##Decision Tree
##fitDT <- train(classe~., data = training, method = "rpart")
##Boosting
##fitGBM <- train(classe~., data=training, method = "gbm")
