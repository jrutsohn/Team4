## Group4

library(data.table)
data <- fread("../bank/bank-full.csv")
#View(data)
names(data)
dplyr::glimpse(data)
sum(is.na(data))  ## No missing ! but imbalanced because
sum(data$y == "yes") / nrow(data)

#logistic regression //
data.logistic <- data
data.logistic$y <- ifelse(data.logistic$y=="yes", 1, 0 )
logitMod <- glm(y ~ . , data=data.logistic, family=binomial(link="logit") )
summary(logitMod)


## Decision tree without any validation set ; faster option.
library(rpart)
library(rpart.plot)
start = Sys.time()
dt.fit <- rpart( y~ ., data=data)
end = Sys.time()
end-start
printcp(dt.fit)
rpart.plot(dt.fit)

## Random forest
library(caret)
# start = Sys.time()
# 
# trCtl <- trainControl(savePredictions=TRUE)
# rf.fit <- train(y~ . , data=data, method="rf", trControl=trCtl)
# end = Sys.time()
# 
# end-start
# rffit$results[rownames(rffit$bestTune),]

#training set proportion
p <- 0.9
set.seed(1)
index <- sample( nrow(data), nrow(data)*p)
# or we can use trainControl function; trCtl <- trainControl(method = "LGOCV", number = 1, p) 

data.train <- data[index,]
data.test <- data[-index,]
start = Sys.time()
rf.fit <- train(y~ . , data=data.train, method="rf", trControl=trainControl(method="none"))
end = Sys.time()
end-start
mean(predict(rf.fit, data.test) == data.test$y) # accuracy


#SVM with Radial kernel
start = Sys.time()
svm.fit <- train(y~ . , data=data.train, method="svmRadial", trControl=trainControl(method="none")) 
end = Sys.time()
end-start
mean(predict(svm.fit, data.test) == data.test$y) # accuracy


?train
