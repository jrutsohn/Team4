## Group4

library(data.table)
data <- fread("C:/Users/samsung/Desktop/BIOS/735/Project/bank-additional/bank-additional/bank-additional-full.csv")


## looking into data

View(data)
names(data)
dplyr::glimpse(data)


sum(is.na(data))  ## No missing?? Missing is coded with "unknown". Also, this data is imbalanced because
sum(data$y == "yes") / nrow(data)

# as.factor
data$job         <- as.factor(data$job)
data$marital     <- as.factor(data$marital)
data$education   <- as.factor(data$education) 
data$default     <- as.factor(data$default)  
data$housing     <- as.factor(data$housing)  
data$loan        <- as.factor(data$loan) 
data$contact     <- as.factor(data$contact)  
data$month       <- as.factor(data$month) 
data$day_of_week <- as.factor(data$day_of_week)
data$poutcome    <- as.factor(data$poutcome)  
data$y           <- as.factor(data$y)


library(tidyverse)
data.test <- data
ggplot(data=data.test) + geom_bar(aes(x=age)) ## no unknown
ggplot(data=data.test) + geom_bar(aes(x=job)) ## small unknown
ggplot(data=data.test) + geom_bar(aes(x=marital)) ## small unknown
ggplot(data=data.test) + geom_bar(aes(x=education)) ## small unknown
ggplot(data=data.test) + geom_bar(aes(x=default)) ## some unknown
ggplot(data=data.test) + geom_bar(aes(x=housing)) ## some unknown
ggplot(data=data.test) + geom_bar(aes(x=loan)) ## some unknown


ggplot(data=data.test) + geom_bar(aes(x=contact)) ## no unknown
ggplot(data=data.test) + geom_bar(aes(x=month)) ## no unknown
ggplot(data=data.test) + geom_bar(aes(x=day_of_week)) ## no unknown
ggplot(data=data.test) + geom_bar(aes(x=duration)) ## no unknown

ggplot(data=data.test) + geom_bar(aes(x=campaign)) ## no unknown
ggplot(data=data.test) + geom_bar(aes(x=pdays))    ## no unknown// 999 means client was not previously contacted)
  data.test %>%
    filter(pdays < 50) %>%
    ggplot() + geom_bar(aes(x=pdays)) 
ggplot(data=data.test) + geom_bar(aes(x=previous)) ## no unknown
ggplot(data=data.test) + geom_bar(aes(x=poutcome)) ## assume no unknown

ggplot(data=data.test) + geom_bar(aes(x=emp.var.rate)) ## no unknown
ggplot(data=data.test) + geom_bar(aes(x=cons.price.idx)) ## no unknown
ggplot(data=data.test) + geom_bar(aes(x=cons.conf.idx))  ## no unknown
ggplot(data=data.test) + geom_bar(aes(x=euribor3m))      ## no unknown
ggplot(data=data.test) + geom_bar(aes(x=nr.employed))    ## no unknown

ggplot(data=data.test) + geom_bar(aes(x=y))  ## no unknown


###
# I think we don't need the variables 'month' and 'day_of_week' indicating the last contact day
# 999 in `pdays` means client was not previously contacted. I think setting this 0 instead of 999 might be better. It depends on the model we choose.

# Only categorical variables has unknown

data.test <- data.test[,-c(9, 10)]  ## remove 'month' and 'day_of_week'

# Let's replace unknown to NA
library(naniar)
sum(data.test=="unknown", na.rm = T)
data.test <- data.test %>%
  replace_with_na(replace = list(job="unknown", marital="unknown", education="unknown", 
                                 default="unknown", housing="unknown", loan="unknown"))
sum(data.test=="unknown", na.rm = T) #double check

# Complete data
complete <- data.test[complete.cases(data.test), ]
sum(is.na(complete))

# Artificially produce missing values using the 'prodNA' function (simulation purpose)
library(missForest) 
n <- nrow(data.test)

set.seed(1)
sim1 <- complete
sim1$job <- prodNA( data.frame(job=complete$job) , sum(is.na(data.test$job))/ n)
sim1$marital   <- prodNA( data.frame(marital=complete$marital) , sum(is.na(data.test$marital))/ n)
sim1$education <- prodNA( data.frame(education=complete$education) , sum(is.na(data.test$education))/ n)
sim1$default   <- prodNA( data.frame(default=complete$default) , sum(is.na(data.test$default))/ n)
sim1$housing   <- prodNA( data.frame(housing=complete$housing) , sum(is.na(data.test$housing))/ n)
sim1$loan      <- prodNA( data.frame(loan=complete$loan) , sum(is.na(data.test$loan))/ n)


set.seed(2)
sim2 <- complete
sim2$job <- prodNA( data.frame(job=complete$job) , sum(is.na(data.test$job))/ n)
sim2$marital   <- prodNA( data.frame(marital=complete$marital) , sum(is.na(data.test$marital))/ n)
sim2$education <- prodNA( data.frame(education=complete$education) , sum(is.na(data.test$education))/ n)
sim2$default   <- prodNA( data.frame(default=complete$default) , sum(is.na(data.test$default))/ n)
sim2$housing   <- prodNA( data.frame(housing=complete$housing) , sum(is.na(data.test$housing))/ n)
sim2$loan      <- prodNA( data.frame(loan=complete$loan) , sum(is.na(data.test$loan))/ n)


set.seed(3)
sim3 <- complete
sim3$job <- prodNA( data.frame(job=complete$job) , sum(is.na(data.test$job))/ n)
sim3$marital   <- prodNA( data.frame(marital=complete$marital) , sum(is.na(data.test$marital))/ n)
sim3$education <- prodNA( data.frame(education=complete$education) , sum(is.na(data.test$education))/ n)
sim3$default   <- prodNA( data.frame(default=complete$default) , sum(is.na(data.test$default))/ n)
sim3$housing   <- prodNA( data.frame(housing=complete$housing) , sum(is.na(data.test$housing))/ n)
sim3$loan      <- prodNA( data.frame(loan=complete$loan) , sum(is.na(data.test$loan))/ n)


set.seed(4)
sim4 <- complete
sim4$job <- prodNA( data.frame(job=complete$job) , sum(is.na(data.test$job))/ n)
sim4$marital   <- prodNA( data.frame(marital=complete$marital) , sum(is.na(data.test$marital))/ n)
sim4$education <- prodNA( data.frame(education=complete$education) , sum(is.na(data.test$education))/ n)
sim4$default   <- prodNA( data.frame(default=complete$default) , sum(is.na(data.test$default))/ n)
sim4$housing   <- prodNA( data.frame(housing=complete$housing) , sum(is.na(data.test$housing))/ n)
sim4$loan      <- prodNA( data.frame(loan=complete$loan) , sum(is.na(data.test$loan))/ n)


set.seed(5)
sim5 <- complete
sim5$job <- prodNA( data.frame(job=complete$job) , sum(is.na(data.test$job))/ n)
sim5$marital   <- prodNA( data.frame(marital=complete$marital) , sum(is.na(data.test$marital))/ n)
sim5$education <- prodNA( data.frame(education=complete$education) , sum(is.na(data.test$education))/ n)
sim5$default   <- prodNA( data.frame(default=complete$default) , sum(is.na(data.test$default))/ n)
sim5$housing   <- prodNA( data.frame(housing=complete$housing) , sum(is.na(data.test$housing))/ n)
sim5$loan      <- prodNA( data.frame(loan=complete$loan) , sum(is.na(data.test$loan))/ n)

#Now, let's impute missing of simulated date and see the accuracy.
result1_mf <- missForest(sim1, xtrue = complete, verbose = TRUE)
result2_mf <- missForest(sim2, xtrue = complete, verbose = TRUE)
result3_mf <- missForest(sim3, xtrue = complete, verbose = TRUE)
result4_mf <- missForest(sim4, xtrue = complete, verbose = TRUE)
result5_mf <- missForest(sim5, xtrue = complete, verbose = TRUE)




## Updating...........
















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
