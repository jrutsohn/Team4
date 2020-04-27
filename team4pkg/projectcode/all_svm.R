# Linear & Radial SVM
#
# These functions produced the linear and radial SVMs for the Team 4 feature selection project.  This syntax requires the imputedData.csv file to run.
# 
#
library(tidyverse)
library(caret)
dat <- read_csv("imputedData.csv")
dat_svm = dat %>% select(-duration, -default)
dat_svm$y <- as.factor(ifelse(dat_svm$y == "yes",1,0))

dat_svm_x = model.matrix(y~.,data=dat_svm)

trCtl <- trainControl(method="cv",number=5,savePredictions=T)
## Linear SVM
svmLin = train(dat_svm_x, dat_svm$y, method="svmLinear", tuneLength=3, trControl=trCtl)
## Radial SVM
svmRad = train(dat_svm_x, dat_svm$y, method="svmRadial", tuneLength=3, trControl=trCtl)
