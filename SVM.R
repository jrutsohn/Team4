library(tidyverse)
library(caret)

dat = read_csv("imputedData.csv")

# To show codes on a smaller dataset: dat = sample_n(dat, 1000)

## duration was mentioned in the readme file that it should not be used
## default only has three people answering "yes", so I am not using it
dat_svm = dat %>% select(-duration, -default)
dat_svm$y = as.factor(ifelse(dat_svm$y == "yes", 1, 0))

dat_svm_x = model.matrix(y~., data = dat_svm)
trCtl <- trainControl(method="cv", number=5, savePredictions=TRUE)

## svm with linear kernel
svmLin = train(dat_svm_x, dat_svm$y, method = "svmLinear", 
               tuneLength = 3, trControl = trCtl)

# svm with radial kernel
start = Sys.time()
svmRad = train(dat_svm_x, dat_svm$y, method = "svmRadial", 
               tuneLength = 3, trControl = trCtl)
end = Sys.time()
