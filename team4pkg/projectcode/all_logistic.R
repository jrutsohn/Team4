#Compares the Logistic Regression Code we produced to the GLM code for accuracy and duration.  Also used to compare LR results for prediction with SVM results later.
#Requires input of 'imputedData.csv'

library(tidyverse)
library(glmnet)
library(caret)
library(modelr)
dat = read_csv("imputedData.csv")
dat_lasso = dat %>% select(-duration, -default)
dat_lasso$y = ifelse(dat_lasso$y == "yes", 1, 0)
dat_lasso_matrix = model.matrix(y~., data=dat_lasso)
dim(dat_lasso_matrix)
set.seed(4)

#First evaluating LASSO with glmnet
cv_lasso = cv.glmnet(dat_lasso_matrix, dat_lasso$y, alpha = 1, nfolds = 5,
                     family = "binomial", type.measure = "auc")
plot(cv_lasso)

#Self-written code takes substantially longer at nearly 20 minutes compared to glm() with about a 1 second.  However
#both sets of results were nearly identical.
start = Sys.time()
glm_logit = glm(y ~ ., data = dat_lasso, family = "binomial")
end = Sys.time()
time_glm = difftime(end, start, units = "secs")
start = Sys.time()
self_logit = logistic(X = dat_lasso_matrix, Y = dat_lasso$y)
end = Sys.time()
time_logit = difftime(end, start, units = "secs")
logit_time = tibble("glm" = as.numeric(time_glm), "self written" = as.numeric(time_logit))
print(logit_time)
logit_time %>%
  gather(key = "method", value = "time") %>%
  ggplot(aes(x = method, y = time)) +
  geom_col(fill = "deepskyblue3", position = position_dodge()) +
  geom_point() +
  theme(text = element_text(size = 12))
  
glm_result = tibble(names(self_logit), glm_logit$coefficients, self_logit)
colnames(glm_result) = c("varname", "glm", "self written")
glm_result %>%
  gather(glm, `self written`, key = "method", value = "estimates") %>%
  ggplot() +
  geom_col(aes(x = varname, y = log(abs(estimates)), fill = method), position = position_dodge()) +
  labs(x = "Covariates", y = "Log of the absolute value of the estimates") +
  theme(axis.title = element_text(size = 12)) + 
  scale_x_discrete(labels = NULL)

##Using the logistic regression to determine optimal features for predicting term deposits
##Uses code from package
set.seed(2020)
testfolds = createFolds(dat_lasso$y, k=5, list=TRUE, returnTrain = FALSE)
logistic_pred = tibble()
for (i in 1:5){
  testset = dat_lasso[testfolds[[i]],]
  trainset = dat_lasso[-testfolds[[i]],]
  logit = glm(y ~ ., data = trainset, family="binomial")
  test_pred = add_predictions(testset, logit, type = "response")
  logistic_pred = rbind(logistic_pred, test_pred)
}
logistic_pred = logistic_pred %>%
  mutate(pred_cat = ifelse(pred>=0.5, 1, 0))
pROC::roc(logistic_pred$y, logistic_pred$pred)$auc
confusionMatrix(as.factor(logistic_pred$pred_cat), as.factor(logistic_pred$y))
