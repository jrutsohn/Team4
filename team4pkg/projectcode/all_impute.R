# missForest and MICE Imputation
#
# This section provides all the syntax used for the imputation performed on the missing variables in the Term Deposit Feature Selection project.  This section requires bank-additional-full.csv data to run.
# 
#
library(data.table)
library(missForest)
data <- fread("bank-additional-full.csv")

##Converting variables of interest to factors for the purpose of imputation
##Uses function from package to convert to factors
v <- c("job","marital","education","default","housing","loan","contact","month","day_of_week","poutcome","y")

to.factor(data,v)

##Converting "unknown" values to NA so that R recognizes as missing
dat <- data
dat <- dat %>% replace_with_na(list(job="unknown",marital="unknown",education="unknown",default="unknown",housing="unknown",loan="unknown"))

# Simulation

#Before iputing NA's, we need to check which method imputes missings "better". There are two candidates, missForest and MICE. To compare them, I will generate fake missing and apply the candidates. The measurement will be proportion of falsely classified entries (PFC) over the categorical missing values.
#This is the simulation step.
#1. Extract rows without any missing. This data is called `complete` 
#2. For each colums, substitute elemets with NA (artificially produce missing values). Since we want the generated simluation data to be similar to the whole data as much as possible, the proportion of NA will be that of full data set. For example, job variables has missing at a rate of 0.8%. Therefore, seed NA's in `complete` at a rate of 0.8%.
#3. Repeat step2 five times. Then five simulation dataset is generated. Let's call them `sim1`, ..., `sim5`
#4. Apply missForest and MICE to `sim1`, ..., `sim5` and obtain PFC's. 
#5. Check which method has smaller PFC's.

##Complete data
complete <- dat[complete.cases(dat),]
n <- nrow(dat)

##Artificially producing the missing values
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

##PFC for the 5 missForests
result1_mf
result2_mf
result3_mf
result4_mf
result5_mf

#MICE Imputation
library(mice)
mice1 <- mice(sim1, m=1, maxit = 50, method = NULL, seed = 1)
imputedData1_mice <- complete(mice1,1)
mice2 <- mice(sim2, m=1, maxit = 50, method = NULL, seed = 1)
imputedData2_mice <- complete(mice2,1)
mice3 <- mice(sim3, m=1, maxit = 50, method = NULL, seed = 1)
imputedData3_mice <- complete(mice3,1)
mice4 <- mice(sim4, m=1, maxit = 50, method = NULL, seed = 1)
imputedData4_mice <- complete(mice4,1)
mice5 <- mice(sim5, m=1, maxit = 50, method = NULL, seed = 1)
imputedData5_mice <- complete(mice5,1)
sum(imputedData1_mice != complete)/sum(is.na(sim1)) 
sum(imputedData2_mice != complete)/sum(is.na(sim2)) 
sum(imputedData3_mice != complete)/sum(is.na(sim3)) 
sum(imputedData4_mice != complete)/sum(is.na(sim4)) 
sum(imputedData5_mice != complete)/sum(is.na(sim5)) 

#Using missForest to impute on the full data set and produce 'imputedData.csv'
result_mf <- missForest(data.test, verbose = TRUE)
imputedData <- result_mf$ximp
#write_csv(imputedData, "imputedData.csv")
