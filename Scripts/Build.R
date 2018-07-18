set.seed(13)


library(readr)
library(ISLR)
library(ROCR)
library(Epi)
library(vcdExtra)
library(MASS)
library(mlbench)
library(ggplot2)
library(dplyr)

install.packages("mlbench")
application_train <- read_csv("~/R/Kaggle/Raw data/application_train.csv")

rm(application_train)
View(application_train)


complete <- read_csv("~/R/Kaggle/Raw data/application_train.csv")
submit <- read_csv("~/R/Kaggle/Raw data/application_test.csv")

train.index <- sample(c("train", "test"), nrow(complete), replace = TRUE, prob = c(4, 1))

complete2 <- complete


complete2<- split(complete, train.index)





summary(complete2)

glimpse(complete2$train)

fit <-glm(TARGET ~ AMT_CREDIT + AMT_ANNUITY + CNT_CHILDREN + AMT_INCOME_TOTAL + AMT_CREDIT + DAYS_BIRTH + DAYS_EMPLOYED , data = complete2$train, family = binomial)


summary(fit)



#################### Evaluate  model##################################
test.prediction <- predict(fit,complete2$test,type = "response")



mean(test.prediction == complete2$test$TARGET)

####predictions 
pred <- prediction(test.prediction,complete2$test$TARGET)

#####performance

perf <- performance(pred, measure = "auc")


print(paste("AUC: ",perf@y.values[[1]]))







########################################################################################
