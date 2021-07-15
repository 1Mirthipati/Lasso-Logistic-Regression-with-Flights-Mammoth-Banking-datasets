library(caret)
library(skimr)
library(glmnet)
#install.packages("e1071")
library(e1071)
library(ROCR)

loan_data <- read.csv(file = "loan_default_dataset.csv", header=T)


summaryStats <- skim(loan_data)
summaryStats
mean(loan_data$Age) #31 is the avg age
mean(loan_data$Emp_duration) #49 is the ave length

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(loan_data$Age) #more borrowers are 32 y/o than any other age
getmode(loan_data$Emp_duration) #0 is the mode


mean(loan_data$Default)

table(loan_data$Default)

boxplot(loan_data$Credit_score~loan_data$Default)
#amount doesn't seem to be a strong indicator of whether or not a borrower will default
boxplot(loan_data$Amount~loan_data$Default)
#wider range in credit scores that default but lower median value
boxplot(loan_data$No_of_credit_acc~loan_data$Default)
#num of accts doesn't show a huge discrepancy in those who default and those that don't
boxplot(loan_data$Checking_amount~loan_data$Default)
#lower account balances in those who default
boxplot(loan_data$Age~loan_data$Default)
#younger borrowers are defaulting , range of age is lower in those who default
summaryStats
boxplot(loan_data$Term~loan_data$Default)
#loan term is typically longer in the cases where the customer default
boxplot(loan_data$Emp_duration~loan_data$Default)
#haven't been employed as long in cases where default happened


plot(loan_data$Age, loan_data$default, main="Scatterplot Age vs Default",
     xlab="Age of Borrower ", ylab="Default Status ", pch=19)
#younger people seem to default more
plot(loan_data$Amount, loan_data$default, main="Scatterplot Amount vs Default",
     xlab="Amount Borrowed ", ylab="Default Status ", pch=19)
#there's less spread in those who default, in the amount they borrow
plot(loan_data$Age, loan_data$Amount, main="Scatterplot Age vs Amount",
     xlab="Age of Borrower ", ylab="Amount Borrowed ", pch=19)

plot(loan_data$Amount, loan_data$Credit_score, main="Scatterplot Amount vs Credit Score",
     xlab="Loan Amount ", ylab="Credit score ", pch=19)

plot(loan_data$No_of_credit_acc, loan_data$default, main="Scatterplot Number of Account vs default status",
     xlab="num of accts", ylab="Default status ", pch=19)
#number of accounts doesn't seem to give any insights on whether a customer is likley to default or not
plot(loan_data$Checking_amount, loan_data$default, main="Scatterplot Account balance vs default status",
     xlab="checking acct balance", ylab="Default status ", pch=19)
#those who default typically have -500 to 500 in their account
plot(loan_data$Term, loan_data$default, main="Scatterplot Term length vs default status",
     xlab="Term length", ylab="Default status ", pch=19)
#longer terms seem to be linked to defaulted account
### Step 1: Partition the Data

###create dummy variables
default_predictors_dummy <- model.matrix(Default  ~ ., data = loan_data)
default_predictors_dummy<- data.frame(default_predictors_dummy[,-1]) #get rid of intercept
loan_data <- cbind(default=loan_data$Default, default_predictors_dummy)


#convert from Response from integer to factor
loan_data$Default<-as.factor(loan_data$Default)


set.seed(12)
index <- createDataPartition(loan_data$Default, p= .7, list = FALSE)

loan_train <- loan_data[index,]
loan_test <- loan_data[-index,]


## Step 2: Fit the model

loan_model <- train(default ~ .,
                      data = loan_train,
                      method = "glmnet",
                      trControl =trainControl(method = "cv", number = 5))
#not sure what the 5 represents and if we should change that..
#plot variable importance
plot(varImp(loan_model))

coef(loan_model$finalModel, loan_model$bestTune$lambda)

#First, get the predicted probabilities of the test data.
predprob_lasso<-predict(loan_model , loan_test, type="prob")

?varImp



pred_lasso <- prediction(predprob_lasso[,2], loan_test$Default)
perf_lasso <- performance(pred_lasso, "tpr", "fpr")
plot(perf_lasso, colorize=TRUE)

auc_lasso<-unlist(slot(performance(pred_lasso, "auc"), "y.values"))


auc_lasso



##more exploratory data
MBCD <- as_tibble(MBRDL) #Take Mammoth Bank Raw Data from imported file...CDW

summary(MBCD$Age) #Provide a summary of data cleaned and headers created...TCDW

head(MBCD)

View(MBCD$AGE)

GetMB_LD <- MBCD %>%
  filter(Default == 1) %>%
  select(Age, Education_loan) %>%
  arrange(Age, mean(Age))

View(GetMB_LD)

GetMB_ND <- MBCD %>%
  filter(Default == 0) %>%
  select(Age, Education_loan) %>%
  arrange(Age, mean(Age))

View(GetMB_ND)

#Additional coding to create visuals
# Install tidyverse
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("mlbench")
install.packages("klaR")
install.packages("PerformanceAnalytics")
install.packages("ggthemes")
install.packages("psych")
install.packages("caretEnsemble")
install.packages("doParallel")
install.packages("car")
install.packages("glmnet")
install.packages("dplyr")

library(readr)
library(mlbench)
library(caret)
library(klaR)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(ggthemes)
library(corrplot)
library(car)
library(psych)
library(caret)
library(caretEnsemble)
library(doParallel)
library(tidyr)
library(tidyverse)
#library(lubridate)

MBCD <- as_tibble(MBRDL) #Take Mammoth Bank Raw Data from imported file...TCDW 

summary(MBCD) #Provide a summary of data cleaned and headers created...TCDW

head(MBCD)

View(MBCD$Age)

GetMB_LD <- MBCD %>%
  filter(Default == 1) %>%
  select(Age, Education_loan) %>%
  arrange(Age, mean(Age))

View(GetMB_LD)

GetMB_ND <- MBCD %>%
  filter(Default == 0) %>%
  select(Age, Education_loan) %>%
  arrange(Age, mean(Age))

View(GetMB_ND)

#Change how you see fit...TCDW
MBD <- ggplot(GetMB_LD, mapping = aes(x=Age, y=Education_loan, col=Age, fill=Education_loan), data = GetMB_LD, pch=c(GetMB_LD$Age,GetMB_LD$Education_loan)) + 
  geom_histogram(stat = "identity", width=0.3, position="dodge") + 
  stat_smooth() 
MBD <- MBD + labs(x="Median Age", y="Education Loan", title="The Median Ages of Loan Holders In Default")
MBD <- MBD + theme(plot.title=element_text(lineheight=3, 
                                           face="bold", color="darkorange3", size=24))
MBD <- MBD + scale_y_continuous(breaks=seq(0, 100, 5),
                                limits=c(0, max(GetMB_LD$Education_loan,na.rm=T)))
MBD

#Change how you see fit...TCDW
MBND <- ggplot(GetMB_ND, mapping = aes(x=Age, y=Education_loan, col=Age, fill=Education_loan), data = GetMB_ND, pch=c(GetMB_ND$Age,GetMB_ND$Education_loan)) + 
  geom_histogram(stat = "identity", width=0.3, position="dodge") + 
  geom_vline(data=GetMB_ND, aes(xintercept = Age, color = Education_loan), linetype="dashed") +
  stat_smooth()

MBND <- MBND + labs(x="Median Age", y="Education Loan", title="The Median Ages of Loan Holders Not In Default")
MBND <- MBND + theme(plot.title=element_text(lineheight=3, 
                                             face="bold", color="darkorange3", size=24))
MBND <- MBND + scale_y_continuous(breaks=seq(0, 100, 5),
                                  limits=c(0, max(GetMB_ND$Education_loan,na.rm=T)))
MBND