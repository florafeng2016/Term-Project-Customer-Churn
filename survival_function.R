library(car)
library(grid)
library(MASS)
library(pROC)
library(plyr)
library(dplyr)
library(rlist)
library(caret)
library(xtable)
library(caTools)
library(cowplot)
library(ggplot2)
library(stargazer)
library(data.table)
library(survival)

# read csv, delete id and nan
customer <- read.csv('/Users/songyurun/Desktop/大三下/数据分析与统计软件/数统project/Telco-Customer-Churn1.csv', header=T, sep=',')
dim(customer)[1]

customer <- customer[complete.cases(customer), ]
customer <- subset(customer, select=-c(1))
dim(customer)[1]
customer$SeniorCitizen <- factor(ifelse(customer$SeniorCitizen %in% c(1), 'Yes', 'No'))

# Deal with special values
customer <- data.frame(lapply(customer, function(x) {
  gsub("No internet service", "No", x)}))

customer <- data.frame(lapply(customer, function(x) {
  gsub("No phone service", "No", x)}))

customer$tenure <- as.integer(as.character(customer$tenure))
customer$MonthlyCharges <- as.double(as.character(customer$MonthlyCharges))
customer$TotalCharges <- as.double(as.character(customer$TotalCharges))

# Split the data into categorical and numeric values
dummy <- subset(customer, select=-c(tenure, MonthlyCharges, TotalCharges, Churn))
dummy <- data.frame(sapply(dummy, function(x){data.frame(model.matrix(~x-1, data=dummy))[, -1]}))
numeric <- subset(customer, select=c(tenure, MonthlyCharges, TotalCharges, Churn))

seed <- 246
# rename the data
setnames(dummy, old=c("gender", "InternetService.xFiber.optic", "InternetService.xNo", 
                      "Contract.xOne.year", "Contract.xTwo.year", "PaymentMethod.xCredit", 
                      "PaymentMethod.xE.check", "PaymentMethod.xM.check"), 
         new=c("Gender", "InternetService.Fiber", "InternetService.No", "Contract.Oneyear", "Contract.Twoyear", 
               "PaymentMethod.Creditcard", "PaymentMethod.Electronic", "PaymentMethod.Mailed"))
setnames(numeric, old=c("tenure"), new=c("Tenure"))


# survival function
customer$survival = Surv(customer$tenure, customer$Churn == 'No')
fit_customer = survfit(survival ~ 1, data = customer)
plot(fit_customer, lty = 1, mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
title(main = 'Survival Curve')

# survival functon via explanary variables


# not significant
fit_gender = survfit(survival ~ gender, data = customer)
plot(fit_gender, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(5, 0.45, c('Female', 'Male'), fill = c('red', 'black'), bty = 'n', ncol = 2)
title(main = "Survival Curves by Gender")
survdiff(formula = survival ~ gender, data = customer)

#significant
fit_seniorcitizen = survfit(survival ~ SeniorCitizen, data = customer)
plot(fit_seniorcitizen, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.1,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(8, 0.45, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 2)
title(main = "Survival Curves by SeniorCitizen")
survdiff(formula = survival ~ SeniorCitizen, data = customer)

#significant
fit_partner = survfit(survival ~ Partner, data = customer)
plot(fit_partner, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.1,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(8, 0.45, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 2)
title(main = "Survival Curves by Partners")
survdiff(formula = survival ~ Partner, data = customer)

#significant
fit_dependents = survfit(survival ~ Dependents, data = customer)
plot(fit_dependents, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.1,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(8, 0.45, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 2)
title(main = "Survival Curves by Dependents")
survdiff(formula = survival ~ Dependents, data = customer)

# not significant
fit_phoneservice = survfit(survival ~ PhoneService, data = customer)
plot(fit_phoneservice, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.1,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(6, 0.48, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 2)
title(main = "Survival Curves by Phone Service")
survdiff(formula = survival ~ PhoneService, data = customer)

# significant
fit_multiplelines = survfit(survival ~ MultipleLines, data = customer)
plot(fit_multiplelines, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(43, 1.0, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 1)
title(main = "Survival Curves by MultipleLines")
survdiff(formula = survival ~ MultipleLines, data = customer)

# significant
fit_internetservice = survfit(survival ~ InternetService, data = customer)
plot(fit_internetservice, col = c('red', 'blue', 'black'), mark.time = FALSE, ylim=c(0.1,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(33, 0.99, c('DSL', 'Fiber optic', 'No Internet'), fill = c('red', 'blue', 'black'), bty = 'n', ncol = 2)
title(main = "Survival Curves by Internet Service")
survdiff(formula = survival ~ InternetService, data = customer)

# significant
fit_onlinesecurity = survfit(survival ~ OnlineSecurity, data = customer)
plot(fit_onlinesecurity, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(40, 0.99, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 1)
title(main = "Survival Curves by OnlineSecurity")
survdiff(formula = survival ~ OnlineSecurity, data = customer)

# significant
fit_onlinebackup = survfit(survival ~ OnlineBackup, data = customer)
plot(fit_onlinebackup, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(50, 0.99, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 1)
title(main = "Survival Curves by OnlineBackup")
survdiff(formula = survival ~ OnlineBackup, data = customer)

# significant
fit_deviceprotection = survfit(survival ~ DeviceProtection, data = customer)
plot(fit_deviceprotection, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(5, 0.49, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 1)
title(main = "Survival Curves by DeviceProtection")
survdiff(formula = survival ~ DeviceProtection, data = customer)

# significant
fit_techsupport = survfit(survival ~ TechSupport, data = customer)
plot(fit_techsupport, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(5, 0.49, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 1)
title(main = "Survival Curves by TechSupport")
survdiff(formula = survival ~ TechSupport, data = customer)

# significant
fit_streamingtv = survfit(survival ~ StreamingTV, data = customer)
plot(fit_streamingtv, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(5, 0.49, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 1)
title(main = "Survival Curves by StreamingTV")
survdiff(formula = survival ~ StreamingTV, data = customer)

# significant
fit_streamingmovies = survfit(survival ~ StreamingMovies, data = customer)
plot(fit_streamingmovies, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(5, 0.49, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 1)
title(main = "Survival Curves by StreamingMovies")
survdiff(formula = survival ~ StreamingMovies, data = customer)

# significant
fit_contract = survfit(survival ~ Contract, data = customer)
plot(fit_contract, col = c('red', 'blue', 'black'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(5, 0.49, c('Month', 'One year', 'Two year'), fill = c('red', 'blue', 'black'), bty = 'n', ncol = 1)
title(main = "Survival Curves by Contract")
survdiff(formula = survival ~ Contract, data = customer)

# significant
fit_paperlessbilling = survfit(survival ~ PaperlessBilling, data = customer)
plot(fit_paperlessbilling, col = c('red', 'black'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(5, 0.49, c('No', 'Yes'), fill = c('red', 'black'), bty = 'n', ncol = 1)
title(main = "Survival Curves by PaperlessBilling")
survdiff(formula = survival ~PaperlessBilling, data = customer)

# significant
fit_paymentmethod = survfit(survival ~ PaymentMethod, data = customer)
plot(fit_paymentmethod, col = c('red', 'blue', 'black', 'green'), mark.time = FALSE, ylim=c(0.01,1), xlab = 'Months since Subscribing', ylab = 'Percent Surviving')
legend(0, 0.34, c('Bank', 'Credit', 'Echeck', 'Mcheck'), fill = c('red', 'blue', 'black', 'green'), bty = 'n', ncol = 2)
title(main = "Survival Curves by PaymentMethod")
survdiff(formula = survival ~PaymentMethod, data = customer)









