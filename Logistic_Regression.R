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
library(glmnet)

# Functions

model.predict <- function(model, data, thre){
  
  predict <- predict(model, type='response', newdata=data)
  predict <- factor(ifelse(predict>=thre, 'Yes', 'No'))
  actual <- data$Churn
  t <- table(actual, predict)
  
  return(list(t, round(t[2, 2]/(t[2, 2]+t[1, 2]), 3), round(t[2, 2]/(t[2, 2]+t[2, 1]), 3), 
              round(2/((t[2, 2]+t[1, 2])/t[2, 2]+(t[2, 1]+t[2, 2])/t[2, 2]), 3)))
}

model.predictlasso <- function(model, data, lmd, thre){
  
  predict <- predict(model, type='response', s=lmd, newx=as.matrix(subset(data, select=-c(Churn))))
  predict <- factor(ifelse(predict>=thre, 'Yes', 'No'))
  actual <- data$Churn
  t <- table(actual, predict)
  
  return(list(t, round(t[2, 2]/(t[2, 2]+t[1, 2]), 3), round(t[2, 2]/(t[2, 2]+t[2, 1]), 3), 
              round(2/((t[2, 2]+t[1, 2])/t[2, 2]+(t[2, 1]+t[2, 2])/t[2, 2]), 3)))
}

model.roc <- function(model, data){
  
  roc <- roc(response=data$Churn, predictor=predict(model, data))
  return(roc)
}

data.plot <- function(data, name, xlabel){
  theme <- theme_bw() + theme(axis.text.x=element_text(angle = 0, hjust = 0.5, vjust = 0.5), legend.position="none")
  count <- ggplot(data, aes(x=name, fill=Churn)) + geom_bar() + theme + labs(x=xlabel, y='count')
  ratio <- ggplot(data, aes(x=name, fill=Churn)) + geom_bar(position='fill') + theme + labs(x=xlabel, y='ratio')
  return(plot_grid(count, ratio, nrow=2, ncol=1))
}

split.train <- function(data, ratio=0.7, seed=246){
  set.seed(seed)
  indices = sample.split(data$Churn, SplitRatio=0.7)
  train = data[indices, ]
  rownames(train) <- seq(length=nrow(train))
  return(train)
}

split.valid <- function(data, ratio=0.7, seed=246){
  set.seed(seed)
  indices = sample.split(data$Churn, SplitRatio=0.7)
  valid = data[!(indices), ]
  rownames(valid) <- seq(length=nrow(valid))
  return(valid)
}

# Data Preprocessing

## read csv, delete id and nan

customer <- read.csv(file='Telco-Customer-Churn.csv', header=T, sep=',')
dim(customer)[1]

customer <- customer[complete.cases(customer), ]
customer <- subset(customer, select=-c(1))
dim(customer)[1]
customer$SeniorCitizen <- factor(ifelse(customer$SeniorCitizen %in% c(1), 'Yes', 'No'))

## Deal with special values

customer <- data.frame(lapply(customer, function(x) {
  gsub("No internet service", "No", x)}))

customer <- data.frame(lapply(customer, function(x) {
  gsub("No phone service", "No", x)}))

customer$tenure <- as.integer(as.character(customer$tenure))
customer$MonthlyCharges <- as.double(as.character(customer$MonthlyCharges))
customer$TotalCharges <- as.double(as.character(customer$TotalCharges))

## Split the data into categorical and numeric values

dummy <- subset(customer, select=-c(tenure, MonthlyCharges, TotalCharges, Churn))
dummy <- data.frame(sapply(dummy, function(x){data.frame(model.matrix(~x-1, data=dummy))[, -1]}))
numeric <- subset(customer, select=c(tenure, MonthlyCharges, TotalCharges, Churn))

seed <- 246

setnames(dummy, old=c("gender", "InternetService.xFiber.optic", "InternetService.xNo", 
                      "Contract.xOne.year", "Contract.xTwo.year", "PaymentMethod.xCredit.card..automatic.", 
                      "PaymentMethod.xElectronic.check", "PaymentMethod.xMailed.check"), 
         new=c("Gender", "InternetService.Fiber", "InternetService.No", "Contract.Oneyear", "Contract.Twoyear", 
               "PaymentMethod.Creditcard", "PaymentMethod.Electronic", "PaymentMethod.Mailed"))
setnames(numeric, old=c("tenure"), new=c("Tenure"))

data = cbind(dummy, numeric)

## Split Train and Test set

train = split.train(data)
valid = split.valid(data)

# Direct Logistic Regression

full <- glm(Churn~., data=train, family=binomial)
null <- glm(Churn~1, data=train, family=binomial)

## step regression

step_both_full <- step(full, direction='both', scope=list(upper=full, lower=null), trace=FALSE)

step_both_null <- step(null, direction='both', scope=list(upper=full, lower=null), trace=FALSE)

step_full <- round(vif(step_both_full), 3)
step_null <- round(vif(step_both_null), 3)
vif <- merge(data.frame(vif(step_both_full)), data.frame(vif(step_both_null)), by=0, all=TRUE)
setnames(vif, old=names(vif), new=c("Feature", "Step(Full)", "Step(Null)"))

options(xtable.floating = TRUE)
options(xtable.timestamp = "")

xtable(vif, caption='VIF Index for Direct Logistic Regression')

print(xtable(vif, caption='VIF Index for Direct Logistic Regression'))

stargazer(step_both_full, step_both_null, align=TRUE, single.row=TRUE, 
          column.labels=c("Step(Full)", "Step(Null)"), no.space=TRUE)

# Check for Multicollinearity

set.seed(seed)
indice = sample.split(data$Churn, SplitRatio=0.7)
charge = lm(train$MonthlyCharges~., data=dummy[indice, ])
chargelog = lm(log(train$MonthlyCharges)~., data=dummy[indice, ])
stargazer(charge, chargelog, align=TRUE, omit.stat=c("n"), single.row=TRUE, no.space=TRUE)
charge = lm(log(train$TotalCharges)~log(train$MonthlyCharges)+log(train$Tenure), data=train)
stargazer(charge, align=TRUE, omit.stat=c("n"), single.row=TRUE, report=c("vcstp*"), no.space=TRUE)

# Improved Logistic Regression

data = cbind(dummy, log(numeric$Tenure), subset(numeric, select=-c(MonthlyCharges)))
glimpse(data)

set.seed(seed)
indice = sample.split(data$Churn, SplitRatio=0.7)
train = data[indice, ]
rownames(train) <- seq(length=nrow(train))
valid = data[!(indice), ]
rownames(valid) <- seq(length=nrow(valid))

full <- glm(Churn~., data=train, family=binomial)
null <- glm(Churn~1, data=train, family=binomial)
step_full_both <- step(full, direction='both', scope=list(upper=full, lower=null), trace=FALSE)
step_null_both <- step(null, direction='both', scope=list(upper=full, lower=null), trace=FALSE)
model <- step_null_both
sapply(list(full, model), AIC)
sapply(list(full, model), logLik)
print(xtable(anova(full, model, test='Chisq'), 
             caption="ANOVA between Full Model and StepAIC"))
imp <- as.data.frame(varImp(model))
imp <- data.frame(Feature=rownames(imp), Overall=imp$Overall, VIF=vif(model))
imp <- as.data.frame(imp[order(imp$Overall, decreasing=T), ])
row.names(imp) <- seq(dim(imp)[1])

print(xtable(imp, caption="Feature Importance and VIF"))

# Lasso

result <- glmnet(x=as.matrix(subset(train, select=-c(Churn))), y=train$Churn, family='binomial', alpha=1)

cv.result <- cv.glmnet(x=as.matrix(subset(train, select=-c(Churn))), y=as.double(train$Churn), alpha=1)

lmd.min <- cv.result$lambda.min
lmd.1se <- cv.result$lambda.1se

png(filename="ROC-lasso.png", bg='transparent')
plot(cv.result)
dev.off()
xtable(data.frame(as.matrix(coef(result, s=lmd.1se))))
data.frame(as.matrix(coef(model)))

# Prediction and Performance

xtable(model.predict(model, train, 0.4))
xtable(model.predict(model, valid, 0.4))

model.predictlasso(result, train, lmd.1se, 0.39)
model.predictlasso(result, valid, lmd.1se, 0.39)