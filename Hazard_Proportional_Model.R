library(survival)
library(carData)
library(car)
library(grid)
library(MASS)
library(dplyr)
library(rlist)
library(caTools)
library(ggplot2)
library(cowplot)
library(magrittr)
library(ggpubr)
library(survminer)

##' read data
setwd("~/Desktop/cuschur")
data<-read.csv("numeric-churn.csv",header=TRUE,sep=',')
km<-survfit(Surv(tenure,Churn)~1,data=data)

##' plot harzard
par(mfrow=c(2,2))
plot(km, main="Cumulative Hazard", xlab="Survival time in months", lwd=2, fun="cumhaz")
plot(km, main="Log Cumulative Hazard", xlab="Survival time in months", ylim=c(-2,-0.5),lwd=2, fun="cloglog")
ggsurvplot(km, data = data, conf.int = TRUE, 
           palette = c("#FF9E29", "#86AA00"),
           risk.table = TRUE, risk.table.col = "strata",
           fun = "cumhaz")

##' step regression
full <- coxph(Surv(tenure,Churn)~.,data=data)
null <- coxph(Surv(tenure,Churn)~1,data=data)
summary(full)
summary(null)
step_back <- step(full, direction='backward', trace=FALSE)
step_for <- step(null, direction='forward', scope=list(upper=full, lower=null), trace=FALSE)
summary(step_back)
summary(step_for)

optimal <- coxph(Surv(tenure,Churn)~TotalCharges	+InternetServicexFiberoptic	+InternetServicexNo	+ContractxTwoyear	+ContractxOneyear	+StreamingTV	+PhoneService	+StreamingMovies	+Partner	+PaymentMethodxMailedcheck	+PaymentMethodxElectroniccheck	+PaperlessBilling	+DeviceProtection,data=data)
summary(optimal)

# plot Hazard rate
ggforest(optimal,data=data)

##' Harzard assumption
czph=cox.zph(full)
czph=cox.zph(optimal)
plot(czph)

par(mfrow=c(2,2))
dev_res=residuals(full,type="deviance")
index=seq(1:7032)
plot(dev_res~index,main="Deviance Residuals for Full Model",xlab="Index",ylab="Residuals")
qqnorm(dev_res,main="Normal Q-Q Plot for Full Model")

dev_res=residuals(optimal,type="deviance")
index=seq(1:7032)
plot(dev_res~index,main="Deviance Residuals for StepAIC Model",xlab="Index",ylab="Residuals")
qqnorm(dev_res,main="Normal Q-Q Plot for StepAIC Model")

##' comparison
AIC(optimal)
BIC(optimal)
anova(full,optimal)
logLik(full)

# Drawing curves
ggsurvplot(fit, color = "#2E9FDF")

##' residual plot
res <- data.frame(Residual=dev_res)
p <- ggplot(res, aes(sample = Residual))
p + stat_qq() + stat_qq_line()
Index <- 1:7032
require(ggplot2)
p <- ggplot(res, aes(Index, Residual))
p + geom_point() +
  geom_point(data = res, aes(y = res), colour = '#2E9FDF', size = 3)
