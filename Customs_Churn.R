library(car)
library(grid)
library(MASS)
library(dplyr)
library(rlist)
library(caTools)
library(cowplot)
library(ggplot2)
library(ggcorrplot)

# read csv, delete id and nan
customer <- read.csv('/Users/songyurun/Desktop/大三下/数据分析与统计软件/数统project/Telco-Customer-Churn1.csv', header=T, sep=',')
customer <- customer[complete.cases(customer), ]
customer <- subset(customer, select=-c(1))
customer$SeniorCitizen <- factor(ifelse(customer$SeniorCitizen %in% c(1), 'Yes', 'No'))

pplot <- function(name, xlabel){
  theme <- theme_bw() + theme(axis.text.x=element_text(angle = 0, hjust = 0.5, vjust = 0.5), legend.position="none")
  count <- ggplot(customer, aes(x=name, fill=Churn)) + geom_bar() + theme + labs(x=xlabel, y='count')
  ratio <- ggplot(customer, aes(x=name, fill=Churn)) + geom_bar(position='fill') + theme + labs(x=xlabel, y='ratio')
  return(plot_grid(count, ratio, nrow=2, ncol=1))
}

name_list = c('gender', 'SeniorCitizen', 'Partner', 'Dependents', 
              'PhoneService', 'MultipleLines',
              'InternetService', 'OnlineSecurity', 'OnlineBackup', 'DeviceProtection', 
              'TechSupport', 'StreamingTV', 'StreamingMovies', 
              'Contract', 'PaperlessBilling', 'PaymentMethod')

gender <- pplot(customer$gender, 'Gender')
senior <- pplot(customer$SeniorCitizen, 'Senior')
partner <- pplot(customer$Partner, 'Partner')
depend <- pplot(customer$Dependents, 'Dependents')

phone <- pplot(customer$PhoneService, 'Phone Service')
muline <- pplot(customer$MultipleLines, 'Multiple Lines')

internet <- pplot(customer$InternetService, 'Internet Service')
security <- pplot(customer$OnlineSecurity, 'Online Security')

backup <- pplot(customer$OnlineBackup, 'OnlineBackup')
protection <- pplot(customer$DeviceProtection, 'DeviceProtection')

support <- pplot(customer$TechSupport, 'TechSupport')
tv <- pplot(customer$StreamingTV, 'StreamingTV')

movies <- pplot(customer$StreamingMovies, 'StreamingMovies')
contract <- pplot(customer$Contract, 'Contract')

billing <- pplot(customer$PaperlessBilling, 'PaperlessBilling')
paymethod <- pplot(customer$PaymentMethod, 'PaymentMethod')

plot_grid(gender, senior, partner, depend, nrow=1)
plot_grid(phone, muline, nrow=1)
plot_grid(internet, security, nrow=1)
plot_grid(backup, protection, nrow=1)
plot_grid(support, tv, nrow=1)
plot_grid(movies, contract, nrow=1)
plot_grid(billing)
plot_grid(billing, paymethod, nrow=1)

# Deal with special values
customer <- data.frame(lapply(customer, function(x) {
  gsub("No internet service", "No", x)}))

customer <- data.frame(lapply(customer, function(x) {
  gsub("No phone service", "No", x)}))

# Analyzing the three continuous variables
customer$tenure <- as.integer(as.character(customer$tenure))
customer$MonthlyCharges <- as.double(as.character(customer$MonthlyCharges))
customer$TotalCharges <- as.double(as.character(customer$TotalCharges))

options(repr.plot.width =6, repr.plot.height = 2)
ggplot(customer, aes(y= tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

options(repr.plot.width =6, repr.plot.height = 2)
ggplot(customer, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

options(repr.plot.width =6, repr.plot.height = 2)
ggplot(customer, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

# check for outliers in the continuous variables
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(customer$tenure)$out
boxplot(customer$MonthlyCharges)$out
boxplot(customer$TotalCharges)$out
#It seems none of the values are beyond the whiskers here.

# Checking the correlation between continuous variables
options(repr.plot.width =6, repr.plot.height = 4)
customer_cor <- round(cor(customer[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)
ggcorrplot(customer_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

#plot the total ratio of chum
options(repr.plot.width = 6, repr.plot.height = 4)
customer %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percent")