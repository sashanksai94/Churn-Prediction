#loading libraries
library(ggplot2)
library(gridExtra)
library(corrplot)
library(C50)
library(e1071)
library(caret)
library(randomForest)
library(dplyr)
library(cowplot)

#setting working directory
setwd("D:/Data Science edwisor/Projects/Churn reduction")
getwd

#LOading the data
Train = read.csv("Train_data.csv")
Test=read.csv("Test_data.csv")

#understanding the data
head(Train)
str(Train)
summary(Train)

#checking target variable
table(Train$Churn)

#Exploring the data
#checking the frequency of international plan
ggplot(Train,aes(x=international.plan))+geom_bar()

#checking the frequency of voicemail plan
ggplot(Train,aes(x=voice.mail.plan))+geom_bar()

#scatterplot of total day calls and total day charge
ggplot(data = Train,aes(x=total.day.calls,y=total.day.charge,colour=Churn)) + geom_point()

#scatterplot of total day charge and international charge
ggplot(data = Train, aes(x=total.day.charge,y=total.intl.charge,colour=Churn)) + geom_point()

ggplot(Train, aes(x = international.plan, fill = Churn)) + geom_bar(position = "dodge")

ggplot(Train, aes(x = voice.mail.plan, fill = Churn)) + geom_bar(position = "dodge")



#Understanding distribution of variables
q1 <- ggplot(Train,aes(total.day.charge)) + geom_density()
q2 <- ggplot(Train,aes(total.night.charge)) + geom_density()
q3 <- ggplot(Train,aes(total.eve.charge)) + geom_density()
q4 <- ggplot(Train,aes(total.intl.charge)) + geom_density()

grid.arrange(q1,q2,q3,q4,ncol=2,nrow=2)

# Bivariante Analysis
q5<-ggplot(Train,aes(x=Churn,y=number.vmail.messages))+geom_boxplot(fill='green')+xlab('Churn')+ylab('Number of vmail')
q6<-ggplot(Train,aes(x=Churn,y=total.day.calls))+geom_boxplot(fill='green')+xlab('Churn')+ylab('total.day.calls')
q7<-ggplot(Train,aes(x=Churn,y=total.day.minutes))+geom_boxplot(fill='green')+xlab('Churn')+ylab('total.day.minutes')
q8<-ggplot(Train,aes(x=Churn,y=total.day.charge))+geom_boxplot(fill='green')+xlab('Churn')+ylab('total.day.charge')
plot_grid(q5,q6,q7,q8)


#checking missing values
missing_val = data.frame(apply(Train,2,function(x){sum(is.na(x))}))
missing_val$columns = rownames(missing_val)
names(missing_val)[1]= 'Missing count'
rownames(missing_val)=NULL
missing_val = missing_val[,c(2,1)]#there are no missing values
missing_val

#selecting only numeric
numeric_index = sapply(Train,is.numeric) 
numeric_data = Train[,numeric_index]
cnames = colnames(numeric_data)

#checking outliers for train data
library(ggplot2)
boxplot(numeric_data,las=2)

#handling outliers for train data
for(i in cnames){
  val = Train[,i][Train[,i] %in% boxplot.stats(Train[,i])$out]
   print(length(val))
  Train[,i][Train[,i] %in% val] = mean(Train[,i])
}

boxplot(numeric_data,las=2)

numeric_index1 = sapply(Test,is.numeric) 
numeric_data1 = Test[,numeric_index]
cnamest = colnames(numeric_data)

#checking outliers for test data
boxplot(numeric_data1,las=2)

#handling outliers for test
for(i in cnamest){
  val = Test[,i][Test[,i] %in% boxplot.stats(Test[,i])$out]
  print(length(val))
  Test[,i][Test[,i] %in% val] = mean(Test[,i])
}

boxplot(numeric_data1,las=2)


#Checking the correlation between the variables
corrplot(cor(numeric_data),is.corr = TRUE)
#from above we can find 4 highly correlated varialble. its obvious that charges depends on minutes of call
# total day minutes,total eve minutes,total night minutes,total intl minutes
# we can remove these from data

#chi-square test
#checking categorical values significant or not
factor_index=sapply(Train,is.factor)
factor_data=Train[,factor_index]
cnames1=colnames(factor_data)

for (i in colnames(factor_data)){
  print(i)
  print(chisq.test(table(factor_data$Churn,factor_data[,i])))
  
}

#we could see that probablity is less than 0.05,so the variables are dependent

#feature engineering
#creating features from existing features
Train$day.charge.per.min<-Train$total.day.charge/Train$total.day.minutes
Train$eve.charge.per.minute<-Train$total.eve.charge/Train$total.eve.minutes
Train$night.charge.per.minute<-Train$total.night.charge/Train$total.night.minutes
Train$intl.charge.per.minute<-Train$total.intl.charge/Train$total.intl.minutes

Test$day.charge.per.min<-Test$total.day.charge/Test$total.day.minutes
Test$eve.charge.per.minute<-Test$total.eve.charge/Test$total.eve.minutes
Test$night.charge.per.minute<-Test$total.night.charge/Test$total.night.minutes
Test$intl.charge.per.minute<-Test$total.intl.charge/Test$total.intl.minutes

#feature selection
#removing correlated continous variables and not useful variables
Train = subset(Train,select =-c(total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes,state,phone.number,area.code))
colnames(Train)
Test = subset(Test,select =-c(total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes,state,phone.number,area.code))
colnames(Test)

#checking normality
qqnorm(Train$total.day.charge)
qqnorm(Train$total.night.charge)
#upon checking the data follows normal distribution.

#standardization
#for train data
cont_names=sapply(Train,is.numeric)
cont_data =Train[,cont_names]
cnames3=colnames(cont_data)

for (i in cnames3){
  Train[,i]=Train[,i]-mean(Train[,i])/sd(Train[,i])
}

#for test data
cont_names1=sapply(Test,is.numeric)
cont_data1 =Test[,cont_names1]
cnames4=colnames(cont_data1)

for (i in cnames4){
  Test[,i]=Test[,i]-mean(Test[,i])/sd(Test[,i])
}

View(Train)

#dummy encoding of categorical variable

Train$international.plan<- as.numeric(Train$international.plan)-1
Train$voice.mail.plan <- as.numeric(Train$voice.mail.plan)-1
Train$Churn<-as.numeric(Train$Churn)-1

Test$international.plan<- as.numeric(Test$international.plan)-1
Test$voice.mail.plan <- as.numeric(Test$voice.mail.plan)-1
Test$Churn<-as.numeric(Test$Churn)-1

Train$international.plan<- as.factor(Train$international.plan)
Train$voice.mail.plan <- as.factor(Train$voice.mail.plan)
Train$Churn<-as.factor(Train$Churn)

Test$international.plan<- as.factor(Test$international.plan)
Test$voice.mail.plan <- as.factor(Test$voice.mail.plan)
Test$Churn<-as.factor(Test$Churn)



#modelbuilding
#decision tree
C50_model = C5.0(Churn ~., Train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases
C50_Predictions = predict(C50_model, Test[,-14], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(Test$Churn, C50_Predictions)
confusionMatrix(ConfMatrix_C50)


#Accuracy: 94.66%
#FNR: 37.5%

#Random forest
RF_model = randomForest(Churn ~ ., Train, importance = TRUE, ntree = 500)

summary(RF_model)

#Predicting test cases
RF_Predictions = predict(RF_model, Test[,-14])

#Evaluating model
ConfMatrix_RF = table(Test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF)

#Accuracy = 93.5
#FNR =43.30

#logistic regression
logit_model = glm(Churn ~ .,data=Train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = Test, type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)

##Evaluate the performance of classification model
ConfMatrix_Lg = table(Test$Churn, logit_Predictions)
ConfMatrix_Lg







