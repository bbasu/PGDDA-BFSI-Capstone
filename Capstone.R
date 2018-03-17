#############################################################################################
# Environment Clean up
#############################################################################################

rm(list = ls())

################################################################################################
#Import nessesary packages
################################################################################################
library(MASS)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(e1071)
library(randomForest)
library(DMwR)

setwd("D:\\F\\IIITB capstone Project\\Source files")

################################################################################################
#Data loading
################################################################################################
# Both Data files are CSV files
demographics = read.csv("Demographic data.csv",  na.strings = c("NA",""))
CreditBureau = read.csv("Credit Bureau data.csv",na.strings = c("NA",""))

################################Required functions####################################################
# Distribution of categorical/continuousl variables with corresponding frequencies

# Function for distribution of categorical variables 
univariate_categorical <- function(dataset,var,var_name){
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1)
    ) 
}

# Function for distribution of continuous variables
univariate_continuous <- function(dataset,var,var_name){
  dataset %>% ggplot(aes(x = (var))) +
    geom_histogram(breaks=seq(min(var), max(var), by=1),col="red", aes(fill=..count..)) +
    scale_fill_gradient("Count", low="green", high="red")+
    labs(title = var_name, y = "Count", x = var_name)
  
}

perform_fn <- function(cutoff){
  test_actual<- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

perform_fn1 <- function(cutoff) 
{
  test$Performance.Tag<- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
  test$predicted.response<- factor(ifelse(test$predicted.prob[,1] >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(test$predicted.response, test$Performance.Tag, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

################################################################################################
#Initail Validation
################################################################################################
dim(demographics)
### 71295 rows X 12 columns

dim(CreditBureau)
### 71295 rows X 19 columns

#demographics[unique(demographics$Application.ID) %in% unique(CreditBureau$Application.ID),]

### number of rows in each dataset is matching.

summary(demographics)
### data has blank values in Gender , Marital status,Education,rofession, type_of_residence
### data has NA values in no_of_dependents and performance tags
### min age which is -3 indicats there are some bad entries in data
### min Income which is  -0.5 indicats data issue.
### max number of dependents is 5 and minimun is 1

str(demographics)
### number of dependents is int which can be converted as factor variable
### perfomance tag should be considered as factor as it is target variable
head(demographics)

###################################check NA values##############################################
###missing values count in each feature of Demographics
sapply(demographics, function(x) sum(is.na(x)))

### missing values percentage in each feature
missing_values <- gather(demographics %>%  summarise_all(funs(sum(is.na(.))/n())),key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  labs(title = "Missing values", x = "varaible name", y = "missing percentage")+
  coord_flip()

### almost 2% (1425/71295) of the records has NA values in performance Tag column.target column should not have NAs.These records are considered as rejected applications
### No.of.dependents column has 3/71295 na values.these can be removed as these are very less in number


###missing values count in each feature of CreditBureau data set
sapply(CreditBureau, function(x) sum(is.na(x)))

### missing values percentage in each feature
missing_values <- gather(CreditBureau %>%  summarise_all(funs(sum(is.na(.))/n())),key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  labs(title = "Missing values", x = "varaible name", y = "missing percentage")+
  coord_flip()


###################################check Blank values##############################################
### blank values count in each feature
sapply(demographics, function(x) sum( trimws(x) == "",na.rm = TRUE))

### Education has hightest blank values 119, profession has 14 and type of residence has 8 , marital status -6 and gender-2 

sapply(CreditBureau, function(x) sum(trimws(x) == "",na.rm = TRUE))
###################################check duplicate values############################################

sum(duplicated(demographics))
sum(duplicated(CreditBureau))
### There are no duplicated records in two dataframe


sum(duplicated(demographics$Application.ID))

sum(duplicated(CreditBureau$Application.ID))

demographics$Application.ID[duplicated(demographics$Application.ID)]
CreditBureau$Application.ID[duplicated(CreditBureau$Application.ID)]

dup <- demographics$Application.ID[duplicated(demographics$Application.ID)]
demographics[demographics$Application.ID %in% dup,]

### 3 application IDs have duplicates which are  [765011468 ,653287861 ,671989187] . But the application demographics are different.
### these 3 application IDs should be removed. 

Master<-merge(demographics,CreditBureau )

# 653287861 will be handled as there was difference performance tag. lets change the application ID

nrow(Master[Master$Application.ID == 653287861 & Master$Performance.Tag == 1,])
Master$Application.ID[Master$Application.ID == 653287861 & Master$Performance.Tag == 1]<- max(Master$Application.ID)+1

Master<-Master[!Master$Application.ID %in% c(765011468,671989187),]


d<-demographics[complete.cases(demographics),]
Master<-Master[complete.cases(Master),]


#####################################################################################################
###Univariate Analysis
#####################################################################################################

###average performace_tag
mean(d$Performance.Tag)
mean(Master$Performance.Tag)

d$Performance.Tag <-as.factor(d$Performance.Tag)
Master$Performance.Tag <-as.factor(Master$Performance.Tag)
### only 4.2 percentage of the records are 1

### categorical Distribution
univariate_categorical(d,d$Gender,"Gender Distribution")
univariate_categorical(d,d$Marital.Status..at.the.time.of.application.,"Marital status Distribution")
univariate_categorical(d,d$No.of.dependents,"dependents Distribution")
univariate_categorical(d,d$Education,"Education Distribution")
univariate_categorical(d,d$Profession,"Profession Distribution")
univariate_categorical(d,d$Type.of.residence,"residence type Distribution")
univariate_categorical(d,d$Performance.Tag,"performance Distribution")


### 76% is male population
### 85% are married
### number of dependents distribution is same and 22% has 3 dependents
### 33.6% masters and 34.9% professional. Blanks 2% can be combined with others which is 2%
### 56.8 % are salaried. Blanks are almost 0 percentage. blanks can be handled based on the performance tag/changed to sal
### 74.8% are rented.blanks can be changed to rented

###continuous values distribution

univariate_continuous(d,d$Age,"Age Distribution")
summary(factor(d$Age))
univariate_continuous(d,d$Income,"Income Distribution")
summary(factor(d$Income))

univariate_continuous(d,d$No.of.months.in.current.residence,"Current residence Distribution")
summary(factor(d$No.of.months.in.current.residence))
univariate_continuous(d,d$No.of.months.in.current.company,"Current company Distribution")
summary(factor(d$No.of.months.in.current.company))

### high distribution from 24 to 65 years.one person has age -3 and 18 members has age as 0. This data issue should be handled.
### there very high spike at income 4.5 and 107 persons has less than or equal to 0 which is a data issue
### huge spike for number of months in current residence at 6 months 
### big spike for 3 months in current company.

#####################################################################################
# creation of dummy variables and Feature standardisation
#####################################################################################

mean(Master$Age)-1.5*IQR(Master$Age)
summary(factor(Master$Age))
boxplot(Master$Age)
quantile(Master$Age,seq(0,1,0.1))

summary(factor(d$Income))
q <-quantile(Master$Income,seq(0,1,0.1))

Master$Age[Master$Age < 18 ] <- 18
Master$Income[Master$Income <= 0 ] <- 0

Master_factor <- Master[,c(4,5,6,8,9,10)]
Master_factor<- data.frame(sapply(Master_factor, function(x) factor(x)))

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(Master_factor,function(x) data.frame(model.matrix(~x-1,data =Master_factor))[,-1]))

Master_final<- cbind(Master[,c(2,3,7,11:29)],dummies)

#####################################################################################
# creating training and test data  
#####################################################################################
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(Master_final), 0.7*nrow(Master_final))
train = Master_final[trainindices,]
test = Master_final[-trainindices,]

#####################################################################################
#Model Building
#####################################################################################

# Logistic Regression: 

#Initial model - Build model 1 containing all variables
model_1 <- glm(Performance.Tag~.,data=train,family = "binomial")
summary(model_1)

# used STEPAIC to find the best model
model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)

# Removing multicollinearity through VIF check
sort(vif(model_2))

#captured the STEPAIC output and built model2

#Excluding Outstanding.Balance due to low significance and high VIF
model_3<-glm(formula = Performance.Tag ~ No.of.times.90.DPD.or.worse.in.last.6.months + 
               No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
               Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
               No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
               Presence.of.open.home.loan +  Total.No.of.Trades + 
               Marital.Status..at.the.time.of.application. + No.of.dependents.x2 + 
               No.of.dependents.x3 + No.of.dependents.x4 + No.of.dependents.x5, 
             family = "binomial", data = train)

summary(model_3)
sort(vif(model_3))

#Excluding No.of.PL.trades.opened.in.last.6.months  due to low significance and high VIF
model_4<-glm(formula = Performance.Tag ~ No.of.times.90.DPD.or.worse.in.last.6.months + 
               No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
               Avgas.CC.Utilization.in.last.12.months +  
               No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
               Presence.of.open.home.loan +  Total.No.of.Trades + 
               Marital.Status..at.the.time.of.application. + No.of.dependents.x2 + 
               No.of.dependents.x3 + No.of.dependents.x4 + No.of.dependents.x5, 
             family = "binomial", data = train)

summary(model_4)
sort(vif(model_4))

#Excluding No.of.times.30.DPD.or.worse.in.last.6.months  due to low significance and high VIF
model_5<-glm(formula = Performance.Tag ~ 
               No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
               Avgas.CC.Utilization.in.last.12.months +  
               No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
               Presence.of.open.home.loan +  Total.No.of.Trades + 
               Marital.Status..at.the.time.of.application. + No.of.dependents.x2 + 
               No.of.dependents.x3 + No.of.dependents.x4 + No.of.dependents.x5, 
             family = "binomial", data = train)

summary(model_5)
sort(vif(model_5))

#Excluding No.of.dependents.x5  due to low significance and high VIF
model_6<-glm(formula = Performance.Tag ~ 
               No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
               Avgas.CC.Utilization.in.last.12.months +  
               No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
               Presence.of.open.home.loan +  Total.No.of.Trades + 
               Marital.Status..at.the.time.of.application. + No.of.dependents.x2 + 
               No.of.dependents.x3 + No.of.dependents.x4 , 
             family = "binomial", data = train)

summary(model_6)
sort(vif(model_6))

test_pred<-predict(model_6, type = "response",newdata = test[,-1])

summary(test_pred)
test$prob <- test_pred

test_actual<- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100){  
  OUT[i,] = perform_fn(s[i])
}

#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


best_cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
best_cutoff
# best_cutoff is 0.185
test_pred_Attrition <- ifelse(test_pred >= best_cutoff, "Yes", "No")
confusionmartix_final<-confusionMatrix(test_pred_Attrition, test_actual, positive = "Yes")

#####################################################################################
#Metrics - Accuracy,Sensitivity,Specificity,KS -statistic
#Lift & Gain Chart 
#####################################################################################

#confusion matrix output
confusionmartix_final

#####################################################################################

# Logistic Regression: using SMOTE analysis (to account for the data imbalance)

#Initial model - Build model 1 containing all variables
train_smote <- SMOTE(Performance.Tag ~ ., train, perc.over = 100, perc.under=200)

summary(train_smote$Performance.Tag)

# Tag= 1 implies default, 0 implies good
train_smote_model_1 = glm(Performance.Tag ~ ., data = train_smote, family = "binomial")
summary(train_smote_model_1)

# used STEPAIC to find the best model
train_smote_model_2 <- stepAIC(train_smote_model_1,direction = "both")
summary(train_smote_model_2)

# Removing multicollinearity through VIF check
sort(vif(train_smote_model_2))

#captured the STEPAIC output and built model2

#Excluding Outstanding.Balance due to low significance and high VIF
train_smote_model_3<-glm(formula = Performance.Tag ~ Age + No.of.months.in.current.residence + 
    No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months + 
    No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
    Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
    Presence.of.open.home.loan + Total.No.of.Trades + 
    Marital.Status..at.the.time.of.application. + No.of.dependents.x2 + 
    No.of.dependents.x3 + No.of.dependents.x4 + No.of.dependents.x5, 
    family = "binomial", data = train_smote)

summary(train_smote_model_3)
sort(vif(train_smote_model_3))

#Excluding No.of.times.60.DPD.or.worse.in.last.6.months due to low significance and high VIF
train_smote_model_4<-glm(formula = Performance.Tag ~ Age + No.of.months.in.current.residence + 
    No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.6.months + 
	No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
	No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
    Presence.of.open.home.loan + Total.No.of.Trades + 
    Marital.Status..at.the.time.of.application. + No.of.dependents.x2 + 
    No.of.dependents.x3 + No.of.dependents.x4 + No.of.dependents.x5, 
    family = "binomial", data = train_smote)

summary(train_smote_model_4)
sort(vif(train_smote_model_4))

test_pred_smote<-predict(train_smote_model_4, type = "response",newdata = test[,-1])

summary(test_pred_smote)
test$prob_smote <- test_pred_smote

test_actual_smote<- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100){  
  OUT[i,] = perform_fn(s[i])
}

#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


best_cutoff_smote <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
best_cutoff_smote
# best_cutoff is 0.185
test_pred_Attrition_smote <- ifelse(test_pred_smote >= best_cutoff, "Yes", "No")
confusionmartix_final_smote<-confusionMatrix(test_pred_Attrition_smote, test_actual_smote, positive = "Yes")

#####################################################################################
#Metrics - Accuracy,Sensitivity,Specificity,KS -statistic
#Lift & Gain Chart 
#####################################################################################

#confusion matrix output
confusionmartix_final_smote

#####################################################################################
#Random Forrest 
#####################################################################################
set.seed(100)
trainindices= sample(1:nrow(Master_final), 0.7*nrow(Master_final))
train = Master_final[trainindices,]
test = Master_final[-trainindices,]

rf_fit <- randomForest(Performance.Tag~.,train,ntree=500,importance=T)

plot(rf_fit)

varImpPlot(rf_fit,sort = T,main="Variable Importance",n.var=5)

var.imp <- data.frame(importance(rf_fit,type=2))

var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

test$predicted.prob <- predict(rf_fit ,test[,-1], type = "prob")

s = seq(.5,.99,length=100)

OUT = matrix(0,100,3)

for(i in 1:100){  
  OUT[i,] = perform_fn1(s[i])
} 

#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

best_cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
best_cutoff

test$Performance.Tag<- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
test$predicted.response_1<- factor(ifelse(test$predicted.prob[,1] >= best_cutoff, "Yes", "No"))
confusionmartix_final_rf<-confusionMatrix(test$predicted.response_1, test$Performance.Tag, positive = "Yes")

############################Random forrest ###########################################
#Metrics - Accuracy,Sensitivity,Specificity,KS -statistic
#Lift & Gain Chart 
#####################################################################################

#confusion matrix output
confusionmartix_final_rf

#####################################################################################
#Application score card
#####################################################################################
test$log_odds <- log(test$prob/(1-test$prob))

PDO <- 20
BaseScore <- 400
Odds <- 10

#Calaculating Factor & Offset
Factor=PDO/log(2)

Offset=BaseScore-(Factor*log(Odds))

Offset
Factor

print("equation is : score = 333.5614 + (28.8539 * log(odds))")

test$score <- 333.5614 + (28.8539 * test$log_odds)
test$score

View(test [,c(1,39:41)])