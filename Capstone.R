
################################################################################################
#Import nessesary packages
################################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("D:\\F\\IIITB capstone Project\\Source files")

################################################################################################
#Data loading
################################################################################################
# Both Data files are CSV files
demographics = read.csv("Demographic data.csv")
CreditBureau = read.csv("Credit Bureau data.csv")


################################################################################################
#Initail_validation
################################################################################################
dim(demographics)
### 71295 rows X 12 columns

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

### blank values count in each feature
sapply(demographics, function(x) sum(x=="",na.rm = TRUE))

###missing values count in each feature
missing_values_count <-gather(demographics %>%summarise_all(funs(sum(is.na(.)))),
                              key='feature',value = 'missing_percentage')
missing_values_count

### missing values percentage in each feature
missing_values <- demographics %>%  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values_count <-gather(demographics %>%summarise_all(funs(sum(is.na(.)))),
                              key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()

### almost 2% (1425/71295) of the records has NA values in performance Tag column.target column should not have NAs
### No.of.dependents column has 3/71295 na values.these can be removed as these are very less in number

d<-demographics[complete.cases(demographics), ]

#####################################################################################################
# Distribution of factor/categorical variables with corresponding frequencies

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

univariate_continuous <- function(dataset,var,var_name){
  dataset %>% ggplot(aes(x = (var))) +
    geom_histogram(breaks=seq(min(var), max(var), by=1),col="red", aes(fill=..count..)) +
    scale_fill_gradient("Count", low="green", high="red")+
  labs(title = var_name, y = "Count", x = var_name)
    
}

#####################################################################################################
############## Univariate Analysis  ###############

###average performace_tag
mean(d$Performance.Tag)
d$Performance.Tag <-as.factor(d$Performance.Tag)
### only 4.2 percentage of the records are 1

### categorical Distribution
univariate_categorical(d,d$Gender,"Gender Distribution")
univariate_categorical(d,d$Marital.Status..at.the.time.of.application.,"Marital status Distribution")
univariate_categorical(d,d$No.of.dependents,"dependents Distribution")
univariate_categorical(d,d$Education,"Education Distribution")
univariate_categorical(d,d$Profession,"Profession Distribution")
univariate_categorical(d,d$Type.of.residence,"residence type Distribution")
univariate_categorical(d,d$Performance.Tag,"performance Distribution")


###76% is male population
###85% are married
### numbe of dependents distribution is same and 22% has 3 dependents
###  33.6% masters and 34.9% professional. Blanks 2% can be combined with others which is 2%
### 56.8 % are salaried. Blanks are almost 0 percentage. blanks can be handled based on the performance tag/changed to sal
### 74.8% are rented.blanks can be changed to rented

###continuous values distribution

univariate_continuous(d,d$Age,"Age Distribution")
univariate_continuous(d,d$Income,"Income Distribution")
summary(factor(d$Income))
univariate_continuous(d,d$No.of.months.in.current.residence,"Current residence Distribution")
summary(factor(d$No.of.months.in.current.residence))
univariate_continuous(d,d$No.of.months.in.current.company,"Current company Distribution")
summary(factor(d$No.of.months.in.current.company))

### high distribution from 24 to 65 years
### there very high spike at income 4.5 and 107 persons has less than or equal to 0
### huge spike for number of months in current residence at 6 months 
### big spike for 3 months in current company. 
