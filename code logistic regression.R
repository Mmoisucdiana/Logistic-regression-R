library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(ROCR)
library(dplyr)
library(ggthemes)

str(HR1)
dim(HR)
sum(is.na(HR))
(colSums(is.na(HR))/nrow(HR))*100

HR$NumCompaniesWorked[which(is.na(HR$NumCompaniesWorked))]<-median(HR$NumCompaniesWorked, na.rm = TRUE)
HR$TotalWorkingYears[which(is.na(HR$TotalWorkingYears))]<-median(HR$TotalWorkingYears, na.rm = TRUE)

HR$Education[which(HR$Education==1)]<-'Below College'
HR$Education[which(HR$Education==2)]<-'College'
HR$Education[which(HR$Education==3)]<-'Bachelor'
HR$Education[which(HR$Education==4)]<-'Master'
HR$Education[which(HR$Education==5)]<-'Doctor'



summary(HR)
HR$Attrition<- ifelse(HR$Attrition=="Yes",1,0)
HR$Gender<- ifelse(HR$Gender=="Male",1,0)

HR1<-subset(HR,select=-c(EmployeeCount,DistanceFromHome, EmployeeID,JobRole,Department,PercentSalaryHike,StandardHours,TrainingTimesLastYear,Over18,StockOptionLevel,YearsWithCurrManager,YearsSinceLastPromotion))

#Model logit fara variabile independenta
mylogit_dep<-glm(HR$Attrition ~1, data=HR, family="binomial")
summary(mylogit_dep)


#Model logit variabile independenta(AGE)
mylogit_1dep <-glm(HR1$Attrition ~HR1$Age, data=HR1, family="binomial")
summary(mylogit_1dep)

#Model logit variabile independenta(travel)
mylogit_2 <-glm(HR1$Attrition ~HR1$BusinessTravel, data=HR1, family="binomial")
summary(mylogit_2)

#Model logit variabile independenta(Education)
mylogit_3 <-glm(HR1$Attrition ~HR1$Education, data=HR1, family="binomial")
summary(mylogit_3)

#Model logit variabile independenta(Educationfield)
mylogit_4 <-glm(HR1$Attrition ~HR1$EducationField, data=HR1, family="binomial")
summary(mylogit_4)

#Model logit variabile independenta Gender(nesemnificativ)
mylogit_5 <-glm(HR1$Attrition ~HR1$Gender, data=HR1, family="binomial")
summary(mylogit_5)

mylogit_6 <-glm(HR1$Attrition ~HR1$MaritalStatus, data=HR1, family="binomial")
summary(mylogit_6)

mylogit_8 <-glm(HR1$Attrition ~HR1$MonthlyIncome, data=HR1, family="binomial")
summary(mylogit_8)

mylogit_9 <-glm(HR1$Attrition ~HR1$NumCompaniesWorked, data=HR1, family="binomial")
summary(mylogit_9)

mylogit_10 <-glm(HR1$Attrition ~HR1$TotalWorkingYears, data=HR1, family="binomial")
summary(mylogit_10)

mylogit_11 <-glm(HR1$Attrition ~HR1$YearsAtCompany, data=HR1, family="binomial")
summary(mylogit_11)

mylogit_all <-glm(HR1$Attrition ~HR1$Age+HR1$BusinessTravel+HR1$MaritalStatus
                  +HR1$NumCompaniesWorked+HR1$TotalWorkingYears, data=HR1, family="binomial")
summary(mylogit_all)



#verificarea acuratetii predictiilor realizate cu modelul de regresie complet
mylogit_all_probab<-predict(mylogit_all, type="response")
mylogit_all_probab[1:5]

mylogit_all_probab_50<-ifelse(mylogit_all_probab > 0.50,1,0)
mylogit_all_probab_95<-ifelse(mylogit_all_probab > 0.95,1,0)

table(mylogit_all_probab_50,HR1$Attrition)
table(mylogit_all_probab_95,HR1$Attrition)

mean(mylogit_all_probab_50 ==HR1$Attrition)
mean(mylogit_all_probab_95 ==HR1$Attrition)



#verificarea acuratetii predictiilor realizate cu modelul de regresie pentru variabila totalworkingyears
mylogit_10_probab<-predict(mylogit_10, type="response")
mylogit_10_probab[1:5]

mylogit_10_probab_50<-ifelse(mylogit_10_probab > 0.50,1,0)
mylogit_10_probab_95<-ifelse(mylogit_10_probab > 0.95,1,0)

table(mylogit_10_probab_50,HR1$Attrition)
table(mylogit_10_probab_95,HR1$Attrition)

mean(mylogit_10_probab_50 ==HR1$Attrition)
mean(mylogit_10_probab_95 ==HR1$Attrition)

wald.test(b=coef(mylogit_all), Sigma= vcov(mylogit_all), Terms = 2:5)

exp(cbind(OR=coef(mylogit_all), confint(mylogit_all, level=0.9)))

vif(mylogit_all)


anova(mylogit_all, mylogit_dep, test="LRT")

wald.test(b = coef(mylogit_all), Sigma = vcov(mylogit_all), Terms = 2:5)

logitgof(HR1$Attrition, fitted(mylogit_all), g=10)
 