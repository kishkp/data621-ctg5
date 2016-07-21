library("papaja")
apa_prepare_doc() # Prepare document for rendering
if (!require("ggplot2",character.only = TRUE)) (install.packages("ggplot2",dep=TRUE))
if (!require("MASS",character.only = TRUE)) (install.packages("MASS",dep=TRUE))
if (!require("knitr",character.only = TRUE)) (install.packages("knitr",dep=TRUE))
if (!require("xtable",character.only = TRUE)) (install.packages("xtable",dep=TRUE))
if (!require("dplyr",character.only = TRUE)) (install.packages("dplyr",dep=TRUE))
if (!require("psych",character.only = TRUE)) (install.packages("psych",dep=TRUE))
if (!require("stringr",character.only = TRUE)) (install.packages("stringr",dep=TRUE))
#if (!require("car",character.only = TRUE)) (install.packages("car",dep=TRUE))
if (!require("faraway",character.only = TRUE)) (install.packages("faraway",dep=TRUE))
if (!require("popbio",character.only = TRUE)) (install.packages("popbio",dep=TRUE))
if (!require("gdata",character.only = TRUE)) (install.packages("gdata",dep=TRUE))
if (!require("reshape",character.only = TRUE)) (install.packages("reshape",dep=TRUE))
if (!require("rpart",character.only = TRUE)) (install.packages("rpart",dep=TRUE))
if (!require("randomForest",character.only = TRUE)) (install.packages("randomForest",dep=TRUE))
if (!require("boot",character.only = TRUE)) (install.packages("boot",dep=TRUE))


#install.packages("fancyvrb")

library(ggplot2)
library(MASS)
library(knitr)
library(xtable)
library(dplyr)
library(psych)
library(stringr)
#library(car)
library(faraway)
library(aod)
library(Rcpp)
library(leaps)
library(ISLR)
library(AUC)
library(ROCR)
library(Amelia)
library(popbio)
library(gdata)
library(reshape)
library(gridExtra)
library(rpart)
library(randomForest)
library(boot)
library(ResourceSelection)



bank_train <- read.table(
  "https://raw.githubusercontent.com/kishkp/data621-ctg5/master/Final%20Project/bank-additional-full.csv",
           sep = ";",
           header = TRUE)

bank_test <-read.table(
  "https://raw.githubusercontent.com/kishkp/data621-ctg5/master/Final%20Project/bank-additional.csv",
           sep = ";",
           header = TRUE)

variable_analysis<- read.csv(
  "https://raw.githubusercontent.com/kishkp/data621-ctg5/master/Final%20Project/Variable%20Analysis.csv")
kable(variable_analysis, caption = "Variable Analysis") 


bank_train2<-bank_train

#update response variable y to binary values of 0 and 1
#levels(bank_train2$y)
levels(bank_train2$y) <- c(0, 1)
bank_train2$y <- as.numeric(levels(bank_train2$y))[bank_train2$y]
#str(bank_train2)


# age is numeric 

#create dummy variables for job values
for(level in unique(bank_train2$job)){
  bank_train2[paste("job", level, sep = "_")] <- ifelse(bank_train2$job == level, 1, 0)
}
#Delete original catagorical variable
bank_train2$job <- NULL
#head(bank_train2)

#create dummy variables for marital values
#levels(bank_train2$marital)
for(level in unique(bank_train2$marital)){
  bank_train2[paste("marital", level, sep = "_")] <- ifelse(bank_train2$marital == level, 1, 0)
}
#Delete original catagorical variable
bank_train2$marital <- NULL
#head(bank_train2)

#--------------------------------------------------------------------------
#education dummy variables primary, secondary tertiary, unknown and illiterate

#education_None
bank_train2$education_illiterate <- as.numeric(ifelse(bank_train2$education == "illiterate", 1, 0))
#table(bank_train2$education_Illiterate)

#education_Unknown
bank_train2$education_unknown <-as.numeric(ifelse(bank_train2$education == "unknown", 1, 0))
#table(bank_train2$education_Unknown)

#education_Primary
bank_train2$education_primary <- as.numeric(ifelse(bank_train2$education == "basic.4y" 
                                        | bank_train2$education == "basic.6y", 1, 0))
#table(bank_train2$education_Primary)

#education_Secondary
bank_train2$education_secondary <- as.numeric(ifelse(bank_train2$education == "basic.9y" 
                                        | bank_train2$education == "high.school", 1, 0))
#table(bank_train2$education_Secondary)

#education_Tertiary
bank_train2$education_tertiary <- as.numeric(ifelse(bank_train2$education == "professional.course" 
                                        | bank_train2$education == "university.degree", 1, 0))
#table(bank_train2$education_Tertiary)

#Delete original catagorical variable
bank_train2$education <- NULL


# contact has 2 levels - 1 variable is required
#levels(bank_train2$default)
for(level in unique(bank_train2$default)){
  bank_train2[paste("default", level, sep = "_")] <- ifelse(bank_train2$default == level, 1, 0)
}
#Delete original catagorical variable
bank_train2$default <- NULL


#levels(bank_train2$housing)
for(level in unique(bank_train2$housing)){
  bank_train2[paste("housing", level, sep = "_")] <- ifelse(bank_train2$housing == level, 1, 0)
}
#Delete original catagorical variable
bank_train2$housing <- NULL

#levels(bank_train2$loan)
for(level in unique(bank_train2$loan)){
  bank_train2[paste("loan", level, sep = "_")] <- ifelse(bank_train2$loan == level, 1, 0)
}
#Delete original catagorical variable
bank_train2$loan <- NULL

# contact has 2 levels - 1 variable is required
#levels(bank_train2$contact)
for(level in unique(bank_train2$contact)){
  bank_train2[paste("contact", level, sep = "_")] <- ifelse(bank_train2$contact == level, 1, 0)
}
#Delete original catagorical variable
bank_train2$contact <- NULL

#levels(bank_train2$month)
for(level in unique(bank_train2$month)){
  bank_train2[paste("month", level, sep = "_")] <- ifelse(bank_train2$month == level, 1, 0)
}
#Delete original catagorical variable
bank_train2$month <- NULL

#levels(bank_train2$day_of_week)
for(level in unique(bank_train2$day_of_week)){
  bank_train2[paste("day_of_week", level, sep = "_")] <- ifelse(bank_train2$day_of_week == level, 1, 0)
}
#Delete original catagorical variable
bank_train2$day_of_week <- NULL

#duration is numeric
#campaign is numeric

#pdays is numeric
#dummy variable for pdays -previous contact yes or no ; 1 or 0  when 999
bank_train2$previous_contact <- as.numeric(ifelse(bank_train2$pdays == 999, 0, 1))

#previous is numeric

#levels(bank_train2$poutcome)
for(level in unique(bank_train2$poutcome)){
  bank_train2[paste("poutcome", level, sep = "_")] <- ifelse(bank_train2$poutcome == level, 1, 0)
}
#Delete original catagorical variable
bank_train2$poutcome <- NULL

# emp.var.rate is numeric
# cons.price.idx is numeric
# cons.conf.idx  is numeric
# euribor3m is numeric
# nr.employed is numeric



bank_test2<-bank_test

#update response variable to binary values of 0 and 1
levels(bank_test2$y) <- c(0, 1)
bank_test2$y <- as.numeric(levels(bank_test2$y))[bank_test2$y]

# age is numeric 

#create dummy variables for job values
for(level in unique(bank_test2$job)){
  bank_test2[paste("job", level, sep = "_")] <- ifelse(bank_test2$job == level, 1, 0)
}
#Delete original catagorical variable
bank_test2$job <- NULL
#head(bank_test2)

#create dummy variables for marital values
#levels(bank_test2$marital)
for(level in unique(bank_test2$marital)){
  bank_test2[paste("marital", level, sep = "_")] <- ifelse(bank_test2$marital == level, 1, 0)
}
#Delete original catagorical variable
bank_test2$marital <- NULL
#head(bank_test2)

#--------------------------------------------------------------------------
#education dummy variables primary, secondary tertiary, unknown and illiterate

#education_None
bank_test2$education_illiterate <- as.numeric(ifelse(bank_test2$education == "illiterate", 1, 0))
#table(bank_test2$education_Illiterate)

#education_Unknown
bank_test2$education_unknown <-as.numeric(ifelse(bank_test2$education == "unknown", 1, 0))
#table(bank_test2$education_Unknown)

#education_Primary
bank_test2$education_primary <- as.numeric(ifelse(bank_test2$education == "basic.4y" 
                                        | bank_test2$education == "basic.6y", 1, 0))
#table(bank_test2$education_Primary)

#education_Secondary
bank_test2$education_secondary <- as.numeric(ifelse(bank_test2$education == "basic.9y" 
                                        | bank_test2$education == "high.school", 1, 0))
#table(bank_test2$education_Secondary)

#education_Tertiary
bank_test2$education_tertiary <- as.numeric(ifelse(bank_test2$education == "professional.course" 
                                        | bank_test2$education == "university.degree", 1, 0))
#table(bank_test2$education_Tertiary)

#Delete original catagorical variable
bank_test2$education <- NULL
#---------------------------------------------------

# contact has 2 levels - 1 variable is required
#levels(bank_test2$default)
for(level in unique(bank_test2$default)){
  bank_test2[paste("default", level, sep = "_")] <- ifelse(bank_test2$default == level, 1, 0)
}
#Delete original catagorical variable
bank_test2$default <- NULL


#levels(bank_test2$housing)
for(level in unique(bank_test2$housing)){
  bank_test2[paste("housing", level, sep = "_")] <- ifelse(bank_test2$housing == level, 1, 0)
}
#Delete original catagorical variable
bank_test2$housing <- NULL

#levels(bank_test2$loan)
for(level in unique(bank_test2$loan)){
  bank_test2[paste("loan", level, sep = "_")] <- ifelse(bank_test2$loan == level, 1, 0)
}
#Delete original catagorical variable
bank_test2$loan <- NULL

# contact has 2 levels - 1 variable is required
#levels(bank_test2$contact)
for(level in unique(bank_test2$contact)){
  bank_test2[paste("contact", level, sep = "_")] <- ifelse(bank_test2$contact == level, 1, 0)
}
#Delete original catagorical variable
bank_test2$contact <- NULL

#levels(bank_test2$month)
for(level in unique(bank_test2$month)){
  bank_test2[paste("month", level, sep = "_")] <- ifelse(bank_test2$month == level, 1, 0)
}
#Delete original catagorical variable
bank_test2$month <- NULL

#levels(bank_test2$day_of_week)
for(level in unique(bank_test2$day_of_week)){
  bank_test2[paste("day_of_week", level, sep = "_")] <- ifelse(bank_test2$day_of_week == level, 1, 0)
}
#Delete original catagorical variable
bank_test2$day_of_week <- NULL

#duration is numeric
#campaign is numeric

#pdays is numeric
#dummy variable for pdays -previous contact yes or no ; 1 or 0  when 999
bank_test2$previous_contact <- as.numeric(ifelse(bank_test2$pdays == 999, 0, 1))

#previous is numeric

#levels(bank_test2$poutcome)
for(level in unique(bank_test2$poutcome)){
  bank_test2[paste("poutcome", level, sep = "_")] <- ifelse(bank_test2$poutcome == level, 1, 0)
}
#Delete original catagorical variable
bank_test2$poutcome <- NULL

# emp.var.rate is numeric
# cons.price.idx is numeric
# cons.conf.idx  is numeric
# euribor3m is numeric
# nr.employed is numeric



# update "-" in train set
colnames(bank_train2)[15]<-c("job_blue_collar")
colnames(bank_train2)[20]<-c("job_self_employed")

# remove"-" from variable to avoid any issues in running the model in test
colnames(bank_test2)[12]<-c("job_blue_collar")
colnames(bank_test2)[16]<-c("job_self_employed")

bank_train2$y<-as.factor(bank_train2$y)
model1 <- glm(y ~.,family=binomial,data=na.omit(bank_train2))
summary(model1)

#anova(model1, test="Chisq")
model1_update<-glm(y ~.-job_student-marital_unknown-education_tertiary-education_tertiary-default_yes-housing_unknown-loan_yes-loan_unknown-contact_cellular-month_sep-day_of_week_fri-poutcome_success,family=binomial,data=na.omit(bank_train2))

# exp(coef(model1_update))
# Cross validation of model for K=5
t1<-cv.glm(bank_train2, model1_update, K = 10)$call
cv.glm(data = bank_train2, glmfit = model1_update, K = 5)

## 
## 
## # model1_var <-read.table("https://raw.githubusercontent.com/kishkp/data621-ctg5/master/Final%20Project/Model1_var.csv",
## #            sep = ",",
## #            header = TRUE)
## 
## #model1_var<- read.csv("https://raw.githubusercontent.com/kishkp/data621-ctg5/master/Final%20Project/Model1_var.csv")
## #kable(model1_var, caption = "Signifcant Variables model 1")
## 

model2 <- rpart(y~., data=na.omit(bank_train2), method = "class")

model2_update <- prune(model2, cp = model2$cptable[which.min(model2$cptable[,"xerror"]),"CP"])

plotcp(model2_update)

#printcp(model2_update) # display the results 

              
# plot the pruned tree 
par(mfrow = c(1, 3), mar = rep(0.1, 4))
par(mfrow=c(1,1))
plot(model2_update, uniform=TRUE, main=NA)
text(model2_update, use.n=TRUE, all=TRUE, cex=.8)



# Random Forest prediction of Kyphosis data

model3 <- randomForest(as.factor(y) ~ .,data=bank_train2,importance=TRUE, ntree=50)
# print(model3) # view results 


model4 <- randomForest(as.factor(y) ~ .,data=bank_train2,importance=TRUE, ntree=100)

#plot model 3

par(mfrow=c(2,3))

#layout(matrix(c(1,2),nrow=1),
#width=c(1,1)) 
par(mar=c(1,1,2,0)) #No margin on the right side
plot(model3, log="y")
par(mar=c(3,3,1,1)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(model3$err.rate),col=1:3,cex=0.8,fill=1:4)

# plot model 4

#layout(matrix(c(1,2),nrow=1),
#width=c(1,1)) 
par(mar=c(1,1,2,1)) #No margin on the right side
plot(model4, log="y")
par(mar=c(1,3,1,1)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
#legend("top", colnames(model4$err.rate),col=1:3,cex=0.8,fill=1:4)



# display importance of variables
varImpPlot(model3)


#Following function Eval() will be used to calculate various metrics related to the model like Accuracy, Sensitivity, #Precision , Specificity, and F1 score

Eval<-function(x){
    TP<-x$Freq[x$metrics=="TRUE_1"]
    FP<-x$Freq[x$metrics=="FALSE_1"]
    TN<-x$Freq[x$metrics=="FALSE_0"]
    FN<-x$Freq[x$metrics=="TRUE_0"]
    Accuracy <-(TP+TN)/(TP+TN+FP+FN)
    Error_Rate<-(FP+FN)/(TP+TN+FP+FN)
    Precision<-TP/(TP+FP)
    sensitivity<-TP/(TP+FN)
    specificity<-TN/(TN+FP)
    F1_Score=2*Precision*sensitivity/(sensitivity+specificity)
    eval_result<-data.frame(Accuracy=c(0),Error_Rate=c(0),Precision=c(0),sensitivity=c(0),specificity=c(0),F1_Score=c(0))
    
    eval_result[1,1]<-Accuracy
    eval_result[1,2]<-Error_Rate
    eval_result[1,3]<- Precision
    eval_result[1,4]<-sensitivity
    eval_result[1,5]<-specificity
    eval_result[1,6]<-F1_Score
    eval_result
}

model_comparison<-data.frame(Accuracy=c(0),Error_Rate=c(0),Precision=c(0),sensitivity=c(0),specificity=c(0),F1_Score=c(0), AUC=c(0))


#the McFadden R^2 index can be used to assess the model fit.
#pR2(model1)

# Predict result from the model 1 with probability

bank_test2$TARGET_FLAG1<-predict(model1_update,newdata=na.omit(bank_test2),type='response')



#confusion matrix model 1


df_pre_train1<-as.data.frame(table(bank_test2$TARGET_FLAG1>0.5,bank_test2$y))

df_pre_train1$metrics <- paste(df_pre_train1$Var1,df_pre_train1$Var2, sep = '_') 

model_comparison[1,]<-Eval(df_pre_train1)

# cauculate AUC

results1<-ifelse(bank_test2$TARGET_FLAG1>0.5,1,0)

pr <- prediction(results1, bank_test2$y)

auc1<- performance(pr,"auc")

model_comparison[1,c("AUC")]<-c(auc1@y.values[1])


#kable(model_comparison[1,],row.names = TRUE, caption = " Model 1 evaluation KPIs")

library(ResourceSelection)
m <- model1_update<-glm(y ~.-job_student-marital_unknown-education_tertiary-education_tertiary-default_yes-housing_unknown-loan_yes-loan_unknown-contact_cellular-month_sep-day_of_week_fri-poutcome_success,family=binomial,data=na.omit(bank_test2))
hoslem.test(model1_update$y, fitted(m))

bank_test2$TARGET_FLAG2<- predict(model2_update,newdata=bank_test2)[,2]


#confusion matrix model 2

df_pre_train1<-as.data.frame(table(bank_test2$TARGET_FLAG2>0.5,bank_test2$y))



df_pre_train1$metrics <- paste(df_pre_train1$Var1,df_pre_train1$Var2, sep = '_') 

model_comparison[2,]<-Eval(df_pre_train1)

# Calulate AUC

results2 <- predict(model2_update,newdata=bank_test2,type="prob")[,2]

pr <- prediction(results2, bank_test2$y)


auc2<- performance(pr,"auc")

model_comparison[2,c("AUC")]<-c(auc2@y.values[1])

#kable(model_comparison[2,],row.names = TRUE, caption = " Model 1 evaluation KPIs")


# results from model 3

bank_test2$TARGET_FLAG3<-as.numeric(predict(model3,newdata=bank_test2,type='response'))

bank_test2$TARGET_FLAG3<-ifelse(bank_test2$TARGET_FLAG3==1,0,1)


#confusion matrix model 3

df_pre_train1<-as.data.frame(table(bank_test2$TARGET_FLAG3,bank_test2$y))

df_pre_train1$Var1<-as.character(df_pre_train1$Var1)
df_pre_train1$Var1[df_pre_train1$Var1==0]<-c("FALSE")
df_pre_train1$Var1[df_pre_train1$Var1==1]<-c("TRUE")

df_pre_train1$metrics <- paste(df_pre_train1$Var1,df_pre_train1$Var2, sep = '_') 

model_comparison[3,]<-Eval(df_pre_train1)

# Calculaate AUC

#results3 <- ifelse(as.numeric(predict(model4,newdata=na.omit(bank_test2),type='response'))==1,0,1)

pr <- prediction(bank_test2$TARGET_FLAG3, bank_test2$y)


auc3<- performance(pr,"auc")


model_comparison[3,c("AUC")]<-c(auc3@y.values[1])


#kable(model_comparison[3,],row.names = TRUE, caption = " Model 1 evaluation KPIs")


model_comparison$Model<-c("GLM","CRT","RF")
kable(model_comparison[1:3,c(8,1,2,3,4,5,6,7)],row.names = TRUE, caption = "Comparison of 3 Model3")


# Area under curve model 1

bank_test2$y<-as.factor(bank_test2$y)

myRoc1 <- pROC::roc(bank_test2$y,bank_test2$TARGET_FLAG1) 

# Area under curve model 2
myRoc2 <- pROC::roc(bank_test2$y,bank_test2$TARGET_FLAG2)

# Area under curve model 3

myRoc3<- pROC::roc(bank_test2$y,bank_test2$TARGET_FLAG3) 

plot (c(1,0),c(0,1),type="n", xlab="Specificity",ylab="Sensitivity", xlim=rev(range(myRoc1$specificities)))

lines(x = myRoc1$specificities,y=myRoc1$sensitivities,lwd=2.5, lty=1)
lines(x = myRoc2$specificities,y=myRoc2$sensitivities,lwd=2.5, lty=2)
lines(x = myRoc3$specificities,y=myRoc3$sensitivities,lwd=2.5, lty=3)

legend(x = "bottomright", c("GLM_model1","CRT_model2","RF_model3"), lty=c(1,2,3), lwd=c(2.5,2.5, 2.5))

r_refs(file = "r-references.bib")

variables<- read.csv(
  "https://raw.githubusercontent.com/kishkp/data621-ctg5/master/Final%20Project/Variable%20Description.csv")
kable(variables, caption = "Variable Description") 



#round(prop.table(table(bank_train$y, bank_train$age),2)*100,2)  #student, retired
ggplot(bank_train, aes(age)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#round(prop.table(table(bank_train$y, bank_train$job),2)*100,2)  #student, retired
ggplot(bank_train, aes(job)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#round(prop.table(table(bank_train$y, bank_train$marital),2)*100,2) #unknown
ggplot(bank_train, aes(marital)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#round(prop.table(table(bank_train$y, bank_train$education),2)*100,2) #illeterate
ggplot(bank_train, aes(education)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#round(prop.table(table(bank_train$y, bank_train$default),2)*100,2) #no
ggplot(bank_train, aes(default)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#round(prop.table(table(bank_train$y, bank_train$housing),2)*100,2) #no
ggplot(bank_train, aes(housing)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#round(prop.table(table(bank_train$y, bank_train$loan),2)*100,2) #no
ggplot(bank_train, aes(loan)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#round(prop.table(table(bank_train$y, bank_train$contact),2)*100,2) #cellular
ggplot(bank_train, aes(contact)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
#round(prop.table(table(bank_train$y, bank_train$month),2)*100,2) #march, dec, sep, oct
ggplot(bank_train, aes(month)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
#round(prop.table(table(bank_train$y, bank_train$day_of_week),2)*100,2) #Thursday
ggplot(bank_train, aes(day_of_week)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
#round(prop.table(table(bank_train$y, bank_train$duration),2)*100,2) #increases with every contact upto 5
ggplot(bank_train, aes(duration)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ xlim(0, 2200)

#round(prop.table(table(bank_train$y, bank_train$campaign),2)*100,2) #increases with every contact upto 5
ggplot(bank_train, aes(campaign)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#round(prop.table(table(bank_train$y, bank_train$pdays),2)*100,2) #increases with every contact upto 5
ggplot(bank_train, aes(pdays)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#round(prop.table(table(bank_train$y, bank_train$previous),2)*100,2) #increases with every contact upto 5
ggplot(bank_train, aes(previous)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

  #round(prop.table(table(bank_train$y, bank_train$poutcome),2)*100,2) #success
ggplot(bank_train, aes(poutcome)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

 #round(prop.table(table(bank_train$y, bank_train$emp.var.rate),2)*100,2) #success
ggplot(bank_train, aes(emp.var.rate)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

 #round(prop.table(table(bank_train$y, bank_train$cons.price.idx),2)*100,2) #success
ggplot(bank_train, aes(cons.price.idx)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

 #round(prop.table(table(bank_train$y, bank_train$cons.conf.idx),2)*100,2) #success
ggplot(bank_train, aes(cons.conf.idx)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

 #round(prop.table(table(bank_train$y, bank_train$euribor3m),2)*100,2) #success
ggplot(bank_train, aes(euribor3m)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

 #round(prop.table(table(bank_train$y, bank_train$nr.employed),2)*100,2) #success
ggplot(bank_train, aes(nr.employed)) + geom_bar(aes(fill = y), position = "fill")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


par(mfrow=c(1,1))
#finding missing values
missings<- data.frame(sapply(bank_train,function(x) sum(is.na(x))))
names(missings)[1]<- paste("Missing Values")
kable(missings, caption = "Missing Values")

# missing values graph
#missmap(bank_train, main = "Missing values vs observed")

### finding unique values
uniques<- data.frame(sapply(bank_train, function(x) length(unique(x))))
names(uniques)[1]<- paste("Unique Values")
kable(uniques, caption = "Unique Values")

## 
## #str(bank_train2)
## #bank_train2$y<-as.numeric(bank_train2$y)
## ds_stats <- psych::describe(bank_train2, skew = TRUE, na.rm = TRUE)
## #ds_stats
## kable(ds_stats[1:7], caption= "Data Summary")
## kable(ds_stats[8:13], caption= "Data Summary (Cont)")
## 
## #head(bank_train2)
## 
## fun1 <- function(a, y) cor(y, a)
## x<-bank_train2[,]
## Correlation <- sapply(x, FUN = fun1, y=bank_train2$y)
## 
## 
## Correlation <- sort(Correlation, decreasing = TRUE)
## #head(Correlation)
## kable(Correlation, caption = "Variable Correlation")
## 
## #str(bank_train2)
## #str(bank_train)
## #summary(bank_train2)
## 

mdata<- select(bank_train2,  age, previous, duration, campaign, emp.var.rate, cons.price.idx, euribor3m,nr.employed)
mdata2 <- melt(mdata)

# Output the boxplot
p <- ggplot(data = mdata2, aes(x=variable, y=value)) + 
  geom_boxplot() + ggtitle("Outliers Identification")
p + facet_wrap( ~ variable, scales="free", ncol=4)
## 
## bank_train2 <- bank_train2 %>%
##   select(-y, everything())
## 
## fun1 <- function(a, y) cor(y, a)
## x<-bank_train2[,]
## Correlation <- sapply(x, FUN = fun1, y=bank_train2$y)
## 
## 
## Correlation <- sort(Correlation, decreasing = TRUE)
## 
## vars <- names(Correlation)
## 
## 
## par(mfrow=c(2,4))
## for (i in 2:ncol(bank_train2)) {
##   hist(bank_train2[,vars[i]], main = vars[i], xlab = "")
## }
## 
## 
## #move y to the last column
## 
## 
## par(mfrow=c(2,4))
## #Show in the order of Correlation
## p = list()
## #for (i in 2:ncol(bank_train2)) p[[i]] <- qplot(bank_train2[,i], xlab=names(bank_train2)[[i]])
## for (i in 2:ncol(bank_train2)) {
##   p[[i]] <- logi.hist.plot(bank_train2[,vars[i]],bank_train2$y,logi.mod = 1, type='hist', boxp=FALSE,col='grey',
##                            mainlabel = vars[i])
## }
## #do.call(grid.arrange, p)
## #plot(p)
## #plot(p[[1]], p[[2]])
## #plot (p$your.x.coordinate, p$your.y.coordinate)
## #head(bank_train2)
## 
## 
