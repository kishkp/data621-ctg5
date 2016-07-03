if (!require("ggplot2",character.only = TRUE)) (install.packages("ggplot2",dep=TRUE))
if (!require("MASS",character.only = TRUE)) (install.packages("MASS",dep=TRUE))
if (!require("knitr",character.only = TRUE)) (install.packages("knitr",dep=TRUE))
if (!require("xtable",character.only = TRUE)) (install.packages("xtable",dep=TRUE))
if (!require("dplyr",character.only = TRUE)) (install.packages("dplyr",dep=TRUE))
if (!require("psych",character.only = TRUE)) (install.packages("psych",dep=TRUE))
if (!require("stringr",character.only = TRUE)) (install.packages("stringr",dep=TRUE))
#if (!require("car",character.only = TRUE)) (install.packages("car",dep=TRUE))
if (!require("faraway",character.only = TRUE)) (install.packages("faraway",dep=TRUE))

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
library(pROC)

city_crime_train_full <- read.csv("https://raw.githubusercontent.com/kishkp/data621-ctg5/master/HW3/crime-training-data.csv")

variables<- read.csv("https://raw.githubusercontent.com/kishkp/data621-ctg5/master/HW3/variables2.csv")
kable(variables, caption = "Variable Description") 




city_crime_test <- read.csv("https://raw.githubusercontent.com/kishkp/data621-ctg5/master/HW3/crime-evaluation-data.csv")

#str(city_crime_test)




smp_size <- floor(0.80 * nrow(city_crime_train_full))

## set the seed to make your partition reproductible
set.seed(123)

train_ind <- sample(seq_len(nrow(city_crime_train_full)), size = smp_size)

city_crime_train<- city_crime_train_full[train_ind, ]
train_test <- city_crime_train_full[-train_ind, ]

ds_stats <- psych::describe(city_crime_train, skew = TRUE, na.rm = TRUE)
#ds_stats
kable(ds_stats[1:7], caption= "Data Summary")
kable(ds_stats[8:13], caption= "Data Summary (Cont)")

fun1 <- function(a, y) cor(y, a)
x<-city_crime_train[,]
Correlation <- sapply(x, FUN = fun1, y=city_crime_train$target) 

#kable(data.frame(Correlation), caption = "Correlation between target and predictor variable")
Correlation <- sort(Correlation, decreasing = TRUE)
kable(Correlation, caption = "Variable Correlation")

missings<- sapply(city_crime_train_full,function(x) sum(is.na(x)))
kable(missings, caption = "Missing Values")

#missmap(city_crime_train_full, main = "Missing values vs observed")

### finding unique values


### % break up of target variable 

#prop.table(table(city_crime_train_full$target))

uniques<- sapply(city_crime_train_full, function(x) length(unique(x)))
kable(uniques, caption = "Unique Values")


library(ggplot2)
library(reshape2)
#
mdata<- select(city_crime_train, -(target))
mdata2 <- melt(mdata)
# Output the boxplot
p <- ggplot(data = mdata2, aes(x=variable, y=value)) + 
  geom_boxplot() + ggtitle("Outliers identification")
p + facet_wrap( ~ variable, scales="free", ncol=5)

library(popbio)
par(mfrow=c(2,3))
logi.hist.plot(x$zn,x$target,logi.mod = 1, type="hist", boxp=FALSE,col="gray", mainlabel = "zn")
logi.hist.plot(x$indus, x$target,logi.mod = 1, type="hist",boxp=FALSE,col="gray", mainlabel = "indus")
logi.hist.plot(x$chas,x$target,logi.mod = 1,boxp=FALSE,type="hist",col="gray", mainlabel = "chas")
logi.hist.plot(x$nox, x$target,logi.mod = 1,type="hist",boxp=FALSE,col="gray", mainlabel = "nox")
logi.hist.plot(x$rm,x$target,logi.mod = 1,boxp=FALSE,type="hist",col="gray", mainlabel = "rm")
logi.hist.plot(x$age, x$target,logi.mod = 1,type="hist",boxp=FALSE,col="gray", mainlabel = "age")
logi.hist.plot(x$dis,x$target,logi.mod = 1,boxp=FALSE,type="hist",col="gray", mainlabel = "dis")
logi.hist.plot(x$rad, x$target,logi.mod = 1,type="hist",boxp=FALSE,col="gray", mainlabel = "rad")
logi.hist.plot(x$tax,x$target,logi.mod = 1,boxp=FALSE,type="hist",col="gray", mainlabel = "tax")
logi.hist.plot(x$ptratio, x$target,logi.mod = 1,type="hist",boxp=FALSE,col="gray", mainlabel = "ptratio")
logi.hist.plot(x$black,x$target,logi.mod = 1,boxp=FALSE,type="hist",col="gray", mainlabel = "black")
logi.hist.plot(x$lstat, x$target,logi.mod = 1,type="hist",boxp=FALSE,col="gray", mainlabel = "lstat")
logi.hist.plot(x$medv, x$target,logi.mod = 1,type="hist",boxp=FALSE,col="gray", mainlabel = "medv")

# function for removing outliers - http://r-statistics.co/Outlier-Treatment-With-R.html

treat_outliers <- function(x) {
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
 
return(x)
}

city_crime_train_mod<-city_crime_train
train_test_mod<-train_test

city_crime_train_mod$tax_new <- treat_outliers(city_crime_train$tax)
city_crime_train_mod$medv_new <- treat_outliers(city_crime_train$medv)
city_crime_train_mod$lstat_new <- treat_outliers(city_crime_train$lstat)


train_test_mod$tax_new <- treat_outliers(train_test$tax)
train_test_mod$medv_new <- treat_outliers(train_test$medv)
train_test_mod$lstat_new <- treat_outliers(train_test$lstat)


par(mfrow=c(1,3))

boxplot(city_crime_train_mod$tax_new,main="tax_new")
boxplot(city_crime_train_mod$medv_new,main="medv_new")
boxplot(city_crime_train_mod$lstat_new,main="lstat_new")


city_crime_train_mod$rm_new<-sin(city_crime_train$rm)

city_crime_train_mod$dis_new<-sin(city_crime_train$dis)

train_test_mod$rm_new<-sin(train_test$rm)

train_test_mod$dis_new<-sin(train_test$dis)


par(mfrow=c(1,2))

#boxplot(city_crime_train_mod$rm_new,main="rm_new")
#boxplot(city_crime_train_mod$dis_new,main="dis_new")



show_charts <- function(x, ...) {
    
    par(mfrow=c(2,3))
    
    xlabel <- unlist(str_split(deparse(substitute(x)), pattern = "\\$"))[2]
    ylabel <- unlist(str_split(deparse(substitute(y)), pattern = "\\$"))[2]
    
    hist(x,main=xlabel)
    boxplot(x,main=xlabel)

    y<-log(x)
    boxplot(y,main='log transform')
    y<-sqrt(x)
    boxplot(y,main='sqrt transform')
    y<-sin(x)
    boxplot(y,main='sin transform')
    y<-(x)^(1/9)
    boxplot(y,main='ninth transform')
   
}

# attach file to R so that only variable can be called

attach(city_crime_train)

show_charts(zn)


show_charts(indus)


show_charts(nox)


show_charts(rm)



show_charts(age)

show_charts(dis)


show_charts(rad)



show_charts(tax)



show_charts(ptratio)


show_charts(black)


show_charts(lstat)


show_charts(medv)


city_crime_train_mod$zn_new<-city_crime_train_mod$zn

city_crime_train_mod$zn_new [city_crime_train_mod$zn_new > 0] <- 1
city_crime_train_mod$zn_new [city_crime_train_mod$zn_new== 0] <- 0
#city_crime_train_mod$zn_new<-factor(city_crime_train_mod$zn_new)



train_test_mod$zn_new<-train_test$zn

train_test_mod$zn_new [train_test_mod$zn_new > 0] <- 1
train_test_mod$zn_new [train_test_mod$zn_new== 0] <- 0
#city_crime_train_mod$zn_new<-factor(train_test_mod$zn_new)

#city_crime_train$chas <-factor(city_crime_train$chas)

#city_crime_train$target<-factor(city_crime_train$target)

#city_crime_train_mod$chas <-factor(city_crime_train_mod$chas)

#city_crime_train_mod$target<-factor(city_crime_train_mod$target)





#Correlation <- cor(city_crime_train$rm_new,(as.numeric(city_crime_train$target)))

#Correlation <- cor(city_crime_train$dis_new,(as.numeric(city_crime_train$target)))




model_var <- read.csv("https://raw.githubusercontent.com/kishkp/data621-ctg5/master/HW3/model_var.csv")

kable(model_var,caption="Variables used in different models")


model1 <- glm(target ~ ., data = city_crime_train, family = "binomial")

summary(model1)


stepmodel1<- step(model1, direction="backward")


summary(stepmodel1)


model2 <- glm(target ~ .-zn-tax-lstat-medv, data = city_crime_train_mod, family = "binomial")
summary(model2)


stepmodel2<- step(model2, direction="backward")


summary(stepmodel2)



model3=lda(target~.,data=city_crime_train)

model3




model3_mod=lda(target~.-zn-rm-dis-tax-lstat-medv,data=city_crime_train_mod)

model3_mod


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


#confusion matrix
pre_train1 <- predict(model1, newdata=train_test, type="response")
df_pre_train1<-as.data.frame(table(pre_train1>0.5,train_test$target))
df_pre_train1$metrics <- paste(df_pre_train1$Var1,df_pre_train1$Var2, sep = '_') 

#Eval(df_pre_train1)

model_comparison<-data.frame(Accuracy=c(0),Error_Rate=c(0),Precision=c(0),sensitivity=c(0),specificity=c(0),F1_Score=c(0))
model_comparison[1,]<-Eval(df_pre_train1)
 #model_comparison<-c(1)
 
 #row.names(model_comparison[1,])<-"model 1"

 #AUC 

train_test$pre_train1<-c(pre_train1)
#x<-data.frame(auc(train_test$target, train_test$pre_train1))
model_comparison$AUC<-c(0)
model_comparison[1,c("AUC")]<-c(auc(train_test$target, train_test$pre_train1))

kable(model_comparison,row.names = TRUE, caption = " Model 1 evaluation KPIs")

#model_comparison

#confusion matrix

pre_train1_step<-predict(stepmodel1,type="response",newdata=train_test)
df_pre_train1_step<-as.data.frame(table(pre_train1_step>0.5,train_test$target))
df_pre_train1_step$metrics <- paste(df_pre_train1_step$Var1,df_pre_train1_step$Var2, sep = '_') 
#Eval(df_pre_train1_step)
model_comparison[2,]<-Eval(df_pre_train1_step)

#AUC

train_test$pre_train1<-c(pre_train1_step)
model_comparison[2,c("AUC")]<-c(auc(train_test$target, train_test$pre_train1))

kable(model_comparison[2,],caption = "Model 2 evaluation KPIs")


#confusion matrix

pre_train2<-predict(model2,type="response",newdata=train_test_mod)

df_pre_train2<-as.data.frame(table(pre_train2>0.5,train_test_mod$target))
df_pre_train2$metrics <- paste(df_pre_train2$Var1,df_pre_train2$Var2, sep = '_') 
#Eval(df_pre_train2)
model_comparison[3,]<-Eval(df_pre_train2)

#AUC

train_test_mod$pre_train1<-c(pre_train2)
model_comparison[3,c("AUC")]<-c(auc(train_test_mod$target, train_test_mod$pre_train1))

kable(model_comparison[3,],caption = " Model 3 evaluation KPIs")



#confusion matrix


pre_train2_step<-predict(stepmodel2,type="response",newdata=train_test_mod)

df_pre_train2_step<-as.data.frame(table(pre_train2_step>0.5,train_test_mod$target))
df_pre_train2_step$metrics <- paste(df_pre_train2_step$Var1,df_pre_train2_step$Var2, sep = '_') 
#Eval(df_pre_train2_step)

model_comparison[4,]<-Eval(df_pre_train2_step)

#AUC

train_test_mod$pre_train1<-c(pre_train2_step)
model_comparison[4,c("AUC")]<-c(auc(train_test_mod$target, train_test_mod$pre_train1))
kable(model_comparison[4,],caption = " Model 4 evaluation KPIs")



#confusion matrix


pre_train3<-data.frame(predict(model3,type="response",newdata=train_test))

df_pre_train3<-as.data.frame(table(pre_train3$class,train_test$target))
df_pre_train3$metrics <- paste(df_pre_train3$Var1,df_pre_train3$Var2, sep = '_') 

df_pre_train3$metrics[df_pre_train3$metrics=="0_0"]<-"FALSE_0"
df_pre_train3$metrics[df_pre_train3$metrics=="1_0"]<-"TRUE_0"
df_pre_train3$metrics[df_pre_train3$metrics=="0_1"]<-"FALSE_1"
df_pre_train3$metrics[df_pre_train3$metrics=="1_1"]<-"TRUE_1"


#Eval(df_pre_train3)

model_comparison[5,]<-Eval(df_pre_train3)

#AUC
train_test$pre_train1<-c(pre_train3$posterior.1)
model_comparison[5,c("AUC")]<-c(auc(train_test$target, train_test$pre_train1))
kable(model_comparison[5,],caption = " Model 5 evaluation KPIs")


#confusion matrix


pre_train3_mod<-data.frame(predict(model3_mod,type="response",newdata=train_test_mod))

df_pre_train3_mod<-data.frame(table(pre_train3_mod$class,train_test_mod$target))
df_pre_train3_mod$metrics <- paste(df_pre_train3_mod$Var1,df_pre_train3_mod$Var2, sep = '_') 

df_pre_train3_mod$metrics[df_pre_train3_mod$metrics=="0_0"]<-"FALSE_0"
df_pre_train3_mod$metrics[df_pre_train3_mod$metrics=="1_0"]<-"TRUE_0"
df_pre_train3_mod$metrics[df_pre_train3_mod$metrics=="0_1"]<-"FALSE_1"
df_pre_train3_mod$metrics[df_pre_train3_mod$metrics=="1_1"]<-"TRUE_1"

#Eval(df_pre_train3_mod)

model_comparison[6,]<-Eval(df_pre_train3_mod)

#AUC

train_test_mod$pre_train1<-c(pre_train3_mod$posterior.1)
model_comparison[6,c("AUC")]<-c(auc(train_test_mod$target, train_test_mod$pre_train1))


kable(model_comparison[6,],caption = " Model 6 evaluation KPIs")

model_comparison$Model_No<-c(1:6)
kable(model_comparison[,c("Model_No","Accuracy","Error_Rate","AUC","Precision","sensitivity","specificity","F1_Score")],
      caption = "Model Performance Metrics Comparison")


summary(model1)


exp(cbind(OR = coef(model1), confint.default(model1)))


plot (city_crime_train$nox,city_crime_train$target, main="nox vs target")
abline(v=0.425)
abline(v=0.635)

#AUC
pred <- prediction(pre_train1, train_test$target)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))

plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pre_train1 >= threshold & df$target == 1, "TP", v)
  v <- ifelse(df$pre_train1 >= threshold & df$target == 0, "FP", v)
  v <- ifelse(df$pre_train1 < threshold & df$target == 1, "FN", v)
  v <- ifelse(df$pre_train1 < threshold & df$target == 0, "TN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=target, y=pre_train1)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}

train_test$pre_train1 <- predict(model1, newdata=train_test, type="response")

plot_pred_type_distribution (train_test,0.5)

#city_crime_test$chas<-factor(city_crime_test$chas)

Predict_final<-predict(model1,type="response", newdata=city_crime_test)

kable(data.frame(table(Predict_final>0.5)),,caption="Outcome on evaluation data set")

#code=readLines(knitr::purl('https://raw.githubusercontent.com/kishkp/data621-ctg5/master/HW1/HW3_Final_03.Rmd', #documentation = 0)), eval = FALSE}
