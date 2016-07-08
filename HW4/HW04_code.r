```{r, echo = FALSE, warning=FALSE, message=FALSE}
if (!require("ggplot2",character.only = TRUE)) (install.packages("ggplot2",dep=TRUE))
if (!require("MASS",character.only = TRUE)) (install.packages("MASS",dep=TRUE))
if (!require("knitr",character.only = TRUE)) (install.packages("knitr",dep=TRUE))
if (!require("xtable",character.only = TRUE)) (install.packages("xtable",dep=TRUE))
if (!require("dplyr",character.only = TRUE)) (install.packages("dplyr",dep=TRUE))
if (!require("psych",character.only = TRUE)) (install.packages("psych",dep=TRUE))
if (!require("stringr",character.only = TRUE)) (install.packages("stringr",dep=TRUE))
if (!require("car",character.only = TRUE)) (install.packages("car",dep=TRUE))
if (!require("faraway",character.only = TRUE)) (install.packages("faraway",dep=TRUE))
if (!require("dummy",character.only = TRUE)) (install.packages("dummy",dep=TRUE))
if (!require("reshape2",character.only = TRUE)) (install.packages("reshape2",dep=TRUE))
if (!require("popbio",character.only = TRUE)) (install.packages("popbio",dep=TRUE))
if (!require("rpart",character.only = TRUE)) (install.packages("rpart",dep=TRUE))


library(ggplot2)
library(MASS)
library(knitr)
library(xtable)
library(dplyr)
library(psych)
library(stringr)
library(car)
library(faraway)
library(dummy)
library(reshape2)
library(popbio)
library(rpart)

insure_train_full <- read.csv("https://raw.githubusercontent.com/kishkp/data621-ctg5/master/HW4/insurance_training_data.csv")

kable(read.csv("https://raw.githubusercontent.com/kishkp/data621-ctg5/master/HW4/insurevars.csv"), caption = "Variable Description")

str(insure_train_full)

levels(insure_train_full$MSTATUS)
levels(insure_train_full$SEX)
levels(insure_train_full$EDUCATION)
levels(insure_train_full$JOB)
levels(insure_train_full$CAR_TYPE)
levels(insure_train_full$URBANICITY)
levels(insure_train_full$REVOKED)


summary(insure_train_full)

#- some numeric variables like INCOME, HOME_VAL, BLUEBOOK, OLDCLAIM have been converted to Factor variables. This needs to be set right.

insure_train_full$INCOME <- as.numeric(str_replace_all(insure_train_full$INCOME, pattern =  "[\\$*,]", replacement = ""))
insure_train_full$HOME_VAL <- as.numeric(str_replace_all(insure_train_full$HOME_VAL, pattern =  "[\\$*,]", replacement = ""))
insure_train_full$BLUEBOOK <- as.numeric(str_replace_all(insure_train_full$BLUEBOOK, pattern =  "[\\$*,]", replacement = ""))
insure_train_full$OLDCLAIM <- as.numeric(str_replace_all(insure_train_full$OLDCLAIM, pattern =  "[\\$*,]", replacement = ""))

#head(insure_train_full[, c(8,27,10,28, 17,29, 21, 30)], 20)


#- Some of the variables like MSTATUS, SEX, EDUCATION, JOB, CAR_TYPE, URBANICITY have some of the values encoded with "z_". Not that this will impact the analysis, but it will look a bit odd. So we will be fixing this.

#- EDUCATION has 2 "High School" values - one starting with "<" and another starting with "z_". It is assumed that both these values are to be converted to "HIGH School".

#- JOB has a "" value. This needs to be replaced with Unknown.

#- There are records where CAR_AGE is negative which is not possible. Upon investigation, we find that this is a single record that have the negative value. We will remove this record.

insure_train_full$MSTATUS <- as.factor(str_replace_all(insure_train_full$MSTATUS, "z_", ""))
insure_train_full$SEX <- as.factor(str_replace_all(insure_train_full$SEX, "z_", ""))
insure_train_full$EDUCATION <- as.factor(str_replace_all(insure_train_full$EDUCATION, "z_", ""))
insure_train_full$EDUCATION <- as.factor(str_replace_all(insure_train_full$EDUCATION, "<", ""))
insure_train_full$CAR_TYPE <- as.factor(str_replace_all(insure_train_full$CAR_TYPE, "z_", ""))
insure_train_full$URBANICITY <- as.factor(str_replace_all(insure_train_full$URBANICITY, "z_", ""))


insure_train_full$JOB <- as.character(insure_train_full$JOB)
insure_train_full$JOB[insure_train_full$JOB==""] <- "Unknown"
insure_train_full$JOB <- as.factor(str_replace_all(insure_train_full$JOB, "z_", ""))

insure_train_full <- insure_train_full[ -which( insure_train_full$CAR_AGE == -3 | insure_train_full$CAR_AGE == 0 ) , ]


#- We will also create dummy variables for all the factors and drop the original variables. 

dummy_vars<-as.data.frame(sapply(dummy(insure_train_full), FUN = as.numeric))
dummy_vars <- dummy_vars-1


# EDU_ = factor(insure_train_full$EDUCATION)
# dummies = model.matrix(~EDU_)

# insure_train_full <- cbind(select(insure_train_full, -PARENT1, -MSTATUS, -SEX, -EDUCATION, -JOB, -CAR_USE, -CAR_TYPE, -RED_CAR, -REVOKED, -URBANICITY), dummy_vars)

insure_train_full <- cbind(insure_train_full, dummy_vars)

# - Please note that we will not be using INDEX variable as it serves as just an identifier for each row. And has no relationships to other variables.   

insure_train_full <- select(insure_train_full, -INDEX)

insure_orig <- insure_train_full

ds <- select(insure_train_full, -PARENT1, -MSTATUS, -SEX, -EDUCATION, -JOB, -CAR_USE, -CAR_TYPE, -RED_CAR, -REVOKED, -URBANICITY, -TARGET_AMT)

ds_stats <- psych::describe(ds, skew = TRUE, na.rm = TRUE)
#ds_stats
kable(ds_stats[1:7], caption= "Data Summary")
kable(ds_stats[8:13], caption= "Data Summary (Cont)")


fun1 <- function(a, y) cor(y, a , use = 'na.or.complete')
Correlation_TARGET_FLAG <- sapply(ds, FUN = fun1, y=ds$TARGET_FLAG) 

Correlation_TARGET_FLAG <- sort(Correlation_TARGET_FLAG, decreasing = TRUE)
kable(data.frame(Correlation_TARGET_FLAG), caption = "Correlation between TARGET_FLAG and predictor variables")


show_hist <- function(var) {
    
    col_x <- which(colnames(insure_train_full)==var)
    h0 <- select(insure_train_full[insure_train_full$TARGET_FLAG==1,], col_x)
    h1 <- select(insure_train_full[insure_train_full$TARGET_FLAG==0,], col_x)

    min_x <- min(select(insure_train_full, col_x), na.rm = TRUE) 
    max_x <- max(select(insure_train_full, col_x), na.rm = TRUE) 
    by_x <- (max_x - min_x) / 20
    
        
    hist(h0[,1], breaks = 20, col=rgb(1,0,0,0.5), main="Overlapping Histogram", xlab = var, xaxt = "n") 
    axis(1, at = seq(min_x, max_x, by = by_x), las=2)

    hist(h1[,1], breaks = 20, col=rgb(0,0,1,0.5), add=T) # 
#    axis(1, at = seq(min_x, max_x, by = by_x), las=2)

    box()
}

check_bins <- function(var, thresholds) {

    col_x <- which(colnames(insure_train_full)==var)
    old_x <- select(insure_train_full, col_x)
    cor_old <- cor(old_x, insure_train_full$TARGET_FLAG,use = 'na.or.complete')
    ds <- data.frame("Item" = "Original", "Correlation"= round(cor_old, 5))


    
    for(i in 1:length(thresholds)) {
        New_x <- ifelse(select(insure_train_full, col_x)<=thresholds[i],0,1)
        cor_new <- cor(New_x, insure_train_full$TARGET_FLAG,use = 'na.or.complete')
        ds_1 <- data.frame("Item" = as.character(thresholds[i]), "Correlation"= round(cor_new, 5))
        ds <- rbind(ds, ds_1)
    }
    return (ds)
}

show_hist("INCOME")
check_bins("INCOME", c(0, 20000, 90000, 130000))

show_hist("YOJ")
check_bins("YOJ", c(0, 4, 8, 15))

show_hist("HOME_VAL")
check_bins("HOME_VAL", c(0, 20000, 90000, 130000))

show_hist("OLDCLAIM")
check_bins("OLDCLAIM", c(0, 5000, 10000, 15000, 20000, 40000))

show_hist("CLM_FREQ")
check_bins("CLM_FREQ", c(0, 1, 2, 3, 4))

table(insure_train_full$MVR_PTS)
show_hist("MVR_PTS")
check_bins("MVR_PTS", c(0:12))

#table(insure_train_full$CAR_AGE)
show_hist("CAR_AGE")
#check_bins("CAR_AGE", c(1:27))

#table(insure_train_full$AGE)
show_hist("AGE")
#check_bins("AGE", c(16:80))

#table(insure_train_full$BLUEBOOK)
show_hist("BLUEBOOK")
#check_bins("BLUEBOOK", c(11000, 41000, 41050, 57500, 58000))

table(insure_train_full$TIF)
show_hist("TIF")
check_bins("TIF", c(1, 4, 6, 10, 24))

table(insure_train_full$TRAVTIME)
show_hist("TRAVTIME")
check_bins("TRAVTIME", c(21, 59, 120))

missings<- sapply(insure_train_full,function(x) sum(is.na(x)))
kable(data.frame(missings), caption = "Missing Values")


par(mfrow=c(2,3))
hist(insure_train_full$AGE)
hist(insure_train_full$YOJ)
hist(insure_train_full$INCOME)
hist(insure_train_full$HOME_VAL)
hist(insure_train_full$CAR_AGE)
#
mdata<- select(insure_train_full, AGE, BLUEBOOK, TIF)
mdata2 <- melt(mdata)
# Output the boxplot
p <- ggplot(data = mdata2, aes(x=variable, y=value)) + 
  geom_boxplot() + ggtitle("Outliers Identification")
p + facet_wrap( ~ variable, scales="free", ncol=5)


par(mfrow=c(2,3))

# #fun1 <- function(a, y) cor(y, a , use = 'na.or.complete')
# #Correlation_TARGET_FLAG <- sapply(x, FUN = fun1, y=insure_train_full$TARGET_FLAG) 
# 
# show_chart_logi.hist <- function(a, y, ...) {
# #    xlabel <- unlist(str_split(deparse(substitute(a)), pattern = "\\$"))[2]
#     xlabel <- deparse(substitute(a))
#     message(xlabel)
#     logi.hist.plot(a,y,logi.mod = 1, type="hist", boxp=FALSE,col="gray", mainlabel = xlabel)
# }

x <- select(insure_train_full, -TARGET_AMT)
x <- x[complete.cases(x),]

# sapply(x, FUN = show_chart_logi.hist, y=x$TARGET_FLAG) 

logi.hist.plot(x$REVOKED_Yes,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'REVOKED_Yes')
logi.hist.plot(x$CAR_USE_Private,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'CAR_USE_Private')
logi.hist.plot(x$CAR_TYPE_SUV,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'CAR_TYPE_SUV')
logi.hist.plot(x$PARENT1_Yes,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'PARENT1_Yes')
logi.hist.plot(x$KIDSDRIV,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'KIDSDRIV')
logi.hist.plot(x$CAR_AGE,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'CAR_AGE')
logi.hist.plot(x$JOB_Clerical,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'JOB_Clerical')
logi.hist.plot(x$HOMEKIDS,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'HOMEKIDS')
logi.hist.plot(x$JOB_Doctor,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'JOB_Doctor')
logi.hist.plot(x$CLM_FREQ,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'CLM_FREQ')
logi.hist.plot(x$SEX_F,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'SEX_F')
logi.hist.plot(x$URBANICITY_Highly.Rural..Rural,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'URBANICITY_Highly.Rural..Rural')
logi.hist.plot(x$MVR_PTS,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'MVR_PTS')
logi.hist.plot(x$EDUCATION_Masters,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'EDUCATION_Masters')
logi.hist.plot(x$CAR_TYPE_Van,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'CAR_TYPE_Van')
logi.hist.plot(x$CAR_TYPE_Minivan,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'CAR_TYPE_Minivan')
logi.hist.plot(x$YOJ,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'YOJ')
logi.hist.plot(x$TIF,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'TIF')
logi.hist.plot(x$MSTATUS_Yes,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'MSTATUS_Yes')
logi.hist.plot(x$RED_CAR_no,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'RED_CAR_no')
logi.hist.plot(x$JOB_Lawyer,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'JOB_Lawyer')
logi.hist.plot(x$CAR_TYPE_Pickup,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'CAR_TYPE_Pickup')
logi.hist.plot(x$JOB_Student,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'JOB_Student')
logi.hist.plot(x$OLDCLAIM,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'OLDCLAIM')
logi.hist.plot(x$INCOME,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'INCOME')
logi.hist.plot(x$EDUCATION_Bachelors,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'EDUCATION_Bachelors')
logi.hist.plot(x$JOB_Manager,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'JOB_Manager')
logi.hist.plot(x$EDUCATION_High.School,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'EDUCATION_High.School')
logi.hist.plot(x$RED_CAR_yes,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'RED_CAR_yes')
logi.hist.plot(x$MSTATUS_No,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'MSTATUS_No')
logi.hist.plot(x$JOB_Home.Maker,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'JOB_Home.Maker')
logi.hist.plot(x$EDUCATION_PhD,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'EDUCATION_PhD')
logi.hist.plot(x$TRAVTIME,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'TRAVTIME')
logi.hist.plot(x$JOB_Professional,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'JOB_Professional')
logi.hist.plot(x$URBANICITY_Highly.Urban..Urban,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'URBANICITY_Highly.Urban..Urban')
logi.hist.plot(x$SEX_M,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'SEX_M')
logi.hist.plot(x$JOB_Blue.Collar,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'JOB_Blue.Collar')
logi.hist.plot(x$AGE,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'AGE')
logi.hist.plot(x$CAR_TYPE_Sports.Car,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'CAR_TYPE_Sports.Car')
logi.hist.plot(x$HOME_VAL,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'HOME_VAL')
logi.hist.plot(x$BLUEBOOK,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'BLUEBOOK')
logi.hist.plot(x$PARENT1_No,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'PARENT1_No')
logi.hist.plot(x$CAR_USE_Commercial,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'CAR_USE_Commercial')
logi.hist.plot(x$REVOKED_No,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'REVOKED_No')
logi.hist.plot(x$CAR_TYPE_Panel.Truck,x$TARGET_FLAG,logi.mod = 1, type='hist', boxp=FALSE,col='gray', mainlabel = 'CAR_TYPE_Panel.Truck')

show_charts <- function(x, varlab, ...) {
    xlabel <- varlab
    xlab_log <- paste0(xlabel, '_log')
    xlab_sqrt <- paste0(xlabel, '_sqrt')
    xlab_sin <- paste0(xlabel, '_sin')
    xlab_inv <- paste0(xlabel, '_inv')
    
    mdata <- cbind(x, log(x), sqrt(x), sin(x), 1/x)
    colnames(mdata) <- c(xlabel, xlab_log, xlab_sqrt, xlab_sin, xlab_inv)
    mdata2 <- melt(mdata)
    mdata2 <- mdata2[, c(2:3)]
    names(mdata2) <- c("variable", "value")
    
    # Output the boxplot
    p <- ggplot(data = mdata2, aes(x=variable, y=value)) + geom_boxplot() + ggtitle("Outliers identification")
    p + facet_wrap( ~ variable, scales="free", ncol=5)
}

summary(insure_train_full$TIF)
show_charts(insure_train_full$TIF, 'TIF')
insure_train_full$TIF_sin <- sin(insure_train_full$TIF)

summary(insure_train_full$BLUEBOOK)
show_charts(insure_train_full$BLUEBOOK, 'BLUEBOOK')
insure_train_full$BLUEBOOK_sin <- sin(insure_train_full$BLUEBOOK)

summary(insure_train_full$AGE)
show_charts(insure_train_full$AGE, 'AGE')
insure_train_full$AGE_sin <- sin(insure_train_full$AGE)

insure_train_full$AGE[is.na(insure_train_full$AGE)] <- mean(insure_train_full$AGE, na.rm = T) 
insure_train_full$YOJ[is.na(insure_train_full$YOJ)] <- mean(insure_train_full$YOJ, na.rm = T) 

insure_train_full$INCOME[is.na(insure_train_full$INCOME)] <- median(insure_train_full$INCOME, na.rm = T) 
insure_train_full$HOME_VAL[is.na(insure_train_full$HOME_VAL)] <- median(insure_train_full$HOME_VAL, 
na.rm = T) 
insure_train_full$CAR_AGE[is.na(insure_train_full$CAR_AGE)] <- median(insure_train_full$CAR_AGE, na.rm = 
T) 

# insure_train_full <- insure_train_full[complete.cases(insure_train_full),]
# insure_train_crash <- insure_train_crash[complete.cases(insure_train_crash),]


insure_train_full$CAR_TYPE_FLAG_BIN <- ifelse(insure_train_full$CAR_TYPE_Minivan | insure_train_full$CAR_TYPE_Panel.Truck, 1, 0)


insure_train_full$EDUCATION_FLAG_BIN <- ifelse(insure_train_full$EDUCATION_High.School, 0, 1)


insure_train_full$JOB_TYPE_FLAG_BIN <- ifelse(insure_train_full$JOB_Student |  insure_train_full$JOB_Home.Maker | insure_train_full$JOB_Clerical | insure_train_full$JOB_Blue.Collar, 1, 0)


insure_train_full$INCOME_FLAG_BIN <- ifelse(insure_train_full$INCOME <=0, 1, 0)


insure_train_full$YOJ_FLAG_BIN <- ifelse(insure_train_full$YOJ <=0, 1, 0)


insure_train_full$HOME_VAL_FLAG_BIN <- ifelse(insure_train_full$HOME_VAL <=0, 1, 0)

insure_train_full$OLDCLAIM_FLAG_BIN <- ifelse(insure_train_full$OLDCLAIM <=0, 1, 0)

insure_train_full$CLM_FREQ_FLAG_BIN <- ifelse(insure_train_full$CLM_FREQ <=0, 1, 0)

insure_train_full$MVR_PTS_FLAG_BIN <- ifelse(insure_train_full$MVR_PTS <=0, 1, 0)

insure_train_full$CAR_AGE_FLAG_BIN <- ifelse(insure_train_full$CAR_AGE <=1, 1, 0)
```
\
\

- AGE - There is no specific pattern that emerges. We will retain this variable as is.
\
\

- BLUEBOOK - There is no specific pattern that emerges. We will retain this variable as is.
\
\


- TRAVTIME - from the plot, we can see that there is a clear pattern around the value - 20. We will go ahead and create a binned variable for this.
\
\
TRAVTIME_FLAG_BIN : \
\
- 1 : if TRAVTIME <= 20 \
- 0 : if TRAVTIME > 0
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}

```
\
\
There are a few variables that are binary and can be converted to 0s and 1s. The following are the variables: CAR_USE_Commercial, CAR_USE_Private, MSTATUS_No, MSTATUS_Yes, PARENT1_No, PARENT1_Yes, RED_CAR_no, RED_CAR_yes, REVOKED_No, REVOKED_Yes, SEX_F, SEX_M, URBANICITY_Highly.Rural..Rural, URBANICITY_Highly.Urban..Urban. We will create a binary variable for each of these as well.
\
\

```{r, echo = FALSE, warning=FALSE, message=FALSE}

# CAR_USE_Commercial, CAR_USE_Private, 

insure_train_full$CAR_USE_Commercial <- ifelse(insure_train_full$CAR_USE_Commercial==1, 1, 0)

# MSTATUS_No, MSTATUS_Yes, 
insure_train_full$MSTATUS_Yes <- ifelse(insure_train_full$MSTATUS_Yes==1, 1, 0)

# PARENT1_No, PARENT1_Yes, 
insure_train_full$PARENT1_Yes <- ifelse(insure_train_full$PARENT1_Yes==1, 1, 0)

# RED_CAR_no, RED_CAR_yes, 
insure_train_full$RED_CAR_yes <- ifelse(insure_train_full$RED_CAR_yes==1, 1, 0)

# REVOKED_No, REVOKED_Yes, 
insure_train_full$REVOKED_Yes <- ifelse(insure_train_full$REVOKED_Yes==1, 1, 0)

# SEX_F, SEX_M, 
insure_train_full$SEX_M <- ifelse(insure_train_full$SEX_M==1, 1, 0)

# URBANICITY_Highly.Rural..Rural, URBANICITY_Highly.Urban..Urban.
insure_train_full$URBANICITY_Rural <- ifelse(insure_train_full$URBANICITY_Highly.Rural..Rural==1, 1, 0)


```


After having prepared the data, we will go ahead and drop some of the variables.

```{r}

#write.csv(insure_train_full, file = "D:/CUNY/Courses/Business Analytics and Data Mining/Assignments/data621-ctg5/HW4/final.csv")

DS_TARGET_FLAG <- select(insure_train_full, -AGE, -BLUEBOOK, -CAR_TYPE, -CAR_TYPE_Minivan, -CAR_TYPE_Panel.Truck, -CAR_TYPE_Pickup, -CAR_TYPE_Sports.Car, -CAR_TYPE_SUV, -CAR_TYPE_Van, -CAR_USE, -CAR_USE_Private, -CLM_FREQ, -EDUCATION, -EDUCATION_Bachelors, -EDUCATION_High.School, -EDUCATION_Masters, -EDUCATION_PhD, -HOME_VAL, -INCOME, -JOB, -JOB_Blue.Collar, -JOB_Clerical, -JOB_Doctor, -JOB_Home.Maker, -JOB_Lawyer, -JOB_Manager, -JOB_Professional, -JOB_Student, -JOB_Unknown, -MSTATUS, -MSTATUS_No, -MVR_PTS, -OLDCLAIM, -PARENT1, -PARENT1_No, -RED_CAR, -RED_CAR_no, -REVOKED, -REVOKED_No, -SEX, -SEX_F, -TARGET_AMT, -TIF, -TRAVTIME, -URBANICITY, -URBANICITY_Highly.Rural..Rural, -URBANICITY_Highly.Urban..Urban, -YOJ)

str(DS_TARGET_FLAG)

```
\
\


##2.3 Build Models



In this section, we will create 3 models. Aside from using original and transformed data, we will also using different methods and functions such as Linear Discriminant Analysis, step function, and logit function to enhance our models. 
\newline
\newline
Below is our model definition: \
-Model 1- This model will be created using all the variables in train data set with logit function GLM. \
-Model 2- This model will be created using all the variables; however using step function instead of GLM. \
-Model 3: this model will be created using original variables using Linear Discriminant Analysis function lda in ISLR  package.\
\
\


###2.3.1 Prepare TRAIN and VALID datasets


However, prior to that, we hold out a subset of data as a validation dataset to check model performance. This will be useful when we select a model.
\
\

```{r}

smp_size <- floor(0.80 * nrow(DS_TARGET_FLAG))

## set the seed to make your partition reproductible
set.seed(123)

train_index <- sample(seq_len(nrow(DS_TARGET_FLAG)), size = smp_size)

DS_TARGET_FLAG_TRAIN<- DS_TARGET_FLAG[train_index, ]
DS_TARGET_FLAG_VALID <- DS_TARGET_FLAG[-train_index, ]
```


###2.3.2 Model 1


In this model, we will be using all the given variables in train data set.  We will create model using logit function. We will then step thru the model to remove unnecessary variables and generate the refined model. We will highlight the summary of the refined model. \
\
\



```{r, echo = FALSE, warning=FALSE, message=FALSE}

model1 <- glm(TARGET_FLAG ~ ., data = na.omit(DS_TARGET_FLAG_TRAIN), family = "binomial")
summary(model1)

model1_ref<- step(model1, direction="backward")
#summary(model1_ref)


```

##### Interpretation for model 1  \


**NEED TO WORK ON THIS**

$\newline$
(i) Based on the outcome, it can be seen that YOJ_FLAG_BIN, CLM_FREQ_FLAG_BIN, CAR_AGE_FLAG_BIN, AGE_FLAG_BIN, BLUEBOOK_FLAG_BIN, RED_CAR_FLAG_yes and SEX_FLAG_M are not statistically significant. 

(ii)As for the statistically significant variables, nox has the lowest p-value suggesting a strong association of the nox to the target variable. Other important variables are dis, rad, tax, ptratio, and medv. The AIC value for the model1 =168.71.

(iii) The logistic regression coefficients give the change in the log odds of the outcome for a one unit increase in the predictor variables.

  a. For every one unit change in nox, the log odds of crime rate above median value increases by 53.41.\
  b. For a one unit increase in dis, the log odds of crime rate above median value increases by 0.80.\
  c. For a one unit increase in rad, the log odds of crime rate above median value increases by 0.72.\
  d. For a one unit increase in tax, the log odds of crime rate above median value increases by -0.007. Tax has a negative impact on crime rate.\
  e. For a one unit increase in ptratio, the log odds of crime rate above median value increases by 0.44.\
  f. For a one unit increase in medv , the log odds of crime rate above median value increases by 0.23.\
 
 (iv) No. of iterations are 9 before lowest value of AIC was derived for this model.

\
\


###2.3.3 Model 2


In this model, we will be using original variables; however we use the CART (Classification and Regression Trees) algorithm to train the model. We will then look at the summary of this model as well.


```{r, echo = FALSE, warning=FALSE, message=FALSE}

 # grow tree 
 model2 <- rpart(TARGET_FLAG~., data=DS_TARGET_FLAG_TRAIN, method = "class")

 printcp(model2) # display the results 
 plotcp(model2) # visualize cross-validation results 
 summary(model2) # detailed summary of splits

 # plot tree 
 plot(model2, uniform=TRUE, main="Classification Tree for TARGET_FLAG")
 text(model2, use.n=TRUE, all=TRUE, cex=.8)

 # create attractive postscript plot of tree 
 #post(fit, file = "c:/tree.ps", title = "Classification Tree for Kyphosis")

 
model2_ref <- prune(model2, cp = model2$cptable[which.min(model2$cptable[,"xerror"]),"CP"])

 # plot the pruned tree 
 plot(model2_ref, uniform=TRUE, main="Pruned Classification Tree for TARGET_FLAG")
 text(model2_ref, use.n=TRUE, all=TRUE, cex=.8)
 #post(pfit, file = "c:/ptree.ps",    title = "Pruned Classification Tree for Kyphosis") 

```

#####  Interpretation for model 2  \

$\newline$

**NEED TO WORK ON THIS**

(i)It can be seen that zn, age, and black are not statistically significant. 

(ii)As for the statistically significant variables, nox has the lowest p-value suggesting a strong association of the nox of the target variable. other important variables are dis, rad, tax, ptratio, medv, and lstat. 
The AIC value for the model1 =164.85.

(iii) The logistic regression coefficients give the change in the log odds of the outcome for a one unit increase in the predictor variables.

  a. For every one unit change in nox, the log odds of crime rate above median value increases by 46.61.\
  b. For a one unit increase in dis, the log odds of crime rate above median value increases by 0.71.\
  c. For a one unit increase in rad, the log odds of crime rate above median value increases by 0.77.\
  d. For a one unit increase in tax, the log odds of crime rate above median value increases by -0.009.\
  e. For a one unit increase in ptratio, the log odds of crime rate above median value increases by 0.35.\
  f. For a one unit increase in medv , the log odds of crime rate above median value increases by 0.18\

(iv) there were 9 iterations in backward steps before final model was selected

\
\



###2.3.4 Model 3


In this model we will be using original variables; however the Linear Discriminant Analysis function lda in ISLR package.

```{r model with Linear Discriminant Analysis, eval=TRUE,echo=FALSE}


model3=lda(TARGET_FLAG~.,data=DS_TARGET_FLAG_TRAIN)
summary(model3)


```

#####  Interpretation for model 3  \

$\newline$

**NEED TO WORK ON THIS**


(i)The Classification boundary equation for our model 5 is below: \

 $-0.004 * zn + 0.0281 * indus - 0.055 * chas + 7.910 * nox + 0.165 * rm + 0.013 * age + 0.084 * dis + 0.102 * rad - 0.001 * tax + 0.009 * ptratio  - 0.0009 * black + 0.024 * lstat + 0.042 * medv =0$


(ii) From summary table, we also have the prior probability of of success is 0.4731183 and failure is 0.5268817.\

(iii) Group means provides mean values for each variable with respect to target variable values 0 and 1.\

 (iv) This model has accuracy value 82.97 % which is less compare to the other models. LDA model assumes normality of the variables used in the model and theere are some variables which are not normally distributed and have outliers that is impacting the result out of this model.


##2.4 Select Models
###2.4.1 Model Evaluation Using TRAIN Data 
###2.4.2 Model Evaluation Using VALID Data
###2.4.3 Selection Summary



\newpage



#3 Linear Regression for TARGET_AMT\



In this section we will use Linear regression to model the TARGET_AMT. We will first start with the Data Exploration.\
\


##3.1 Data Exploration Analysis\


\

In this analysis, we will be using only those records where the TARGET_FLAG is 1. This indicates that the vehicle crashed. In such a scenario, we will be modeling the cost of repair using Linear Regression. First, lets create the required data set for the "Crashed" data from the existing "clean" full data and look at the structure of the resulting dataset. We will remove from the new "crashed" dataset all those variables that were created specifically for predicting TARGET_FLAG. We will be creating these variables separately for predicting TARGET_AMT.
\
\

```{r, echo = FALSE, warning=FALSE, message=FALSE}

insure_train_crash <- insure_train_full[insure_train_full$TARGET_FLAG==1,]

#write.csv(insure_train_crash, file = "D:/CUNY/Courses/Business Analytics and Data Mining/Assignments/data621-ctg5/HW4/final.csv")

insure_train_crash <- select(insure_train_crash, -AGE_sin, -BLUEBOOK_sin, -CAR_AGE_FLAG_BIN, -CAR_TYPE, -CAR_TYPE_FLAG_BIN, -CAR_USE, -CAR_USE_Private, -CLM_FREQ_FLAG_BIN, -EDUCATION, -EDUCATION_FLAG_BIN, -HOME_VAL_FLAG_BIN, -INCOME_FLAG_BIN, -JOB, -JOB_TYPE_FLAG_BIN, -MSTATUS, -MSTATUS_No, -MVR_PTS_FLAG_BIN, -OLDCLAIM_FLAG_BIN, -PARENT1, -PARENT1_No, -RED_CAR, -RED_CAR_no, -REVOKED, -REVOKED_No, -SEX, -SEX_F, -TARGET_FLAG, -TIF_sin, -URBANICITY, -URBANICITY_Highly.Rural..Rural, -URBANICITY_Highly.Urban..Urban, -YOJ_FLAG_BIN)

str(insure_train_crash)
```
\
\
We notice that the dependent variable here is TARGET_AMT. Apart from the dependent variables, we have 39 independent or predictor variables.\
\
Also, since we created this dataset from the "Clean" full dataset, we already have taken care of the missing values.
\
\
However, we may need to look into the outliers and correlations again since we have a new target variable to correlate against.
\
\


###3.1.1 Data Summary and Correlation Analysis\


\
\
In this section, we will create summary data to better understand the relationship each of the variables have with our dependent variables using correlation, central tendency, and dispersion as shown below:  
\
\

```{r, echo = FALSE, warning=FALSE, message=FALSE, results='hide'}
ds_stats <- psych::describe(insure_train_crash, skew = TRUE, na.rm = TRUE)
#ds_stats
kable(ds_stats[1:7], caption= "Data Summary")
kable(ds_stats[8:13], caption= "Data Summary (Cont)")

fun1 <- function(a, y) cor(y, a , use = 'na.or.complete')
x<-insure_train_crash[,]
Correlation_TARGET_AMT <- sapply(x, FUN = fun1, y=insure_train_crash$TARGET_AMT) 
```
\
\
Now we will produce the correlation table between the independent variables and the dependent variable - TARGET_AMT  
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}
Correlation_TARGET_AMT <- sort(Correlation_TARGET_AMT, decreasing = TRUE)
kable(data.frame(Correlation_TARGET_AMT), caption = "Correlation between TARGET_AMT and predictor variables")

```
\
\

The above table suggests that none of the variables seem to have a very strong correlation with TARGET_AMT. 
\
\
However, BLUEBOOK, CAR_TYPE_Panel.Truck, SEX_M, CAR_TYPE_Van, CAR_USE_Commercial, INCOME, JOB_Professional, JOB_Unknown, MVR_PTS, YOJ, HOME_VAL, EDUCATION_PhD, AGE, RED_CAR_yes, PARENT1_Yes, JOB_Blue.Collar, EDUCATION_Masters, EDUCATION_Bachelors, JOB_Lawyer, TRAVTIME, CLM_FREQ, HOMEKIDS have a positive correlation. 
\
\
Similarly, KIDSDRIV, URBANICITY_Rural, OLDCLAIM, CAR_TYPE_Minivan, TIF, JOB_Doctor, CAR_AGE, JOB_Clerical, CAR_TYPE_Sports.Car, CAR_TYPE_Pickup, JOB_Manager, JOB_Home.Maker, JOB_Student, MSTATUS_Yes, EDUCATION_High.School, REVOKED_Yes, CAR_TYPE_SUV have a negative correlation. 
\
\

Lets now see how values in some of the variable affects the correlation:
\
\

CAR_TYPE - If you drive Vans or Panel Trucks your cost of repair seems to increase as against Minivan, Pickup, Sports.Car, SUV. Since the distiction is clear, we believe that binning this variable accordingly will help strengthen the correlation.
\
\
EDUCATION - If you have only a high school education then your cost of repair is less compared to a Bachelors, Masters or a Phd. Again binning this variable will strengthen the correlation.
\
\
JOB - If you are a Lawyer, Professional, in a Blue Collar job or the job is unknown, you spend more on repairs as compared to a Doctor, Manager, Home Maker, Student, or Clerical job.  Again binning this variable will strengthen the correlation.
\
\

Lets have a look at the following numeric variables that have 0 as one of their values: INCOME, YOJ, HOME_VAL, OLDCLAIM, CLM_FREQ, MVR_PTS, CAR_AGE, AGE, BLUEBOOK, TIF, TRAVTIME. The goal here is to see if we can bin these variables into zero and non-zero bin values and check the correlations. While doing that we will also see how the variables are distributed vis-a-vis TARGET_AMT.
\
\


```{r}

check_bins <- function(var, thresholds) {
    col_x <- which(colnames(insure_train_crash)==var)
    old_x <- select(insure_train_crash, col_x)
    cor_old <- cor(old_x, insure_train_crash$TARGET_AMT,use = 'na.or.complete')
    ds <- data.frame("Item" = "Original", "Correlation"= round(cor_old, 5))

    old_tresh <- 0
    for(i in 1:length(thresholds)) {
        New_x <- ifelse((select(insure_train_crash, col_x) >= old_tresh & select(insure_train_crash, col_x)<=thresholds[i]),0,1)
        
        cor_new <- cor(New_x, insure_train_crash$TARGET_AMT,use = 'na.or.complete')
        
        ds_1 <- data.frame("Item" = as.character(thresholds[i]), "Correlation"= round(cor_new, 5))
        
        ds <- rbind(ds, ds_1)
        old_tresh <- thresholds[i]
    }
    
    return (ds)
}

plot(insure_train_crash$INCOME, insure_train_crash$TARGET_AMT)
#check_bins("INCOME", c(0, 50000, 125000, 200000))

plot(insure_train_crash$YOJ, insure_train_crash$TARGET_AMT)
#check_bins("YOJ", c(0:19))

plot(insure_train_crash$HOME_VAL, insure_train_crash$TARGET_AMT)
#check_bins("HOME_VAL", c(seq(0, 600000, 10000)))

plot(insure_train_crash$OLDCLAIM, insure_train_crash$TARGET_AMT)
hist(insure_train_crash$OLDCLAIM, breaks=50)
#check_bins("OLDCLAIM", c(seq(0, 50000, 1000)))

#show_hist("CLM_FREQ")
plot(insure_train_crash$CLM_FREQ, insure_train_crash$TARGET_AMT)
#check_bins("CLM_FREQ", c(0, 1, 2, 3, 4))

#table(insure_train_full$MVR_PTS)
plot(insure_train_crash$MVR_PTS, insure_train_crash$TARGET_AMT)
#check_bins("MVR_PTS", c(0:12))

table(insure_train_full$CAR_AGE)
#show_hist("CAR_AGE")
plot(insure_train_crash$CAR_AGE, insure_train_crash$TARGET_AMT)
#check_bins("CAR_AGE", c(1:27))

#table(insure_train_full$AGE)
plot(insure_train_crash$AGE, insure_train_crash$TARGET_AMT)
#show_hist("AGE")
#check_bins("AGE", c(16:80))

#table(insure_train_full$BLUEBOOK)
plot(insure_train_crash$BLUEBOOK, insure_train_crash$TARGET_AMT)
#show_hist("BLUEBOOK")
#check_bins("BLUEBOOK", c(5000, 10000, 20000, 30000, 45000, 57500, 58000))

# table(insure_train_full$TIF)
# show_hist("TIF")
plot(insure_train_crash$TIF, insure_train_crash$TARGET_AMT)
# check_bins("TIF", c(1, 4, 6, 10, 24))

#table(insure_train_full$TRAVTIME)
plot(insure_train_crash$TRAVTIME, insure_train_crash$TARGET_AMT)
#show_hist("TRAVTIME")
#check_bins("TRAVTIME", c(21, 59, 120))

```

\
\

From the outputs above, we can come to the following conclusions:
\
\


- INCOME - From the plot we can see that there is a marked difference in the chart at around 125000. We will use this value to bin this variable. 

- YOJ - We can see that from 7 - 17 years, there is a visible change in the TARGET_AMT. We will use this  bound to create the binned variable.
 
- HOME_VAL - We see from the plot 3 distinct segments - Between 0-10000, 60000-400000 and the rest. We will use these values to create 2 bins.

- OLDCLAIM- We can visualize 3 clusters in the data - 0-2000, 2000-10000, > 10000,  We will use these values to create 2 bins.

- CLM_FREQ - Values less than 4 seem to have a positive correlation. We will use this value for binning. 

- MVR_PTS - We can see from the plot that after 2, the TARGET_AMT starts decreasing. We will use this value for binning. 

- CAR_AGE - There are quite a few records with a 1 year car age. We will use this bound to generate a binned variable as well as retain the original varible as is. 

- AGE - There is no specific pattern that emerges in AGE. We will retain the variable as is.

- BLUEBOOK - There is no specific pattern that emerges. We will retain the variable as is.

- TIF - Looking at the plot we can conclude that this is not a good variable for binning. We will retain this variable as is.

- TRAVTIME - from the plot, we can see that there is a clear pattern around the value - 20. We will go ahead and create a binned variable for this.



\
\


We will carry out the above transformations in the Data Preparation phase.
\
\



###3.1.2 Missing Values\


\
\
To reiterate, since we started with a "clean" dataset, the missing values have already been handled. Lets take a quick look to confirm. 
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}
missings<- sapply(insure_train_crash,function(x) sum(is.na(x)))
kable(data.frame(missings), caption = "Missing Values")

```
\
\


###3.1.3 Outliers identification\ 



In this sub-section, we will look at the boxplots and determine the outliers in variables and decide on whether to act on the outliers.\
\
We will do the outliers only on the numeric variables: AGE, BLUEBOOK and TIF. The other variables will be binned and would not beed outlier handling.
\
\
Below are the plots:
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}

# AGE, BLUEBOOK, CAR_AGE, CLM_FREQ, HOME_VAL, HOMEKIDS, INCOME, KIDSDRIV, MVR_PTS, OLDCLAIM, TIF, TRAVTIME, YOJ

mdata<- select(insure_train_crash, AGE, BLUEBOOK, TIF)
mdata2 <- melt(mdata)
# Output the boxplot
p <- ggplot(data = mdata2, aes(x=variable, y=value)) + 
  geom_boxplot() + ggtitle("Outliers identification")
p + facet_wrap( ~ variable, scales="free", ncol=5)

```

From the "Outliers identification" plot above, we see that we have few outliers that we need to treat. \
\
We see thatall the 3 variables need to be treated when we do the data preparation for modeling the TARGET_AMT. 
\
\
\



##3.2 Data Preparation\ 


\
Now that we have completed the data exploration / analysis, we will be transforming the data for use in analysis and modeling. \
\
\
We will be following the below steps as guidelines: \
- Outliers treatment \
- Missing values treatment \
- Adding New Variables \
\



###3.2.1 Outliers treatment\


In this sub-section, we will check different transformations for each of the variables - AGE, BLUEBOOK, TIF - and create the appropriate outlier-handled / transformed variables.  
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}
show_charts <- function(x, varlab, ...) {
    xlabel <- varlab
    xlab_log <- paste0(xlabel, '_log')
    xlab_sqrt <- paste0(xlabel, '_sqrt')
    xlab_sin <- paste0(xlabel, '_sin')
    xlab_inv <- paste0(xlabel, '_inv')
    
    mdata <- cbind(x, log(x), sqrt(x), sin(x), 1/x)
    colnames(mdata) <- c(xlabel, xlab_log, xlab_sqrt, xlab_sin, xlab_inv)
    mdata2 <- melt(mdata)
    mdata2 <- mdata2[, c(2:3)]
    names(mdata2) <- c("variable", "value")
    
    # Output the boxplot
    p <- ggplot(data = mdata2, aes(x=variable, y=value)) + geom_boxplot() + ggtitle("Outliers identification")
    p + facet_wrap( ~ variable, scales="free", ncol=5)
}

```
\
\



**Transformations for AGE**


```{r, echo = FALSE, warning=FALSE, message=FALSE}
#KIDSDRIV, AGE, CAR_AGE, MVR_PTS, TIF, TRAVTIME and YOJ
show_charts(insure_train_crash$AGE, 'AGE')
insure_train_crash$AGE_sin <- sin(insure_train_crash$AGE)
```
\
\
From the above charts we can see that a sin transformation works well for AGE. We will create this variable.
\
\


**Transformations for TIF**


```{r, echo = FALSE, warning=FALSE, message=FALSE}
show_charts(insure_train_crash$TIF, 'TIF')
insure_train_crash$TIF_sin <- sin(insure_train_crash$TIF)
```
\
\

From the above charts we can see that a log, sqrt, sin or an inverse transformation works well for TIF. However, a sin transformation seems to be more appropriate as it is well centered. Hence, We will create these variables.
\
\

**Transformations for BLUEBOOK**


```{r, echo = FALSE, warning=FALSE, message=FALSE}
show_charts(insure_train_crash$BLUEBOOK, 'BLUEBOOK')
insure_train_crash$BLUEBOOK_sin <- sin(insure_train_crash$BLUEBOOK)
```
\
\

From the above charts we can see that a sin transformation works well for BLUEBOOK. We will create these variables.
\
\



###3.2.2 Adding New Variables\


\
In this section, we generate some additional variables that we feel will help the correlations. The following were some of the observations we made during the data exploration phase for TARGET_AMT.
\

The following were some of the observations we made during the data exploration phase for TARGET_AMT

\
\

CAR_TYPE - If you drive Vans or Panel Trucks your cost of repair seems to increase as against Minivan, Pickup, Sports.Car, SUV. Since the distiction is clear, we believe that binning this variable accordingly will help strengthen the correlation.

\
\
Accordingly, we will bin these variables as below:
\
CAR_TYPE_AMT_BIN : \
\
- 1 : if CAR_TYPE is Vans or Panel Trucks \
- 0 : if CAR_TYPE is Pickups, Sports, SUVs or Minivans
\
\

```{r}

insure_train_crash$CAR_TYPE_AMT_BIN <- ifelse(insure_train_crash$CAR_TYPE_Van | insure_train_crash$CAR_TYPE_Panel.Truck, 1, 0)

```

EDUCATION - If you have only a high school education then your cost of repair is less compared to a Bachelors, Masters or a Phd. Again binning this variable will strengthen the correlation.
\
\

Accordingly, we will bin these variables as below:
\
EDUCATION_AMT_BIN : \
\
- 1 : if EDUCATION is High School \
- 0 : if EDUCATION is Bachelors, Masters or Phd
\
\


```{r}

insure_train_crash$EDUCATION_AMT_BIN <- ifelse(insure_train_crash$EDUCATION_High.School, 1, 0)

```

JOB - If you are a Lawyer, Professional, in a Blue Collar job or the job is unknown, you spend more on repairs as compared to a Doctor, Manager, Home Maker, Student, or Clerical job.  Again binning this variable will strengthen the correlation.
\
\

Accordingly, we will bin these variables as below:
\
JOB_TYPE_AMT_BIN : \
\
- 1 : if JOB_TYPE is Lawyer, Professional, Unknown or in a Blue Collar \
- 0 : if JOB_TYPE is Doctor, Manager, Home Maker, Student, or Clerical
\
\

```{r}

insure_train_crash$JOB_TYPE_AMT_BIN <- ifelse(insure_train_crash$JOB_Lawyer |  insure_train_crash$JOB_Professional | insure_train_crash$JOB_Blue.Collar | insure_train_crash$JOB_Unknown, 1, 0)

```
\
\


- INCOME - From the plot we can see that there is a marked difference in the chart at around 125000. We will use this value to bin this variable. 

\
\

INCOME_AMT_BIN : \
\
- 1 : if INCOME <= 125000 \
- 0 : if INCOME > 125000
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}

insure_train_crash$INCOME_AMT_BIN <- ifelse(insure_train_crash$INCOME <=125000, 1, 0)

```
\
\

- YOJ - We can see that from 7 - 17 years, there is a visible change in the TARGET_AMT. We will use this  bound to create the binned variable.
\
\
YOJ_AMT_BIN : \
\
- 1 : if YOJ >=7 and YOJ<= 17 \
- 0 : ELSE 0
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}

insure_train_crash$YOJ_AMT_BIN <- ifelse((insure_train_crash$YOJ>=7 & insure_train_crash$YOJ<=17), 1, 0)

```
\
\


- HOME_VAL - We see from the plot 3 distinct segments - Between 0-10000, 60000-400000 and the rest. We will use these values to create 2 bins.
\
\
HOME_VAL_AMT_0_10K_BIN : \
\
- 1 : if HOME_VAL >=0 and HOME_VAL<= 10000 \
- 0 : ELSE 0
\
\
HOME_VAL_AMT_60K_400K_BIN : \
\
- 1 : if HOME_VAL >=60000 and HOME_VAL<= 400000 \
- 0 : ELSE 0


\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}

insure_train_crash$HOME_VAL_AMT_0_10K_BIN <- ifelse((insure_train_crash$HOME_VAL>=0 & insure_train_crash$HOME_VAL<=10000), 1, 0)

insure_train_crash$HOME_VAL_AMT_60K_400K_BIN <- ifelse((insure_train_crash$HOME_VAL>=60000 & insure_train_crash$HOME_VAL<=400000), 1, 0)

```
\
\

- OLDCLAIM- We can visualize 3 clusters in the data - 0-2000, 2000-10000, > 10000,  We will use these values to create 2 bins.
\
\
OLDCLAIM_AMT_0_2K_BIN : \
\
- 1 : if OLDCLAIM >=0 and OLDCLAIM<= 2000 \
- 0 : ELSE 0
\
\
OLDCLAIM_AMT_2K_10K_BIN : \
\
- 1 : if OLDCLAIM >=2000 and OLDCLAIM<= 10000 \
- 0 : ELSE 0


\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}

insure_train_crash$OLDCLAIM_AMT_0_2K_BIN <- ifelse((insure_train_crash$OLDCLAIM>=0 & insure_train_crash$OLDCLAIM<=2000), 1, 0)

insure_train_crash$OLDCLAIM_AMT_2K_10K_BIN <- ifelse((insure_train_crash$OLDCLAIM>=2001 & insure_train_crash$OLDCLAIM<=10000), 1, 0)

```
\
\


- CLM_FREQ - Values less than 4 seem to have a positive correlation. We will use this value for binning. 
\
\
CLM_FREQ_AMT_BIN : \
\
- 1 : if CLM_FREQ < 4 \
- 0 : if CLM_FREQ >= 4
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}
insure_train_crash$CLM_FREQ_AMT_BIN <- ifelse(insure_train_crash$CLM_FREQ <4, 1, 0)
```
\
\

- MVR_PTS - We can see from the plot that after 2, the TARGET_AMT starts decreasing. We will use this value for binning. 
\
\
MVR_PTS_AMT_BIN : \
\
- 1 : if MVR_PTS <=2 \
- 0 : if MVR_PTS > 0
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}
insure_train_crash$MVR_PTS_AMT_BIN <- ifelse(insure_train_crash$MVR_PTS <=2, 1, 0)
```
\
\

- CAR_AGE - There are quite a few records with a 1 year car age. We will use this bound to generate a binned variable as well as retain the original varible as is. 
\
\
CAR_AGE_AMT_BIN : \
\
- 1 : if CAR_AGE <= 1 \
- 0 : if CAR_AGE > 0
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}
insure_train_crash$CAR_AGE_AMT_BIN <- ifelse(insure_train_crash$CAR_AGE <=1, 1, 0)
```
\
\

- TRAVTIME - from the plot, we can see that there is a clear pattern around the value - 20. We will go ahead and create a binned variable for this.
\
\
TRAVTIME_AMT_BIN : \
\
- 1 : if TRAVTIME <= 20 \
- 0 : if TRAVTIME > 0
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE}
insure_train_crash$TRAVTIME_AMT_BIN <- ifelse(insure_train_crash$TRAVTIME <=20, 1, 0)
```
\
\

After having prepared the data, we will go ahead and drop some of the variables.

```{r}

#write.csv(insure_train_crash, file = "D:/CUNY/Courses/Business Analytics and Data Mining/Assignments/data621-ctg5/HW4/final.csv")

DS_TARGET_AMT <- select(insure_train_crash, -AGE, -BLUEBOOK, -CAR_AGE, -CAR_TYPE_Minivan, -CAR_TYPE_Panel.Truck, -CAR_TYPE_Pickup, -CAR_TYPE_Sports.Car, -CAR_TYPE_SUV, -CAR_TYPE_Van, -CLM_FREQ, -EDUCATION_Bachelors, -EDUCATION_High.School, -EDUCATION_Masters, -EDUCATION_PhD, -HOME_VAL, -INCOME, -JOB_Blue.Collar, -JOB_Clerical, -JOB_Doctor, -JOB_Home.Maker, -JOB_Lawyer, -JOB_Manager, -JOB_Professional, -JOB_Student, -JOB_Unknown, -MVR_PTS, -OLDCLAIM, -TIF, -TRAVTIME, -YOJ)

```
\
\


##3.3 Build Models


Now that we have the dataset in a shape that can be modeled, we will go ahead and train the model for TARGET_AMT. We will train 2 models and select the best among these 2 models. The following will be the model specifications:
\
\

- Model1 - This will use the standard lm for building the model. We will then step thru the model to refine it further. This will use all the variables available in the available "Train" dataset.\
\

- Model2 - This will use a tree based algorithm. This is a bit unconventional, but, rpart provides an option to do regression modeling for continuous variables. We will explore this in model2 with all the variables in the "Train" dataset. \
\


We will then generate inferences from these models.\
\
\

###3.3.1 Prepare TRAIN and VALID datasets

However, prior to that, we hold out a subset of data as a validation dataset to check model performance. This will be useful when we select a model.
\
\

```{r}

smp_size <- floor(0.80 * nrow(DS_TARGET_AMT))

## set the seed to make your partition reproductible
set.seed(123)

train_index <- sample(seq_len(nrow(DS_TARGET_AMT)), size = smp_size)

DS_TARGET_AMT_TRAIN<- DS_TARGET_AMT[train_index, ]
DS_TARGET_AMT_VALID <- DS_TARGET_AMT[-train_index, ]
```

###3.3.2 Model 1
\
\

In this model, we will be using the stadard lm modeling technique. We will create model and carry out a "backward"" stepwise refinement. \

```{r, echo = FALSE, warning=FALSE, message=FALSE}

TA_model1<-lm(TARGET_AMT~., na.omit(DS_TARGET_AMT_TRAIN))
TA_model1_ref <- step(TA_model1,direction="forward",test="F")
summary(TA_model1_ref)

```

\
\

**Interpretation of the Model**

Based on the backward stepwise selection, below are the characteristics of the refined model :

- The Residual standard error is 10.18
- Multiple R-squared: 0.4058
- Adjusted R-squared: 0.4019
- F-statistic: 103.7 on 12 and 1822 DF
- p-value: < 2.2e-16

\


```{r, echo = FALSE, warning=FALSE, message=FALSE, results='asis'}
#summary(step1)

coef1<- data.frame('Coefficients'= TA_model1_ref$coefficients)
kable(coef1, caption="Coefficients for the refined model 1")

```
\
\

Based on the above coefficients, we can see that some of the coefficients are counter-intutive to the Theoretical impact. 

- TEAM_BATTING_H (-0.034), TEAM_BATTING_2B (-0.049), TEAM_FIELDING_DP (-0.112), TEAM_PITCHING_SO (-0.054) have a negative coefficient even though they are theoretically supposed to have a positive impact on wins. This means that a unit change in each of these variables will decrease the number of a wins. 

- Similarly, TEAM_BATTING_SO (0.033), TEAM_PITCHING_H (0.06) have a positive coefficient even though they are theoretically supposed to have a negative impact on wins. This means that a unit change in each of these variables will increase the number of a wins. 

- TEAM_BATTING_3B (0.183), TEAM_BATTING_HR (0.1), TEAM_BATTING_BB (0.118), TEAM_BASERUN_SB (0.069), TEAM_FIELDING_E (-0.119), TEAM_PITCHING_BB (-0.08) have the intended theoretical impact on wins. This means that a unit change in each of these variables will either decrease or increase the number of a wins as intended by the theoretical impact. 

\

Since we have already seen this result in our data exploration phase, we will retain this model as is for comparision with other models. 
\
\


###3.3.3 Model 2

\
\

In this model, we will be using the rpart package to do a tree based regression. We will create model and carry out further pruning for the tree. \

```{r, echo = FALSE, warning=FALSE, message=FALSE}

# grow tree 
TA_model2 <- rpart(TARGET_AMT~., method="anova", data=DS_TARGET_AMT_TRAIN)

# prune the tree 
TA_model2_ref<- prune(TA_model2, cp=TA_model2$cptable[which.min(TA_model2$cptable[,"xerror"]),"CP"]) 

# plot the pruned tree 
 plot(pfit, uniform=TRUE, 
    main="Pruned Regression Tree for Mileage")
 text(pfit, use.n=TRUE, all=TRUE, cex=.8)
 post(pfit, file = "c:/ptree2.ps", 
    title = "Pruned Regression Tree for Mileage")



```

\
\

**Interpretation of the Model**

Based on the backward stepwise selection, below are the characteristics of the refined model :

- The Residual standard error is 10.18
- Multiple R-squared: 0.4058
- Adjusted R-squared: 0.4019
- F-statistic: 103.7 on 12 and 1822 DF
- p-value: < 2.2e-16

\


```{r, echo = FALSE, warning=FALSE, message=FALSE, results='asis'}
#summary(step1)

coef1<- data.frame('Coefficients'= model1_ref$coefficients)
kable(coef1, caption="Coefficients for the refined model 1")

```
\
\

Based on the above coefficients, we can see that some of the coefficients are counter-intutive to the Theoretical impact. 

- TEAM_BATTING_H (-0.034), TEAM_BATTING_2B (-0.049), TEAM_FIELDING_DP (-0.112), TEAM_PITCHING_SO (-0.054) have a negative coefficient even though they are theoretically supposed to have a positive impact on wins. This means that a unit change in each of these variables will decrease the number of a wins. 

- Similarly, TEAM_BATTING_SO (0.033), TEAM_PITCHING_H (0.06) have a positive coefficient even though they are theoretically supposed to have a negative impact on wins. This means that a unit change in each of these variables will increase the number of a wins. 

- TEAM_BATTING_3B (0.183), TEAM_BATTING_HR (0.1), TEAM_BATTING_BB (0.118), TEAM_BASERUN_SB (0.069), TEAM_FIELDING_E (-0.119), TEAM_PITCHING_BB (-0.08) have the intended theoretical impact on wins. This means that a unit change in each of these variables will either decrease or increase the number of a wins as intended by the theoretical impact. 

\

Since we have already seen this result in our data exploration phase, we will retain this model as is for comparision with other models. 
\
\






##3.4 Select Models

    
###3.4.1 Model Evaluation Using TRAIN Data 

###3.4.2 Model Evaluation Using VALID Data

###3.4.3 Selection Summary



#4 Prediction Using Evaluation Data
\
\

Now that we have selected the final models for both the TARGET_FLAG and the TARGET_AMT, we will go ahead and use these models to predict the results for the evaluation dataset. After transforming the data to meet the needs of the trained models, we will apply the models in 2 steps. 
\
\ 
Step 1 - Here we use the transformed evaluation dataset to predict for the TARGET_FLAG using the requisite predictors.\
\
\ 
Step 2 - Once we have the prediction for the TARGET_FLAG, we will filter this data for only those rows that were predicted for a "CRASH". We then use this smaller dataset to precict for the TARGET_AMT.
\

##4.1 Tranformation of Evaluation Data 

First we need to transform the evaluation dataset to account for all the predictors that were used in both the models.
\
\

```{r, echo = FALSE, warning=FALSE, message=FALSE, results='asis'}

eval_ds <- read.csv("https://raw.githubusercontent.com/kishkp/data621-ctg5/master/HW4/insurance-evaluation-data.csv")





```

##4.2 Model Output for Logistic Regression

\
\
We now apply the final Logistic regression model that was trained for predicting the TARGET_FLAG. Below are the predictions.

```{r, echo = FALSE, warning=FALSE, message=FALSE, results='asis'}


```



##4.3 Model Output for Linear Regression
\
\
Next we filter for the "predicted" crashes.
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE, results='asis'}


```
\
\

Next, we apply the final linear model to this smaller dataset and see the results.
\
\
```{r, echo = FALSE, warning=FALSE, message=FALSE, results='asis'}


```

