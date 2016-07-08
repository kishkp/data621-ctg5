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



# Read the data

insure_train_full <- read.csv("https://raw.githubusercontent.com/kishkp/data621-ctg5/master/HW4/insurance_training_data.csv")


# Basic cleanup

insure_train_full$INCOME <- as.numeric(str_replace_all(insure_train_full$INCOME, pattern =  "[\\$*,]", replacement = ""))
insure_train_full$HOME_VAL <- as.numeric(str_replace_all(insure_train_full$HOME_VAL, pattern =  "[\\$*,]", replacement = ""))
insure_train_full$BLUEBOOK <- as.numeric(str_replace_all(insure_train_full$BLUEBOOK, pattern =  "[\\$*,]", replacement = ""))
insure_train_full$OLDCLAIM <- as.numeric(str_replace_all(insure_train_full$OLDCLAIM, pattern =  "[\\$*,]", replacement = ""))

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


# Generate single variable for 2 factor variables.

# CAR_USE_Commercial, CAR_USE_Private, 
insure_train_full$CAR_USE_Commercial <- ifelse(insure_train_full$CAR_USE=="Commercial", 1, 0)

# MSTATUS_No, MSTATUS_Yes, 
insure_train_full$MSTATUS_Yes <- ifelse(insure_train_full$MSTATUS=="Yes", 1, 0)

# PARENT1_No, PARENT1_Yes, 
insure_train_full$PARENT1_Yes <- ifelse(insure_train_full$PARENT1=="Yes", 1, 0)

# RED_CAR_no, RED_CAR_yes, 
insure_train_full$RED_CAR_yes <- ifelse(insure_train_full$RED_CAR=="yes", 1, 0)

# REVOKED_No, REVOKED_Yes, 
insure_train_full$REVOKED_Yes <- ifelse(insure_train_full$REVOKED=="Yes", 1, 0)

# SEX_F, SEX_M, 
insure_train_full$SEX_M <- ifelse(insure_train_full$SEX=="M", 1, 0)

# URBANICITY_Highly.Rural..Rural, URBANICITY_Highly.Urban..Urban.
insure_train_full$URBANICITY_Rural <- ifelse(insure_train_full$URBANICITY=="Highly Rural/ Rural", 1, 0)

# remove original variables
insure_train_full <- select(insure_train_full, -CAR_USE, -MSTATUS, -PARENT1, -RED_CAR, -REVOKED, -SEX, -URBANICITY)

#- Dummy Varibles for remaining factor variables JOB, EDUCATION, CAR_TYPE. 

dummy_vars<-as.data.frame(sapply(dummy(insure_train_full), FUN = as.numeric))
dummy_vars <- dummy_vars-1

insure_train_full <- cbind(insure_train_full, dummy_vars)

# - Please note that we will not be using INDEX variable as it serves as just an identifier for each row. And has no relationships to other variables.   

insure_train_full <- select(insure_train_full, -INDEX)


# Save point for Original data set with dummies created
insure_orig <- insure_train_full


###########################
# LOGISTIC Regression
###########################

# Transformations for TARGET_FLAG

# Outliers
insure_train_full$TIF_sin <- sin(insure_train_full$TIF)
insure_train_full$BLUEBOOK_sin <- sin(insure_train_full$BLUEBOOK)
insure_train_full$AGE_sin <- sin(insure_train_full$AGE)

# Missing Impute
insure_train_full$AGE[is.na(insure_train_full$AGE)] <- mean(insure_train_full$AGE, na.rm = T) 
insure_train_full$YOJ[is.na(insure_train_full$YOJ)] <- mean(insure_train_full$YOJ, na.rm = T) 
insure_train_full$INCOME[is.na(insure_train_full$INCOME)] <- median(insure_train_full$INCOME, na.rm = T) 
insure_train_full$HOME_VAL[is.na(insure_train_full$HOME_VAL)] <- median(insure_train_full$HOME_VAL, na.rm = T) 
insure_train_full$CAR_AGE[is.na(insure_train_full$CAR_AGE)] <- median(insure_train_full$CAR_AGE, na.rm = 
                                                                          T) 
# Binning
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

insure_train_full$TRAVTIME_FLAG_BIN <- ifelse(insure_train_full$TRAVTIME <=20, 1, 0)


## - OPTIONAL - REMOVE ALL ORIGINAL VARIABLES THAT HAVE BEEN TRANSFORMED.

# insure_train_full <- select(insure_train_full, -AGE, -BLUEBOOK, -CAR_TYPE, -CAR_TYPE_Minivan, -CAR_TYPE_Panel.Truck, -CAR_TYPE_Pickup, -CAR_TYPE_Sports.Car, -CAR_TYPE_SUV, -CAR_TYPE_Van, -CAR_USE, -CAR_USE_Private, -CLM_FREQ, -EDUCATION, -EDUCATION_Bachelors, -EDUCATION_High.School, -EDUCATION_Masters, -EDUCATION_PhD, -HOME_VAL, -INCOME, -JOB, -JOB_Blue.Collar, -JOB_Clerical, -JOB_Doctor, -JOB_Home.Maker, -JOB_Lawyer, -JOB_Manager, -JOB_Professional, -JOB_Student, -JOB_Unknown, -MSTATUS, -MSTATUS_No, -MVR_PTS, -OLDCLAIM, -PARENT1, -PARENT1_No, -RED_CAR, -RED_CAR_no, -REVOKED, -REVOKED_No, -SEX, -SEX_F, -TARGET_AMT, -TIF, -TRAVTIME, -URBANICITY, -URBANICITY_Highly.Rural..Rural, -URBANICITY_Highly.Urban..Urban, -YOJ)

# TRAIN and VALID

smp_size <- floor(0.80 * nrow(insure_train_full))

## set the seed to make your partition reproductible
set.seed(123)

train_index <- sample(seq_len(nrow(insure_train_full)), size = smp_size)

# Dataset ready for modeling TARGET_FLAG

DS_TARGET_FLAG_TRAIN<- insure_train_full[train_index, ]
DS_TARGET_FLAG_VALID <- insure_train_full[-train_index, ]


# Model 1

model1 <- glm(TARGET_FLAG ~ ., data = na.omit(DS_TARGET_FLAG_TRAIN), family = "binomial")
summary(model1)

model1_ref<- step(model1, direction="backward")
summary(model1_ref)


# Model 2

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



###########################
# Linear Regression
###########################



# Use built dataset

insure_train_crash <- insure_train_full[insure_train_full$TARGET_FLAG==1,]


# Remove transformations that were specific to TARGET_FLAG

# insure_train_crash <- select(insure_train_crash, -AGE_sin, -BLUEBOOK_sin, -CAR_AGE_FLAG_BIN, -CAR_TYPE, -CAR_TYPE_FLAG_BIN, -CAR_USE, -CAR_USE_Private, -CLM_FREQ_FLAG_BIN, -EDUCATION, -EDUCATION_FLAG_BIN, -HOME_VAL_FLAG_BIN, -INCOME_FLAG_BIN, -JOB, -JOB_TYPE_FLAG_BIN, -MSTATUS, -MSTATUS_No, -MVR_PTS_FLAG_BIN, -OLDCLAIM_FLAG_BIN, -PARENT1, -PARENT1_No, -RED_CAR, -RED_CAR_no, -REVOKED, -REVOKED_No, -SEX, -SEX_F, -TARGET_FLAG, -TIF_sin, -URBANICITY, -URBANICITY_Highly.Rural..Rural, -URBANICITY_Highly.Urban..Urban, -YOJ_FLAG_BIN)


# Transformations for TARGET_FLAG

# Outliers

insure_train_crash$AGE_sin <- sin(insure_train_crash$AGE)

insure_train_crash$TIF_sin <- sin(insure_train_crash$TIF)

insure_train_crash$BLUEBOOK_sin <- sin(insure_train_crash$BLUEBOOK)


# Binning

insure_train_crash$CAR_TYPE_AMT_BIN <- ifelse(insure_train_crash$CAR_TYPE_Van | insure_train_crash$CAR_TYPE_Panel.Truck, 1, 0)

insure_train_crash$EDUCATION_AMT_BIN <- ifelse(insure_train_crash$EDUCATION_High.School, 1, 0)

insure_train_crash$JOB_TYPE_AMT_BIN <- ifelse(insure_train_crash$JOB_Lawyer |  insure_train_crash$JOB_Professional | insure_train_crash$JOB_Blue.Collar | insure_train_crash$JOB_Unknown, 1, 0)

insure_train_crash$INCOME_AMT_BIN <- ifelse(insure_train_crash$INCOME <=125000, 1, 0)

insure_train_crash$YOJ_AMT_BIN <- ifelse((insure_train_crash$YOJ>=7 & insure_train_crash$YOJ<=17), 1, 0)

insure_train_crash$HOME_VAL_AMT_0_10K_BIN <- ifelse((insure_train_crash$HOME_VAL>=0 & insure_train_crash$HOME_VAL<=10000), 1, 0)

insure_train_crash$HOME_VAL_AMT_60K_400K_BIN <- ifelse((insure_train_crash$HOME_VAL>=60000 & insure_train_crash$HOME_VAL<=400000), 1, 0)


insure_train_crash$OLDCLAIM_AMT_0_2K_BIN <- ifelse((insure_train_crash$OLDCLAIM>=0 & insure_train_crash$OLDCLAIM<=2000), 1, 0)

insure_train_crash$OLDCLAIM_AMT_2K_10K_BIN <- ifelse((insure_train_crash$OLDCLAIM>=2001 & insure_train_crash$OLDCLAIM<=10000), 1, 0)

insure_train_crash$CLM_FREQ_AMT_BIN <- ifelse(insure_train_crash$CLM_FREQ <4, 1, 0)

insure_train_crash$MVR_PTS_AMT_BIN <- ifelse(insure_train_crash$MVR_PTS <=2, 1, 0)

insure_train_crash$CAR_AGE_AMT_BIN <- ifelse(insure_train_crash$CAR_AGE <=1, 1, 0)


insure_train_crash$TRAVTIME_AMT_BIN <- ifelse(insure_train_crash$TRAVTIME <=20, 1, 0)

insure_train_crash<- insure_orig[insure_orig$TARGET_FLAG==1,]
insure_train_crash<- select(insure_train_crash, -TARGET_FLAG)

insure_train_crash$KIDSDRIV_BIN_0  <- ifelse(insure_train_crash$KIDSDRIV <=0, 1, 0)
insure_train_crash$KIDSDRIV_BIN_1  <- ifelse(insure_train_crash$KIDSDRIV <=1, 1, 0)

insure_train_crash$HOMEKIDS_BIN_0  <- ifelse(insure_train_crash$HOMEKIDS <=0, 1, 0)
insure_train_crash$HOMEKIDS_BIN_3  <- ifelse(insure_train_crash$HOMEKIDS <=3, 1, 0)

insure_train_crash$YOJ_BIN_0_AND_9To14  <- ifelse((insure_train_crash$YOJ ==0 | (insure_train_crash$YOJ>=9 & insure_train_crash$YOJ>=14)), 1, 0)

insure_train_crash$INCOME_BIN_MISS_0  <- ifelse((is.na(insure_train_crash$INCOME) |  insure_train_crash$INCOME<=0), 1, 0)

insure_train_crash$HOME_VAL_BIN_MISS_0  <- ifelse((is.na(insure_train_crash$HOME_VAL) |  insure_train_crash$HOME_VAL<=0), 1, 0)

insure_train_crash$EDUCATION_BIN_HS  <- ifelse(insure_train_crash$EDUCATION_High.School==1, 1, 0)
insure_train_crash$EDUCATION_BIN_HS_B  <- ifelse((insure_train_crash$EDUCATION_Bachelors |  insure_train_crash$EDUCATION_High.School), 1, 0)

insure_train_crash$JOB_BIN_CPSB  <- ifelse((insure_train_crash$JOB_Clerical |  insure_train_crash$JOB_Blue.Collar | insure_train_crash$JOB_Professional |  insure_train_crash$JOB_Student), 1, 0)

insure_train_crash$TIF_BIN_6  <- ifelse(insure_train_crash$TIF <=6, 1, 0)

insure_train_crash$CAR_TYPE_BIN_V_PT_MV  <- ifelse((insure_train_crash$CAR_TYPE_Van |  insure_train_crash$CAR_TYPE_Panel.Truck | insure_train_crash$CAR_TYPE_Minivan), 1, 0)

insure_train_crash$OLDCLAIM_BIN_MISS_0  <- ifelse((is.na(insure_train_crash$OLDCLAIM) |  insure_train_crash$OLDCLAIM<=0), 1, 0)

insure_train_crash$CLM_FREQ_BIN_0  <- ifelse(insure_train_crash$CLM_FREQ<=0, 1, 0)
insure_train_crash$CLM_FREQ_BIN_3  <- ifelse(insure_train_crash$CLM_FREQ<=3, 1, 0)

insure_train_crash$MVR_PTS_BIN_0  <- ifelse(insure_train_crash$MVR_PTS<=0, 1, 0)
insure_train_crash$MVR_PTS_BIN_5  <- ifelse(insure_train_crash$MVR_PTS<=5, 1, 0)


## - OPTIONAL - REMOVE ALL ORIGINAL VARIABLES THAT HAVE BEEN TRANSFORMED.


# insure_train_crash <- select(insure_train_crash, -AGE, -BLUEBOOK, -CAR_AGE, -CAR_TYPE_Minivan, -CAR_TYPE_Panel.Truck, -CAR_TYPE_Pickup, -CAR_TYPE_Sports.Car, -CAR_TYPE_SUV, -CAR_TYPE_Van, -CLM_FREQ, -EDUCATION_Bachelors, -EDUCATION_High.School, -EDUCATION_Masters, -EDUCATION_PhD, -HOME_VAL, -INCOME, -JOB_Blue.Collar, -JOB_Clerical, -JOB_Doctor, -JOB_Home.Maker, -JOB_Lawyer, -JOB_Manager, -JOB_Professional, -JOB_Student, -JOB_Unknown, -MVR_PTS, -OLDCLAIM, -TIF, -TRAVTIME, -YOJ)

# TRAIN and VALID

smp_size <- floor(0.80 * nrow(insure_train_crash))

## set the seed to make your partition reproductible
set.seed(123)

train_index <- sample(seq_len(nrow(insure_train_crash)), size = smp_size)



# Dataset ready for modeling TARGET_AMT

DS_TARGET_AMT_TRAIN<- insure_train_crash[train_index, ]
DS_TARGET_AMT_VALID <- insure_train_crash[-train_index, ]


# TA Model 1

TA_model1<- lm(TARGET_AMT~., data=DS_TARGET_AMT_TRAIN)
summary(TA_model1)
kable(sort(summary(TA_model1)$coefficients[,4]), col.names = "pvalues")


# Model refinement

TA_model1_ref<-step(TA_model1,direction="backward",test="F")
summary(TA_model1_ref)
kable(sort(summary(TA_model1_ref)$coefficients[,4]), col.names = "pvalues")








