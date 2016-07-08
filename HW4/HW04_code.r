
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
#insure_train_full <- select(insure_train_full, -CAR_USE, -MSTATUS, -PARENT1, -RED_CAR, -REVOKED, -SEX, -URBANICITY)

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



###########################
# Linear Regression
###########################



# Use built dataset

insure_train_crash <- insure_train_full[insure_train_full$TARGET_FLAG==1,]


# Remove transformations that were specific to TARGET_FLAG

insure_train_crash <- select(insure_train_crash, -AGE_sin, -BLUEBOOK_sin, -CAR_AGE_FLAG_BIN, -CAR_TYPE, -CAR_TYPE_FLAG_BIN, -CAR_USE, -CAR_USE_Private, -CLM_FREQ_FLAG_BIN, -EDUCATION, -EDUCATION_FLAG_BIN, -HOME_VAL_FLAG_BIN, -INCOME_FLAG_BIN, -JOB, -JOB_TYPE_FLAG_BIN, -MSTATUS, -MSTATUS_No, -MVR_PTS_FLAG_BIN, -OLDCLAIM_FLAG_BIN, -PARENT1, -PARENT1_No, -RED_CAR, -RED_CAR_no, -REVOKED, -REVOKED_No, -SEX, -SEX_F, -TARGET_FLAG, -TIF_sin, -URBANICITY, -URBANICITY_Highly.Rural..Rural, -URBANICITY_Highly.Urban..Urban, -YOJ_FLAG_BIN)


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

insure_amt<- insure_orig[insure_orig$TARGET_FLAG==1,]
insure_amt<- select(insure_amt, -TARGET_FLAG)

insure_amt$KIDSDRIV_BIN_0  <- ifelse(insure_amt$KIDSDRIV <=0, 1, 0)
insure_amt$KIDSDRIV_BIN_1  <- ifelse(insure_amt$KIDSDRIV <=1, 1, 0)

insure_amt$HOMEKIDS_BIN_0  <- ifelse(insure_amt$HOMEKIDS <=0, 1, 0)
insure_amt$HOMEKIDS_BIN_3  <- ifelse(insure_amt$HOMEKIDS <=3, 1, 0)

insure_amt$YOJ_BIN_0_AND_9To14  <- ifelse((insure_amt$YOJ ==0 | (insure_amt$YOJ>=9 & insure_amt$YOJ>=14)), 1, 0)

insure_amt$INCOME_BIN_MISS_0  <- ifelse((is.na(insure_amt$INCOME) |  insure_amt$INCOME<=0), 1, 0)

insure_amt$HOME_VAL_BIN_MISS_0  <- ifelse((is.na(insure_amt$HOME_VAL) |  insure_amt$HOME_VAL<=0), 1, 0)

insure_amt$EDUCATION_BIN_HS  <- ifelse(insure_amt$EDUCATION_High.School==1, 1, 0)
insure_amt$EDUCATION_BIN_HS_B  <- ifelse((insure_amt$EDUCATION_Bachelors |  insure_amt$EDUCATION_High.School), 1, 0)

insure_amt$JOB_BIN_CPSB  <- ifelse((insure_amt$JOB_Clerical |  insure_amt$JOB_Blue.Collar | insure_amt$JOB_Professional |  insure_amt$JOB_Student), 1, 0)

insure_amt$TIF_BIN_6  <- ifelse(insure_amt$TIF <=6, 1, 0)

insure_amt$CAR_TYPE_BIN_V_PT_MV  <- ifelse((insure_amt$CAR_TYPE_Van |  insure_amt$CAR_TYPE_Panel.Truck | insure_amt$CAR_TYPE_Minivan), 1, 0)

insure_amt$OLDCLAIM_BIN_MISS_0  <- ifelse((is.na(insure_amt$OLDCLAIM) |  insure_amt$OLDCLAIM<=0), 1, 0)

insure_amt$CLM_FREQ_BIN_0  <- ifelse(insure_amt$CLM_FREQ<=0, 1, 0)
insure_amt$CLM_FREQ_BIN_3  <- ifelse(insure_amt$CLM_FREQ<=3, 1, 0)

insure_amt$MVR_PTS_BIN_0  <- ifelse(insure_amt$MVR_PTS<=0, 1, 0)
insure_amt$MVR_PTS_BIN_5  <- ifelse(insure_amt$MVR_PTS<=5, 1, 0)


## - OPTIONAL - REMOVE ALL ORIGINAL VARIABLES THAT HAVE BEEN TRANSFORMED.


# insure_train_crash <- select(insure_train_crash, -AGE, -BLUEBOOK, -CAR_AGE, -CAR_TYPE_Minivan, -CAR_TYPE_Panel.Truck, -CAR_TYPE_Pickup, -CAR_TYPE_Sports.Car, -CAR_TYPE_SUV, -CAR_TYPE_Van, -CLM_FREQ, -EDUCATION_Bachelors, -EDUCATION_High.School, -EDUCATION_Masters, -EDUCATION_PhD, -HOME_VAL, -INCOME, -JOB_Blue.Collar, -JOB_Clerical, -JOB_Doctor, -JOB_Home.Maker, -JOB_Lawyer, -JOB_Manager, -JOB_Professional, -JOB_Student, -JOB_Unknown, -MVR_PTS, -OLDCLAIM, -TIF, -TRAVTIME, -YOJ)

# TRAIN and VALID

smp_size <- floor(0.80 * nrow(insure_train_crash))

## set the seed to make your partition reproductible
set.seed(123)

train_index <- sample(seq_len(nrow(insure_train_crash)), size = smp_size)



# Dataset ready for modeling TARGET_AMT

DS_TARGET_FLAG_TRAIN<- insure_train_crash[train_index, ]
DS_TARGET_FLAG_VALID <- insure_train_crash[-train_index, ]










