# Build a Cox regression model to predict survival of ALS patients 
# Train and test the model using PROACT dataset 

# load packages 
library(tidyverse)
library(survival)
library(survminer)

# read data
surv = read.csv("/Users/hong/Dropbox/ALSmaster/PROACT/Survival_all.csv") # n=9080 
surv$time_event = (surv$time_event)/30 # convert days to months 
proact = read.csv("/Users/hong/Dropbox/ALSmaster/PROACT/PROACT_imputed_dummied.csv") # n=6507
proact = proact %>% # select features to build a prototype model
  select(SubjectID, Age, GenderM,
         fvc.slope, fvc_mean,
         ALSFRS_Total_slope, ALSFRS_Total_mean,
         onset_delta,
         onset_siteLimb, onset_siteBulbar, onset_siteLimb.and.Bulbar)

df = merge(surv, proact, by = "SubjectID")
df = df[,-1] # n=6041

# sort out feature names 
names(df) = c("Time", "Status", "Age_enrollment", "Gender", 
              "FVC_slope", "FVC_mean", 
              "ALSFRS_slope", "ALSFRS_mean", 
              "Onset2enrollment", 
              "Onset_limb", "Onset_bulbar", "Onset_both"
              ) 
df$Onset2enrollment = -(df$Onset2enrollment)/30
# Time; from enrollment to death or censoring
# Status; 0=censoring, 1=event (death)
# Gender; 0=female, 1=male 
# Onset2Enrollment; time from onset to enrollment (months)

# fit a cox model to data
formula = as.formula("Surv(Time, Status) ~ Age_enrollment + Gender + 
                    FVC_slope + FVC_mean + ALSFRS_slope + 
                    ALSFRS_mean +  Onset2enrollment +  
                    Onset_limb + Onset_bulbar + Onset_both")

model_cox = coxph(formula, data = df)
save(model_cox, file = "cox_model.rda")

fit_cox = survfit(model_cox, data = df)
# fit_cox_rank = survfit(model_cox, newdata = df)
# names(fit_cox_rank$call)
# Plot survival curve with confidence
ggsurvplot(fit_cox, ggtheme = theme_minimal())

# new data
sex_df <- with(df,
               data.frame(Gender = c(0, 1), 
                          Age_enrollment = rep(mean(Age_enrollment), 2),
                          FVC_slope = rep(mean(FVC_slope), 2), 
                          FVC_mean = rep(mean(FVC_mean), 2),
                          ALSFRS_slope = rep(mean(ALSFRS_slope), 2),
                          ALSFRS_mean = rep(mean(ALSFRS_mean), 2),
                          Onset2enrollment = rep(mean(Onset2enrollment), 2), 
                          Onset_limb = rep(mean(Onset_limb), 2), 
                          Onset_bulbar = rep(mean(Onset_bulbar), 2), 
                          Onset_both = rep(mean(Onset_both), 2))
)
sex_df

sample_data = read_csv('sample_data.csv')
source("featureMapping.R")
new_df = feature_mapping(sample_data)

newfit <- survfit(model_cox, newdata = sex_df)
newfit
summary(newfit)$table[,'median']

newfit <- survfit(model_cox, newdata = new_df)
newfit
summary(newfit)$table[,c(7:9)]


ggsurvplot(newfit, data = sex_df, conf.int = FALSE,
           censor = FALSE, surv.median.line = "hv",
           ggtheme = theme_minimal(),
           xlim = c(0,75))

ggsurvplot(newfit, data = new_df, conf.int = FALSE, 
           censor = FALSE, surv.median.line = "hv", 
           ggtheme = theme_minimal(), 
           xlim = c(0,24))

