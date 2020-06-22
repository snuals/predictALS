
# load library
library(tidyverse)
library(mlr)
library(Hmisc)
library(randomForestSRC)
library(doMC)
registerDoMC(cores = 4)

# prediction error curve 
library(pec)
library(survival)
library(randomForestSRC)
library(rms) 
library(prodlim)
library(party)
library(riskRegression)

# Predict survival 
surv = read.csv("/Users/hong/Dropbox/ALSmaster/PROACT/Survival_all.csv") # n=9080 
proact = read.csv("/Users/hong/Dropbox/ALSmaster/PROACT/PROACT_imputed_dummied.csv") # n=6507

df = merge(surv, proact, by = "SubjectID")
df = df[,-1] # n=6041


df = df %>% # to build a prototype model
  select(time_event, status, Age, GenderM,
         fvc.slope, fvc_mean,
         ALSFRS_Total_slope, ALSFRS_Total_mean,
         onset_delta,
         onset_siteBulbar, onset_siteLimb, onset_siteLimb.and.Bulbar)
            
# predict with a sample dataset        
m = sample(dim(df)[1], size = 5)
sample_data = df[m,]
write.csv(sample_data, "sample_data.csv")

fitform = Surv(time_event, status) ~ . 
fitcox <- coxph(fitform, data=df, x = TRUE)
save(fitcox, file = "model_cox.rda")

plotPredictSurvProb(fitcox,newdata=sample_data[,-c(1,2)],lty=1)

# Evaluate performance of the prediction model

# split dataset
set.seed(999)
n = sample(dim(df)[1], size = 0.75*dim(df)[1]) # 
trainset = df[n,]
testset = df[-n,]

# task 
surv.task = makeSurvTask(data = trainset, # only the trainset 
                         target = c("time_event", "status"))
# learner 
surv.lrn.coxph = makeLearner("surv.coxph")

# train 
model_cox = train(surv.lrn.coxph, surv.task) 

# predict & performance
pred.coxph = predict(model_cox, newdata = testset) # testset 
pred.coxph.df = as.data.frame(pred.coxph)
performance(pred.coxph)

# resample 
rdsc = makeResampleDesc("CV", iters = 10)
cv.coxph = resample("surv.coxph", surv.task, 
         resampling = rdsc, 
         keep.pred = F)                         
                   
fitform = Surv(time_event, status) ~ . 
fitcox <- coxph(fitform, data=trainset, x = TRUE)

set.seed(13)
pcox = predictSurvProb(fitcox, newdata = testset, 
                       times = 30*12)
extends <- function(...)TRUE

# par(mfrow = c(2,4))
# lapply(1:8, function(x){
# plotPredictSurvProb(fitcox,newdata=testset[x,],lty=1)
# plotPredictSurvProb(fitcforest, newdata=testset[x,], add=TRUE, lty=2)
# })

pec_cox = pec(list("Cox" = fitcox), # prediction error curves
             data = testset,
             traindata = trainset, 
             formula = Surv(time_event,status)~1, # KM estimator to calculate the weights (probabilities of not being censored) 
             reference = T, # KM prediction model as a reference
             splitMethod = "cv10", B = 4, # 10-fold CV, repeated 4 times 
             keep.index = T, keep.matrix = T)

save(pec_cox, file = "pec_cox.rda")

plot(pec_cox)
crps.t1000 = crps(pec_cox, times = 1000)
# continuous rank probability score 
# computes integrated Brier scores 






