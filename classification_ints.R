#load packages
library(data.table)
library(pscl)
library(MASS)
library(ndot.tools)
library(bestglm)
library(glmulti)
library(rJava)
library(caret)
library(xgboost)
library(mltools)
library(tidyverse)
library(lime)
library(gbm)
#set dir
setwd("/Users/ryanloos/Capstone/")

#load data
ints <- fread("clean_data/int_for_model.csv")

#create classification var
ints <- ints[, sev_obs := 0]
ints <- ints[Severe_Crashes>0, sev_obs := 1]
#refactorize columns...
for(var in colnames(ints)) {
  if (class(ints[[var]]) == "character") {
    ints[[var]] <- as.factor(ints[[var]])
  }
}
ints[, Previous_Stop := as.factor(Previous_Stop)]
#need to set 0/1 as factor as well

#split urban/rural
int_r <- ints[Area_Type %in% c("rural", "small town")]
int_u <- ints[Area_Type %in% c("urban", "urban core", "suburban")]

CRSP2_Unique_ID_r <- int_r$CRSP2_Unique_ID
CRSP2_Unique_ID_u <- int_u$CRSP2_Unique_ID

int_r[, CRSP2_Unique_ID := NULL]
int_r[, Severe_Crashes := NULL]
int_u[, CRSP2_Unique_ID := NULL]
int_u[, Severe_Crashes := NULL]

int_r <- int_r[, c("Area_Type", "Context_Zone", "Int_Type", "Total_Entering_ADT", 
                   "Cross_Product", "Design_Type", "Traffic_Control", "Leg_Configuration", 
                   "Major_Division_Configuration", "Minor_Division_Configuration", 
                   "Major_Surface_Type", "Minor_Surface_Type", "Alignment_Skew", 
                   "Railroad_Crossing", "Adjacent_Curve", "Adjacent_Development", 
                   "Lighting", "Previous_Stop", "Major_Speed_Limit", 
                   "Minor_Speed_Limit", "Flashers",
                   "Maj1_Lane_Config", "Maj2_Lane_Config", "Legs", 
                   "Max_Lanes_Cross", "sev_obs")]
#took out parking, overhead signal, LT phasing, sidewalk, refuge island, ped crossing, ,bike facility, ped indicator
# transit adjacent, school crosswalk

int_u <- int_u[, c("Area_Type", "Context_Zone", "Int_Type", "Total_Entering_ADT", 
                   "Cross_Product", "Design_Type", "Traffic_Control", "Leg_Configuration", 
                   "Major_Division_Configuration", "Minor_Division_Configuration", 
                   "Major_Surface_Type", "Minor_Surface_Type", "Alignment_Skew", 
                   "Railroad_Crossing", "Adjacent_Curve", "Adjacent_Development", 
                   "Street Parking", "Lighting", "Previous_Stop", "Major_Speed_Limit", 
                   "Minor_Speed_Limit", "Flashers", "Overhead_Signal", "Left_Turn_Phasing_Maj", 
                   "Left_Turn_Phasing_Min", "Maj1_Lane_Config", "Maj2_Lane_Config", 
                   "Sidewalk", "Refuge_Island", "Ped_Crossing", "Bike_Facility", 
                   "Ped_Indicator", "Transit_Adjacent", "School_Crosswalk", "Legs", 
                   "Max_Lanes_Cross", "sev_obs")]
#left everything in for urban, nothing super rural specific.

#drop boring columns
drop_boring_columns(int_r)
#drop boring columns
drop_boring_columns(int_u)

#### XGBOOST ALL ----
#functions ----
model_type.gbm <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  
  return("classification")
}

predict_model.gbm <- function(x, newdata, ...) {
  # Function performs prediction and returns data frame with Response
  pred <- predict(x, newdata, type = "response")
  return(as.data.frame(pred))
}

#rural----
set.seed(123)
#set up test train
#https://towardsdatascience.com/understanding-gradient-boosting-machines-9be756fe76ab

#run model
int_xgb_r <- gbm(sev_obs~.,data=int_r,distribution="bernoulli",n.trees = 5000,
                 shrinkage = .001,interaction.depth = 6, cv.folds = 5)
print(int_xgb_r)
summary(int_xgb_r)
summary.gbm(int_xgb_r)
for_r_plot <- summary(int_xgb_r)

ggplot(for_r_plot, aes(x = rel.inf, y = reorder(var, rel.inf))) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  scale_x_continuous(name = "Relative Variable Weight") +
  scale_y_discrete(name = "Variable")

model_type(int_xgb_r)

best_iter_r = gbm.perf(int_xgb_r, method="cv")
best_iter_r
pred_xg_r <- predict_model.gbm(int_xgb_r, newdata = int_r)
pred_xg_r <- pred_xg_r$pred

explainer_gbm_r <- lime(int_r, int_xgb_r, n_bins = 5)
explanation_gbm_r <- explain(int_r, explainer_gbm_r, n_features = 10, n_labels = 1, n_permutations = 1000, kernel_width = .75)
plot_features(explanation_gbm_r[1:19,]) + ggtitle("gbm")


#contingency table
threshhold_xg_r <- 0.15  # Set Y=1 when predicted probability exceeds this

#check for a more reasonable threshold
predcrash_xg_r <- cut(pred_xg_r, breaks=c(-Inf, threshhold_xg_r, Inf), 
                      labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here

cTab_xg_r <- table(int_r$sev_obs, predcrash_xg_r) 
addmargins(cTab_xg_r)

#accuracy
perc_xg_r <- sum(diag(cTab_xg_r)) / sum(cTab_xg_r)  # compute the proportion of correct classifications #catches lots of false
print(paste('Proportion correctly predicted = ', round(perc_xg_r,3))) 
#precision - predicting the positive class - this could be ok if we want false positives to examine.
prec_xg_r <- cTab_xg_r[2,2]/(cTab_xg_r[1,2]+cTab_xg_r[2,2])
print(paste('Precision (correctly predict positive) = ', round(prec_xg_r,3))) 
#precision - rate at which model captures true class.
recall_xg_r <- cTab_xg_r[2,2]/(cTab_xg_r[2,1]+cTab_xg_r[2,2])
print(paste('Recall (correctly capture positive) = ', round(recall_xg_r,3))) 

#test different thresholds
predperc_xg_r <- c()
predprec_xg_r <- c()
predrecall_xg_r <- c()
predsev_xg_r <- c()
for (i in 1:20){
  threshhold <- i/10/2  # Set Y=1 when predicted probability exceeds this
  predcrash_xg_r <- cut(pred_xg_r, breaks=c(-Inf, threshhold, Inf), 
                        labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here
  
  cTab_xg_r <- table(int_r$sev_obs, predcrash_xg_r) 
  addmargins(cTab_xg_r)
  
  p <- sum(diag(cTab_xg_r)) / sum(cTab_xg_r)  # compute the proportion of correct classifications
  s <- sum(cTab_xg_r[,2])
  prec <- cTab_xg_r[2,2]/(cTab_xg_r[1,2]+cTab_xg_r[2,2])
  recallcv <- cTab_xg_r[2,2]/(cTab_xg_r[2,1]+cTab_xg_r[2,2])
  #prcurv(paste('Proportion correctly predicted = ', p))
  predperc_xg_r <- c(predperc_xg_r,p) #accuracy
  predprec_xg_r <- c(predprec_xg_r,prec)
  predrecall_xg_r <- c(predrecall_xg_r, recallcv)
  predsev_xg_r <- c(predsev_xg_r, s) #predicted severe
  
}
predperc_xg_r
predprec_xg_r
predrecall_xg_r
predsev_xg_r

plot_data_r <- cbind(seq(.05,1, by = .05), predperc_xg_r, predprec_xg_r, predrecall_xg_r)
colnames(plot_data_r) <- c("threshold","accuracy", "precision", "recall")
plot_data_r <- melt(as.data.table(plot_data_r), id.vars = "threshold")

x = seq(.05,1, by = .05)
for_plot_r <- as.data.frame(cbind(predsev_xg_r, x))
ggplot(for_plot_r, aes(x = x, y = predsev_xg_r)) +
  geom_line()+
  geom_point()+
  theme_bw() +
  scale_x_continuous(name = "Threshold for Severe Presence Prediction", breaks = seq(0,1, by = .1)) +
  scale_y_continuous(name = "Segments Predicted to Have Severe Crashes") +
  geom_hline(yintercept = 212)

ggplot(plot_data_r, aes(x=threshold, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(name = "Threshold for Severe Presence Prediction", breaks = seq(0,1, by = .1)) +
  scale_y_continuous(name = "Metric Value", breaks = seq(0,1, by = .1)) +
  labs(color = "Performance Metric") +
  theme(legend.position = "bottom")

#urban ----
#set up test train
#https://towardsdatascience.com/understanding-gradient-boosting-machines-9be756fe76ab

#run model
int_xgb_u <- gbm(sev_obs~.,data=int_u,distribution="bernoulli",n.trees = 5000,
                 shrinkage = .001,interaction.depth = 6, cv.folds = 5)
print(int_xgb_u)
summary(int_xgb_u)
summary.gbm(int_xgb_u)
for_u_plot <- summary(int_xgb_u)

ggplot(for_u_plot, aes(x = rel.inf, y = reorder(var, rel.inf))) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  scale_x_continuous(name = "Relative Variable Weight") +
  scale_y_discrete(name = "Variable")

model_type(int_xgb_u)

best_iter_u = gbm.perf(int_xgb_u, method="cv")
best_iter_u
pred_xg_u <- predict_model.gbm(int_xgb_u, newdata = int_u)
pred_xg_u <- pred_xg_u$pred

explainer_gbm_u <- lime(int_u, int_xgb_u, n_bins = 5) #use full data, but tie pack predictions for plot. Or, put test data here? Can try both
explanation_gbm_u <- explain(int_u, explainer_gbm_u, n_features = 10, n_labels = 1, n_permutations = 1000, kernel_width = .75)
plot_features(explanation_gbm_u[1:19,]) + ggtitle("gbm")


#contingency table
threshhold_xg_u <- 0.25  # Set Y=1 when predicted probability exceeds this

#check for a more reasonable threshold
predcrash_xg_u <- cut(pred_xg_u, breaks=c(-Inf, threshhold_xg_u, Inf), 
                      labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here

cTab_xg_u <- table(int_u$sev_obs, predcrash_xg_u) 
addmargins(cTab_xg_u)

#accuracy
perc_xg_u <- sum(diag(cTab_xg_u)) / sum(cTab_xg_u)  # compute the proportion of correct classifications #catches lots of false
print(paste('Proportion correctly predicted = ', round(perc_xg_u,3))) 
#precision - predicting the positive class - this could be ok if we want false positives to examine.
prec_xg_u <- cTab_xg_u[2,2]/(cTab_xg_u[1,2]+cTab_xg_u[2,2])
print(paste('Precision (correctly predict positive) = ', round(prec_xg_u,3))) 
#precision - rate at which model captures true class.
recall_xg_u <- cTab_xg_u[2,2]/(cTab_xg_u[2,1]+cTab_xg_u[2,2])
print(paste('Recall (correctly capture positive) = ', round(recall_xg_u,3))) 

#test different thresholds
predperc_xg_u <- c()
predprec_xg_u <- c()
predrecall_xg_u <- c()
predsev_xg_u <- c()
for (i in 1:20){
  threshhold <- i/10/2  # Set Y=1 when predicted probability exceeds this
  predcrash_xg_u <- cut(pred_xg_u, breaks=c(-Inf, threshhold, Inf), 
                        labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here
  
  cTab_xg_u <- table(int_u$sev_obs, predcrash_xg_u) 
  addmargins(cTab_xg_u)
  
  p <- sum(diag(cTab_xg_u)) / sum(cTab_xg_u)  # compute the proportion of correct classifications
  s <- sum(cTab_xg_u[,2])
  prec <- cTab_xg_u[2,2]/(cTab_xg_u[1,2]+cTab_xg_u[2,2])
  recallcv <- cTab_xg_u[2,2]/(cTab_xg_u[2,1]+cTab_xg_u[2,2])
  #prcurv(paste('Proportion correctly predicted = ', p))
  predperc_xg_u <- c(predperc_xg_u,p) #accuracy
  predprec_xg_u <- c(predprec_xg_u,prec)
  predrecall_xg_u <- c(predrecall_xg_u, recallcv)
  predsev_xg_u <- c(predsev_xg_u, s) #predicted severe
  
}
predperc_xg_u
predprec_xg_u
predrecall_xg_u
predsev_xg_u

plot_data_u <- cbind(seq(.05,1, by = .05), predperc_xg_u, predprec_xg_u, predrecall_xg_u)
colnames(plot_data_u) <- c("threshold","accuracy", "precision", "recall")
plot_data_u <- melt(as.data.table(plot_data_u), id.vars = "threshold")

x = seq(.05,1, by = .05)
for_plot_u <- as.data.frame(cbind(predsev_xg_u, x))
ggplot(for_plot_u, aes(x = x, y = predsev_xg_u)) +
  geom_line()+
  geom_point()+
  theme_bw() +
  scale_x_continuous(name = "Threshold for Severe Presence Prediction", breaks = seq(0,1, by = .1)) +
  scale_y_continuous(name = "Segments Predicted to Have Severe Crashes") +
  geom_hline(yintercept = 252)

ggplot(plot_data_u, aes(x=threshold, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(name = "Threshold for Severe Presence Prediction", breaks = seq(0,1, by = .1)) +
  scale_y_continuous(name = "Metric Value", breaks = seq(0,1, by = .1)) +
  labs(color = "Performance Metric") +
  theme(legend.position = "bottom")

#### TEST AGAIN WITH SMALLER MODELS WITH ONLY TREATABLE FEATURES ----

#### WRITE LIME TABLES----
LIME_r_export <- explanation_gbm_r[,1:11]
LIME_u_export <- explanation_gbm_u[,1:11]
fwrite(LIME_r_export, "clean_data/int_r_LIME_output.csv")
fwrite(LIME_u_export, "clean_data/int_u_LIME_output.csv")
