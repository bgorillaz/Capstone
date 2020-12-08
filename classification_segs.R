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
segs <- fread("clean_data/seg_for_model.csv")
set.seed(123)
#create classification var
segs <- segs[, sev_obs := 0]
segs <- segs[SevereCrashCount>0, sev_obs := 1]
# segs <- segs[, sev_obs := as.factor(sev_obs)]
#refactorize columns...
for(var in colnames(segs)) {
  if (class(segs[[var]]) == "character") {
    segs[[var]] <- as.factor(segs[[var]])
  }
}
#need to set 0/1 as factor as well

#split urban/rural
seg_r <- segs[Area_Type %in% c("rural", "small town")]
seg_u <- segs[Area_Type %in% c("urban", "urban core", "suburban")]

CRSP2_Unique_ID_r <- seg_r$CRSP2_Unique_ID
CRSP2_Unique_ID_u <- seg_u$CRSP2_Unique_ID

segs[, CRSP2_Unique_ID := NULL] #bring back if we are looking at predictions and just remove from model

seg_r_full <- seg_r[, c("MVMT_5yrMillionVMT", "ADT_vpd", "Density_Curve", "Area_Type", 
                        "Context_Zone", "Segment_Cross_Section", "Segment_Design", "Seg_CrossSection_Design", 
                        "Speed_Limit_mph", "Access_Density", "Rumble_Strips", "Mumble_Strips", 
                        "Lane_Width", "Shoulder_Type", "Shoulder_Width", "Median_Type", 
                        "Median_Width", "Edge_Risk", "Edgeline_Striping", "Centerline_Striping", 
                        "Road_Access_Count", 
                        "Commercial_Access_Count", "Residential_Access_Count", "Farm_Access_Count", 
                        "Alley_Other_Access_Count", "Total_Access_County", "sev_obs")]
#took out parking, sidewalks, bike lanes

seg_u_full <- seg_u[, c("MVMT_5yrMillionVMT", "ADT_vpd", "Density_Curve", "Area_Type", 
                        "Context_Zone", "Segment_Cross_Section", "Segment_Design", "Seg_CrossSection_Design", 
                        "Speed_Limit_mph", "Access_Density", "Rumble_Strips", "Lane_Width", 
                        "Shoulder_Type", "Shoulder_Width", "Median_Type", "Median_Width", 
                        "Edge_Risk", "Edgeline_Striping", "Centerline_Striping", "Parking_Allowed", 
                        "Sidewalks", "Bike_Lanes", "Road_Access_Count", "Commercial_Access_Count", 
                        "Residential_Access_Count", "Alley_Other_Access_Count", 
                        "Total_Access_County", "sev_obs")]
#took out farm access count

seg_u_full <- drop_boring_columns(seg_u_full) 

#### FOR BOOST WITH CLASSIFICATION----
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

#rural ----
#find total number of features - will need for Tableau? Just use top 10... F1, F2, etc.
seg_xgb_r <- gbm(sev_obs~.,data=seg_r_full,distribution="bernoulli",n.trees = 5000,
                 shrinkage = .001,interaction.depth = 6, cv.folds = 5)
for_r_plot <- summary(seg_xgb_r)

ggplot(for_r_plot, aes(x = rel.inf, y = reorder(var, rel.inf))) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  scale_x_continuous(name = "Relative Variable Weight") +
  scale_y_discrete(name = "Variable")

model_type(seg_xgb_r)

best_iter_r = gbm.perf(seg_xgb_r, method="cv")
best_iter_r
pred_xg_r <- predict_model.gbm(seg_xgb_r, newdata = seg_r_full)
pred_xg_r <- pred_xg_r$pred

explainer_gbm_r <- lime(seg_r_full, seg_xgb_r, n_bins = 5)
explanation_gbm_r <- explain(seg_r_full, explainer_gbm_r, n_features = 10, n_labels = 1, n_permutations = 500, kernel_width = .75)
plot_features(explanation_gbm_r[1:19,]) + ggtitle("gbm")

#need to join data back on to this.
r_rows <- nrow(seg_r_full)*10
r_exp_DT <- as.data.table(explanation_gbm_r[1:r_rows,])
test <- r_exp_DT[feature == "sev_obs"]

seg_r_full_processing <- seg_r_full[, idx := 1:.N]


#contingency table
threshhold_xg_r <- 0.35  # Set Y=1 when predicted probability exceeds this
#check for a more reasonable threshold

predcrash_xg_r <- cut(pred_xg_r, breaks=c(-Inf, threshhold_xg_r, Inf), 
                      labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here

cTab_xg_r <- table(seg_r_full$sev_obs, predcrash_xg_r) 
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
  
  cTab_xg_r <- table(seg_r_full$sev_obs, predcrash_xg_r) 
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
  geom_hline(yintercept = 365)

ggplot(plot_data_r, aes(x=threshold, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(name = "Threshold for Severe Presence Prediction", breaks = seq(0,1, by = .1)) +
  scale_y_continuous(name = "Metric Value", breaks = seq(0,1, by = .1)) +
  labs(color = "Performance Metric") +
  theme(legend.position = "bottom")

#urban ----
seg_xgb_u <- gbm(sev_obs~.,data=seg_u_full,distribution="bernoulli",n.trees = 5000,
                 shrinkage = .001,interaction.depth = 6, cv.folds = 5)
for_u_plot <- summary(seg_xgb_u)

ggplot(for_u_plot, aes(x = rel.inf, y = reorder(var, rel.inf))) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  scale_x_continuous(name = "Relative Variable Weight") +
  scale_y_discrete(name = "Variable")

model_type(seg_xgb_u)
best_iter_u = gbm.perf(seg_xgb_u, method="cv")
best_iter_u

pred_xg_u <- predict_model.gbm(seg_xgb_u, newdata = seg_u_full)
pred_xg_u <- pred_xg_u$pred

explainer_gbm_u <- lime(seg_u_full, seg_xgb_u, n_bins = 5)
explanation_gbm_u <- explain(seg_u_full, explainer_gbm_u, n_features = 10, n_labels = 1, n_permutations = 500, kernel_width = .75)
plot_features(explanation_gbm_u[1:19,]) + ggtitle("gbm")

#contingency table
threshhold_xg_u <- 0.4  # Set Y=1 when predicted probability exceeds this
#check for a more reasonable threshold

predcrash_xg_u <- cut(pred_xg_u, breaks=c(-Inf, threshhold_xg_u, Inf), 
                      labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here

cTab_xg_u <- table(seg_u_full$sev_obs, predcrash_xg_u) 
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
  
  cTab_xg_u <- table(seg_u_full$sev_obs, predcrash_xg_u) 
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
  geom_hline(yintercept = 214)

ggplot(plot_data_u, aes(x=threshold, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(name = "Threshold for Severe Presence Prediction", breaks = seq(0,1, by = .1)) +
  scale_y_continuous(name = "Metric Value", breaks = seq(0,1, by = .1)) +
  labs(color = "Performance Metric") +
  theme(legend.position = "bottom")

#### WRITE LIME TABLES----
LIME_r_export <- explanation_gbm_r[,1:11]
len_r_seg <- length(unique(LIME_r_export$case))
case_ID <- as.data.table(cbind(1:len_r_seg, as.character(CRSP2_Unique_ID_r), seg_r$SevereCrashCount))
colnames(case_ID) <- c("case", "CRSP_ID", "Severe_Crash_Count")

LIME_r_export <- merge(LIME_r_export, case_ID, by = "case")

LIME_u_export <- explanation_gbm_u[,1:11]
fwrite(LIME_r_export, "clean_data/seg_r_LIME_output.csv")
fwrite(LIME_u_export, "clean_data/seg_u_LIME_output.csv")

