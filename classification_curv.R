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
library(tidymodels)
library(rsample)
library(Metrics)
#set dir
setwd("/Users/ryanloos/Capstone/")

#load data
curvs <- fread("clean_data/curv_for_model.csv")

#create classification var
curvs <- curvs[, sev_obs := 0]
curvs <- curvs[Severe_Crashes>0, sev_obs := 1]
#refactorize columns...
for(var in colnames(curvs)) {
  if (class(curvs[[var]]) == "character") {
    curvs[[var]] <- as.factor(curvs[[var]])
  }
}

CRSP2_Unique_ID <- curvs$CRSP2_Unique_Curve_ID

curvs[, CRSP2_Unique_Curve_ID := NULL]
curvs[, Severe_Crashes := NULL]
curvs[, Redraw_Flag := NULL]

#drop boring columns
drop_boring_columns(curvs)
#set 0/1 to factors

for(var in c("Delineation","Rumble_Strips","Mumble","Visual_Trap","Curve_Lighting")) {
    curvs[[var]] <- as.factor(curvs[[var]])
}

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


#curves----
set.seed(123)
#set up test train
#https://towardsdatascience.com/understanding-gradient-boosting-machines-9be756fe76ab

#run model
curv_xgb <- gbm(sev_obs~.,data=curvs,distribution="bernoulli",n.trees = 5000,
                 shrinkage = .001,interaction.depth = 6, cv.folds = 5)
print(curv_xgb)
summary(curv_xgb)
summary.gbm(curv_xgb)

for_plot <- summary(curv_xgb)

ggplot(for_plot, aes(x = rel.inf, y = reorder(var, rel.inf))) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  scale_x_continuous(name = "Relative Variable Weight") +
  scale_y_discrete(name = "Variable")

model_type(curv_xgb)

best_iter = gbm.perf(curv_xgb, method="cv")
best_iter
pred_xg <- predict_model.gbm(curv_xgb, newdata = curvs)
pred_xg <- pred_xg$pred


explainer_gbm <- lime(curvs, curv_xgb, n_bins = 5)
explanation_gbm <- explain(curvs, explainer_gbm, n_features = 10, n_labels = 1, n_permutations = 500, kernel_width = .75)
plot_features(explanation_gbm[1:19,]) + ggtitle("gbm")


#contingency table
threshhold_xg <- 0.1  # Set Y=1 when predicted probability exceeds this

#check for a more reasonable threshold
predcrash_xg <- cut(pred_xg, breaks=c(-Inf, threshhold_xg, Inf), 
                      labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here

cTab_xg <- table(curvs$sev_obs, predcrash_xg) 
addmargins(cTab_xg)

#accuracy
perc_xg <- sum(diag(cTab_xg)) / sum(cTab_xg)  # compute the proportion of correct classifications #catches lots of false
print(paste('Proportion correctly predicted = ', round(perc_xg,3))) 
#precision - predicting the positive class - this could be ok if we want false positives to examine.
prec_xg <- cTab_xg[2,2]/(cTab_xg[1,2]+cTab_xg[2,2])
print(paste('Precision (correctly predict positive) = ', round(prec_xg,3))) 
#precision - rate at which model captures true class.
recall_xg <- cTab_xg[2,2]/(cTab_xg[2,1]+cTab_xg[2,2])
print(paste('Recall (correctly capture positive) = ', round(recall_xg,3))) 

#test different thresholds
predperc_xg <- c()
predprec_xg <- c()
predrecall_xg <- c()
predsev_xg <- c()
for (i in 1:20){
  threshhold <- i/10/2  # Set Y=1 when predicted probability exceeds this
  predcrash_xg <- cut(pred_xg, breaks=c(-Inf, threshhold, Inf), 
                        labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here
  
  cTab_xg <- table(curvs$sev_obs, predcrash_xg) 
  addmargins(cTab_xg)
  
  p <- sum(diag(cTab_xg)) / sum(cTab_xg)  # compute the proportion of correct classifications
  s <- sum(cTab_xg[,2])
  prec <- cTab_xg[2,2]/(cTab_xg[1,2]+cTab_xg[2,2])
  recallcv <- cTab_xg[2,2]/(cTab_xg[2,1]+cTab_xg[2,2])
  #prcurv(paste('Proportion correctly predicted = ', p))
  predperc_xg <- c(predperc_xg,p) #accuracy
  predprec_xg <- c(predprec_xg,prec)
  predrecall_xg <- c(predrecall_xg, recallcv)
  predsev_xg <- c(predsev_xg, s) #predicted severe
  
}
predperc_xg
predprec_xg
predrecall_xg
predsev_xg

plot_data <- cbind(seq(.05,1, by = .05), predperc_xg, predprec_xg, predrecall_xg)
colnames(plot_data) <- c("threshold","accuracy", "precision", "recall")
plot_data <- melt(as.data.table(plot_data), id.vars = "threshold")

x = seq(.05,1, by = .05)
for_plot <- as.data.frame(cbind(predsev_xg, x))
ggplot(for_plot, aes(x = x, y = predsev_xg)) +
  geom_line()+
  geom_point()+
  theme_bw() +
  scale_x_continuous(name = "Threshold for Severe Presence Prediction", breaks = seq(0,1, by = .1)) +
  scale_y_continuous(name = "Segments Predicted to Have Severe Crashes") +
  geom_hline(yintercept = 157)

ggplot(plot_data, aes(x=threshold, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(name = "Threshold for Severe Presence Prediction", breaks = seq(0,1, by = .1)) +
  scale_y_continuous(name = "Metric Value", breaks = seq(0,1, by = .1)) +
  labs(color = "Performance Metric") +
  theme(legend.position = "bottom")

#git test
