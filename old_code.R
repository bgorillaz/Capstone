### OLD CODE ----
#### XGBOOST BASE ----
#https://www.r-bloggers.com/2016/03/an-introduction-to-xgboost-r-package/
#xgboost datacamp
#for test and train https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
####rural ----
# seg_r_small <- seg_r_full[, c("ADT_vpd", "Density_Curve", "Speed_Limit_mph", "Access_Density",
#                               "Edge_Risk", "sev_obs")] #only use this if you want to look at the factors used in CRSP
# seg_r_boost <- one_hot(seg_r_full)
# seg_r_outcome <- seg_r_boost$sev_obs
# seg_r_boost[, sev_obs := NULL]
# seg_r_boost <- as.matrix(seg_r_boost)
# 
# set.seed(100)
# xg_r <- xgb.cv(data = seg_r_boost, nrounds = 100, objective = "binary:logistic", nfold = 10, label = seg_r_outcome,
#                eta = .3, max_depth = 6, early_stopping_rounds = 25)
# elog_r <- xg_r$evaluation_log
# 
# elog_r %>%
#   summarize(ntrees.train = which.min(elog_r$train_error_mean),
#             ntrees.test = which.min(elog_r$test_error_mean))
# #n.trees.test = 17
# xgb_r <- xgboost(data = seg_r_boost, # training data as matrix
#                  label = seg_r_outcome,  # column of outcomes
#                  nrounds = 17,       # number of trees to build
#                  objective = "binary:logistic", # objective
#                  eta = .3,
#                  depth = 6,
#                  verbose = 0)  # silent
# #make predictions
# pred_xg_r <- predict(xgb_r, seg_r_boost)
# 
# #contingency table
# threshhold_xg_r <- 0.4  # Set Y=1 when predicted probability exceeds this
# #check for a more reasonable threshold
# 
# predcrash_xg_r <- cut(pred_xg_r, breaks=c(-Inf, threshhold_xg_r, Inf), 
#                       labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here
# 
# cTab_xg_r <- table(seg_r_outcome, predcrash_xg_r) 
# addmargins(cTab_xg_r)
# 
# perc_xg_r <- sum(diag(cTab_xg_r)) / sum(cTab_xg_r)  # compute the proportion of correct classifications
# print(paste('Proportion correctly predicted = ', round(perc_xg_r,3))) 
# 
# #test different thresholds
# predperc_xg_r <- c()
# predsev_xg_r <- c()
# for (i in 1:10){
#   threshhold <- i/10  # Set Y=1 when predicted probability exceeds this
#   predcrash_xg_r <- cut(pred_xg_r, breaks=c(-Inf, threshhold, Inf), 
#                         labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here
#   
#   cTab_xg_r <- table(seg_r_outcome, predcrash_xg_r) 
#   addmargins(cTab_xg_r)
#   
#   p <- sum(diag(cTab_xg_r)) / sum(cTab_xg_r)  # compute the proportion of correct classifications
#   s <- sum(cTab_xg_r[,2])
#   #print(paste('Proportion correctly predicted = ', p))
#   predperc_xg_r <- c(predperc_xg_r,p)
#   predsev_xg_r <- c(predsev_xg_r, s)
# }
# predperc_xg_r
# predsev_xg_r
# 
# plot(predperc_xg_r, type = 'o', main = "Accuracy by Acceptable Threshold", xlab = "Threshold (tenths)",
#      ylab = "Percent Correctly Predicted", col = c("Blue"), cex.axis = .70, cex.lab=.70, cex.main = .8)
# plot(predsev_xg_r, type = 'o', main = "Severe Predictions by Acceptable Threshold", xlab = "Threshold (tenths)",
#      ylab = "Percent Correctly Predicted", col = c("Blue"), cex.axis = .70, cex.lab=.70, cex.main = .8)
# abline(h = 346) #indicates that we should use a threshold of .3, accurately predicts .794 
# #threshold of .4 hits peak accuracy of 92.7
# #somewhere between .3 and .4 hits the right number of severe segments.
# #compare the contingency tables and make comparisons!!!
# 
# #feature importance
# importance_matrix_r <- xgb.importance(model = xgb_r) #can use this as realtive weights as well!
# xgb.plot.importance(importance_matrix_r)
# 
# ####urban ----
# seg_u_boost <- one_hot(seg_u_full)
# seg_u_outcome <- seg_u_boost$sev_obs
# seg_u_boost[, sev_obs := NULL]
# seg_u_boost <- as.matrix(seg_u_boost)
# 
# set.seed(100)
# xg_u <- xgb.cv(data = seg_u_boost, nrounds = 500, objective = "binary:logistic", nfold = 10, label = seg_u_outcome,
#                eta = .3, max_depth = 6, early_stopping_uounds = 25)
# elog_u <- xg_u$evaluation_log
# 
# elog_u %>%
#   summarize(ntrees.train = which.min(elog_u$train_error_mean),
#             ntrees.test = which.min(elog_u$test_error_mean))
# #n.trees.test = 6
# xgb_u <- xgboost(data = seg_u_boost, # training data as matrix
#                  label = seg_u_outcome,  # column of outcomes
#                  nrounds = 6,       # number of trees to build
#                  objective = "binary:logistic", # objective
#                  eta = .3,
#                  depth = 6,
#                  verbose = 0)  # silent
# #make predictions
# pred_xg_u <- predict(xgb_u, seg_u_boost)
# 
# seg_u_full[sev_obs >0, .N] #214
# #contingency table
# threshhold_xg_u <- 0.4  # Set Y=1 when predicted probability exceeds this
# #check for a more reasonable threshold
# 
# predcrash_xg_u <- cut(pred_xg_u, breaks=c(-Inf, threshhold_xg_u, Inf), 
#                       labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here
# 
# cTab_xg_u <- table(seg_u_outcome, predcrash_xg_u) 
# addmargins(cTab_xg_u)
# 
# perc_xg_u <- sum(diag(cTab_xg_u)) / sum(cTab_xg_u)  # compute the proportion of correct classifications
# print(paste('Proportion correctly predicted = ', round(perc_xg_u,3))) 
# 
# #test different thresholds
# predperc_xg_u <- c()
# predsev_xg_u <- c()
# for (i in 1:10){
#   threshhold <- i/10  # Set Y=1 when predicted probability exceeds this
#   predcrash_xg_u <- cut(pred_xg_u, breaks=c(-Inf, threshhold, Inf), 
#                         labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here
#   
#   cTab_xg_u <- table(seg_u_outcome, predcrash_xg_u) 
#   addmargins(cTab_xg_u)
#   
#   p <- sum(diag(cTab_xg_u)) / sum(cTab_xg_u)  # compute the proportion of correct classifications
#   s <- sum(cTab_xg_u[,2])
#   #print(paste('Proportion correctly predicted = ', p))
#   predperc_xg_u <- c(predperc_xg_u,p)
#   predsev_xg_u <- c(predsev_xg_u, s)
# }
# predperc_xg_u
# predsev_xg_u
# 
# plot(predperc_xg_u, type = 'o', main = "Accuracy by Acceptable Threshold", xlab = "Threshold (tenths)",
#      ylab = "Percent Correctly Predicted", col = c("Blue"), cex.axis = .70, cex.lab=.70, cex.main = .8)
# plot(predsev_xg_u, type = 'o', main = "Severe Predictions by Acceptable Threshold", xlab = "Threshold (tenths)",
#      ylab = "Percent Correctly Predicted", col = c("Blue"), cex.axis = .70, cex.lab=.70, cex.main = .8)
# abline(h = 214) 
# #threshold of .4 hits peak accuracy of 91.1
# #somewhere between .3 and .4 hits the right number of severe segments.
# #compare the contingency tables and make comparisons to logistic!!!
# 
# #feature importance
# importance_matrix_u <- xgb.importance(model = xgb_u) #can use this as realtive weights as well!
# xgb.plot.importance(importance_matrix_u)
# #sub category importance!
# 
# #### LIME ----
# #### rural ----
# #https://uc-r.github.io/lime
# #https://rpubs.com/omicsdata/gbm
# seg_r_boost <- as.data.table(seg_r_boost)
# #seg_r_boost[, sev_obs := seg_r_outcome]
# #### categorical variable explanation ---- 
# set.seed(123)
# explainer_caret_seg_r <- lime(seg_r_boost, xgb_r, n_bins = 5)
# class(explainer_caret_seg_r)
# summary(explainer_caret_seg_r)
# 
# explanation_caret_r <- explain(
#   x = seg_r_boost, 
#   explainer = explainer_caret_seg_r, 
#   n_permutations = 5000,
#   dist_fun = "gower",
#   kernel_width = .75,
#   n_features = 10, 
#   feature_select = "highest_weights", #can use different feature selection values
#   labels = 1)
# tibble::glimpse(explanation_caret_r)
# 
# head(explanation_caret_r,20)
# plot_features(explanation_caret_r[1:10,])
# #can get prediction value with [1,13]$prediction - gives same value as predict
# #probability value can be used to see which are closest to the threshold - would be interesting to see...
# 
# #### urban ----
# seg_u_boost <- as.data.table(seg_u_boost)
# #seg_u_boost[, sev_obs := seg_u_outcome]
# explainer_caret_seg_u <- lime(seg_u_boost, xgb_u, n_bins = 5)
# class(explainer_caret_u)
# summary(explainer_caret_seg_u)
# 
# explanation_caret_u <- explain(
#   x = seg_u_boost, 
#   explainer = explainer_caret_seg_u, 
#   n_permutations = 5000,
#   dist_fun = "gower",
#   kernel_width = .75,
#   n_features = 10, 
#   feature_select = "highest_weights",
#   labels = 1)
# tibble::glimpse(explanation_caret_u)
# 
# head(explanation_caret_u,20)
# plot_features(explanation_caret_u[11:20,])
# #plot_explanations(explanation_caret_u)
# #can get prediction value with [1,13]$prediction - gives same value as predict
# 
# #write out lime objects and send to data_prep_LIME file?

#### LOGISTIC BASE ---- 
#exponentiate the coefficients to get odds (e.g. 1.81 = 81% more likely)
#rural ----
summary(full_r <- glm(sev_obs ~ MVMT_5yrMillionVMT+Density_Curve+Context_Zone+Seg_CrossSection_Design+
                        Speed_Limit_mph+Access_Density+Lane_Width+Shoulder_Type+Shoulder_Width+Median_Type+
                        Median_Width+Edge_Risk+Edgeline_Striping+Centerline_Striping+ Road_Access_Count+
                        Commercial_Access_Count+Residential_Access_Count+Farm_Access_Count+Total_Access_County, data = seg_r_full,
                      family = "binomial")) #AIC 1485.2
#removed ADT, area type, xsection, design, rumble/mumble, and park, sidewalk, bike for rural
step(full_r, dir = "both") #run to see what minimizes AIC

summary(step_r <- glm(sev_obs ~ MVMT_5yrMillionVMT + Density_Curve + 
                        Seg_CrossSection_Design + Speed_Limit_mph + Access_Density + 
                        Median_Width, family = "binomial", data = seg_r_full)) #AIC 1484.4
#cross section, median width and road access count still not significant.
summary(step_r_small <- glm(sev_obs ~ MVMT_5yrMillionVMT + Density_Curve + 
                              Access_Density + Residential_Access_Count + Farm_Access_Count, 
                            family = "binomial", data = seg_r_full)) #AIC 1456.4
#try using just access count
summary(step_r_small <- glm(sev_obs ~ MVMT_5yrMillionVMT + Density_Curve + Speed_Limit_mph + Access_Density,
                            family = "binomial", data = seg_r_full)) #AIC 1490.7, all significant
#Not a very interesting model

#try same as count model
summary(count_r <- glm(sev_obs ~ MVMT_5yrMillionVMT + Density_Curve+Access_Density+Shoulder_Width+
                         Edge_Risk+Road_Access_Count+Commercial_Access_Count+
                         Residential_Access_Count+Farm_Access_Count, family = "binomial", data = seg_r_full)) #AIC 1462.
#Lowish AIC, more interesting - will use this model.

# run some predictions
#predict crash values
crashpredict <- predict(step_r_small, seg_r_full, type="response") #split into test and train?!
summary(crashpredict)

#contingency table
threshhold <- 0.3  # Set Y=1 when predicted probability exceeds this
#check for a more reasonable threshold
seg_r_full[sev_obs >0, .N] #346/1696 = .2, so 20% of segs have a severe crash.
#threshold of .5 only predicts 124... lets lower threshold.

predcrash <- cut(crashpredict, breaks=c(-Inf, threshhold, Inf), 
                 labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here

cTab <- table(seg_r_full$sev_obs, predcrash) 
addmargins(cTab)

perc <- sum(diag(cTab)) / sum(cTab)  # compute the proportion of correct classifications
print(paste('Proportion correctly predicted = ', round(perc,3))) 

#test different thresholds
predperc <- c()
predsev <- c()
for (i in 1:10){
  threshhold <- i/10  # Set Y=1 when predicted probability exceeds this
  predcrash <- cut(crashpredict, breaks=c(-Inf, threshhold, Inf), 
                   labels=c(0, 1))  # Y=1 is "Sp Rich>=4" here
  
  cTab <- table(seg_r_full$sev_obs, predcrash) 
  addmargins(cTab)
  
  p <- sum(diag(cTab)) / sum(cTab)  # compute the proportion of correct classifications
  s <- sum(cTab[,2])
  #print(paste('Proportion correctly predicted = ', p))
  predperc <- c(predperc,p)
  predsev <- c(predsev, s)
}
predperc
predsev

plot(predperc, type = 'o', main = "Accuracy by Acceptable Threshold", xlab = "Threshold (tenths)",
     ylab = "Percent Correctly Predicted", col = c("Blue"), cex.axis = .70, cex.lab=.70, cex.main = .8)
plot(predsev, type = 'o', main = "Severe Predictions by Acceptable Threshold", xlab = "Threshold (tenths)",
     ylab = "Percent Correctly Predicted", col = c("Blue"), cex.axis = .70, cex.lab=.70, cex.main = .8)
abline(h = 346) #indicates that we should use a threshold of .3, accurately predicts .794 

#urban ----
summary(full_u <- glm(sev_obs ~ MVMT_5yrMillionVMT+Density_Curve+
                        Context_Zone+Seg_CrossSection_Design+
                        Speed_Limit_mph+Access_Density+Lane_Width+Shoulder_Type+Shoulder_Width+
                        Median_Type+Median_Width+Edge_Risk+Edgeline_Striping+Centerline_Striping+Parking_Allowed+
                        Sidewalks+Bike_Lanes+Road_Access_Count+Residential_Access_Count+Farm_Access_Count+
                        Alley_Other_Access_Count+Total_Access_County, data = seg_u_full,
                      family = "binomial")) #AIC 665.8
#removed ADT, area type, xsection, design, rumble/mumble
step(full_u, dir = "both") #run to see what minimizes AIC

summary(step_u <- glm(formula = sev_obs ~ MVMT_5yrMillionVMT + Access_Density + 
                        Edge_Risk + Sidewalks + Road_Access_Count + Residential_Access_Count, 
                      family = "binomial", data = seg_u_full)) #AIC 629.6 #rez access count and acc den really close to significant
summary(step_u_small <- glm(formula = sev_obs ~ MVMT_5yrMillionVMT+ 
                              Edge_Risk + Sidewalks + Road_Access_Count, 
                            family = "binomial", data = seg_u_full)) #AIC 629.2 #just use the one above.
#try final count model
summary(count_u <- glm(formula = sev_obs ~ MVMT_5yrMillionVMT +Density_Curve + Access_Density + 
                         Median_Width + Edge_Risk + Parking_Allowed + Sidewalks + Bike_Lanes + 
                         Shoulder_Width + Road_Access_Count + Residential_Access_Count,
                       data = seg_u_full)) #AIC 693.13, if you remove the non-significant vars it looks like small step.

#Results - use step_u and count_r - make justification as to why!