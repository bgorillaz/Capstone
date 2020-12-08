#load packages
library(car)
library(data.table)
library(rpart)
library(rpart.plot)
library(pscl)
library(MASS)

#load data
segments <- fread("clean_data/seg_for_model.csv")

#remove NAs
segments <- na.omit(segments)
colnames(segments)[colSums(is.na(segments)) > 0]
#refactorize columns...
for(var in colnames(segments)) {
  if (class(segments[[var]]) == "character") {
    segments[[var]] <- as.factor(segments[[var]])
  }
}

segments[, CRSP2_Unique_ID := as.character(CRSP2_Unique_ID)]

#### FIT DECISION TREES ----
#run some quick decision trees
segments_noID <- segments[, CRSP2_Unique_ID := NULL]
explore_tree <- rpart(SevereCrashCount ~ ., data = segments_noID)
rpart.plot(explore_tree, digits = -1) # Plot the decision tree
#this is a great high level plot!
#try again with no crash count data
explore_tree2 <- rpart(SevereCrashCount ~ . - TotalCrashes_LaneDeparture - TotalCrashes_HO_SSO - TotalCrashCount -
                         TotalCrashes_MultiVehicle - TotalCrash_PED, data = segments_noID)
rpart.plot(explore_tree2, digits = -1) # Plot the decision tree
#road count, MVMT, Area Type
#try one more time with those removed
explore_tree3 <- rpart(SevereCrashCount ~ . - TotalCrashes_LaneDeparture - TotalCrashes_HO_SSO - TotalCrashCount -
                         TotalCrashes_MultiVehicle - TotalCrash_PED - Road_Access_Count - MVMT_5yrMillionVMT, data = segments_noID)
rpart.plot(explore_tree3, digits = -1) # Plot the decision tree
#Access counts, area type, cross section

#### FIT LINEAR MODELS ----
fitfull <- lm(SevereCrashCount ~ . - CRSP2_Unique_ID, data = segments, na.action=na.exclude)
#full model adjusted r2 of  .6761

#remove all count data
fit_nocrash <- lm(SevereCrashCount ~ . - CRSP2_Unique_ID - TotalCrashCount  -TotalCrashes_LaneDeparture -
                    TotalCrashes_MultiVehicle - TotalCrashes_HO_SSO - TotalCrash_PED, data = segments, na.action=na.exclude)
#adjusted r2 of .3818

#step model
step(fitfull,direction="both")

#run model given with step
fit_step <- lm(formula = SevereCrashCount ~ Segment_Cross_Section + Seg_CrossSection_Design + 
                 Mumble_Strips + Shoulder_Width + Edge_Risk + Edgeline_Striping + 
                 Residential_Access_Count + Farm_Access_Count + Access_Density + 
                 TotalCrashCount + TotalCrashes_LaneDeparture + TotalCrashes_MultiVehicle + 
                 TotalCrash_PED, data = segments, na.action = na.exclude)
#AIC of -2412.5 and adjusted r2 of .6767

#remove non-significant variables and try again...
fit_step_simple <- lm(formula = SevereCrashCount ~ Mumble_Strips + Edge_Risk + Edgeline_Striping + 
                 Residential_Access_Count + Farm_Access_Count + Access_Density + 
                 TotalCrashCount + TotalCrashes_LaneDeparture + TotalCrashes_MultiVehicle + 
                 TotalCrash_PED, data = segments, na.action = na.exclude)
#adjusted r2 of .6735, now edge risk isn't significant, neither is farm access

fit_step_simple2 <- lm(formula = SevereCrashCount ~ Mumble_Strips + Edgeline_Striping + 
                        Residential_Access_Count + Access_Density + 
                        TotalCrashCount + TotalCrashes_LaneDeparture + TotalCrashes_MultiVehicle + 
                        TotalCrash_PED, data = segments, na.action = na.exclude)
#adjusted r2 .6729, all significant - crash count data explaining most of the information is really boring!

#residuals are normally distributed (histogram of resids or Q-Q plot)
summary(fit_step_simple2)
plot(fit_step_simple2) 
#Q-Q plot shows that these are not normally distributed. #try to transform the variables?
#homeoscedastic (resids vs. predicted values) #above and below looks fine, but not across fitted values

#check for collinearity (cor matrix or VIF)
car::vif(fit_step_simple2) #multivehicle and total crash count have some collinearity, but not enough to warrant changing

#### FIT POISSON REGRESSION MODELS ----
fit_p <- glm(SevereCrashCount ~ ., data = segments_noID, family = poisson)
summary(fit_p)
#AIC is 3042 - how to interpret against a basic LM? Null deviance of 2891, residual 1649
sum(resid(fit_p, type = "pearson")^2) / (nrow(segments_noID) - length(coef(fit_p))) #.9279 # what does this mean?
# goodness of fit measure:
1 - pchisq(summary(fit_p)$deviance, summary(fit_p)$df.residual) # model does not fit the data (p < 0.05)

#can we step through this model?
step(fit_p,direction="both")

#step model gives AIC of 3011, residual deviance of 1679
fit_p_step <- glm(formula = SevereCrashCount ~ MVMT_5yrMillionVMT + Area_Type + 
                  Seg_CrossSection_Design + Speed_Limit_mph + Mumble_Strips + 
                  Shoulder_Type + Edge_Risk + Parking_Allowed + Sidewalks + 
                  Commercial_Access_Count + Residential_Access_Count + Farm_Access_Count + 
                  Access_Density + TotalCrashCount + TotalCrashes_LaneDeparture + 
                  TotalCrashes_HO_SSO + TotalCrash_PED, family = poisson, data = segments_noID)
summary(fit_p_step)
sum(resid(fit_p_step, type = "pearson")^2) / (nrow(segments_noID) - length(coef(fit_p_step))) #.92531 # what does this mean?
# goodness of fit measure:
1 - pchisq(summary(fit_p_step)$deviance, summary(fit_p_step)$df.residual) # model does not fit the data (p < 0.05)

#remove non-significant vars from data model? 
fit_p_step_small <- glm(formula = SevereCrashCount ~ MVMT_5yrMillionVMT + Area_Type + 
                    Speed_Limit_mph + Shoulder_Type + Edge_Risk + Parking_Allowed + Sidewalks + 
                    Commercial_Access_Count + Residential_Access_Count + Farm_Access_Count + 
                    Access_Density + TotalCrashCount + TotalCrashes_LaneDeparture + 
                    TotalCrashes_HO_SSO + TotalCrash_PED, family = poisson, data = segments_noID)
summary(fit_p_step_small)
sum(resid(fit_p_step_small, type = "pearson")^2) / (nrow(segments_noID) - length(coef(fit_p_step_small))) #.9238 # what does this mean?
# goodness of fit measure:
1 - pchisq(summary(fit_p_step_small)$deviance, summary(fit_p_step_small)$df.residual) # model does not fit the data (p < 0.05)

#try with no crash count information
fit_p_noCount <- glm(SevereCrashCount ~ . - TotalCrashCount - TotalCrashes_LaneDeparture - TotalCrashes_MultiVehicle -
                       TotalCrashes_HO_SSO - TotalCrash_PED, data = segments_noID, family = poisson)
summary(fit_p_noCount)

#step through without count
step(fit_p_noCount,direction="both")

#step model
fit_p_noCount_step <- glm(formula = SevereCrashCount ~ MVMT_5yrMillionVMT + Density_Curve + 
                            Area_Type + Segment_Cross_Section + Seg_CrossSection_Design + 
                            Speed_Limit_mph + Mumble_Strips + Lane_Width + Shoulder_Type + 
                            Shoulder_Width + Median_Type + Edge_Risk + Edgeline_Striping + 
                            Sidewalks + Road_Access_Count + Residential_Access_Count + 
                            Farm_Access_Count + Alley_Other_Access_Count + Access_Density, 
                          family = poisson, data = segments_noID)
summary(fit_p_noCount_step)

#remove non-significant
fit_p_noCount_step_small <- glm(formula = SevereCrashCount ~ MVMT_5yrMillionVMT + Density_Curve + 
                            Area_Type + Speed_Limit_mph + Lane_Width + Shoulder_Type + 
                            Shoulder_Width + Median_Type + Edge_Risk + Edgeline_Striping + 
                            Sidewalks + Road_Access_Count + Residential_Access_Count + 
                            Farm_Access_Count + Alley_Other_Access_Count + Access_Density, 
                          family = poisson, data = segments_noID)
summary(fit_p_noCount_step_small)
sum(resid(fit_p_noCount_step_small, type = "pearson")^2) / (nrow(segments_noID) - length(coef(fit_p_noCount_step_small))) #1.04 # what does this mean?
# goodness of fit measure:
1 - pchisq(summary(fit_p_noCount_step_small)$deviance, summary(fit_p_noCount_step_small)$df.residual) # model does not fit the data (p < 0.05)

#### FIT NEGATIVE BINOMIAL REGRESSION MODELS ----
fit_nb <- MASS::glm.nb(SevereCrashCount ~ ., data = segments_noID)
summary(fit_nb)
# Dispersion Statistic
sum(resid(fit_nb)^2) / (nrow(segments_noID) - length(coef(fit_nb)) + 1) # .6845
# goodness of fit measure:
1 - pchisq(summary(fit_nb)$deviance, summary(fit_nb)$df.residual)

#do other reduced models? No count
fit_nb2 <- MASS::glm.nb(SevereCrashCount ~ . - TotalCrashCount - TotalCrashes_LaneDeparture - TotalCrashes_MultiVehicle -
                          TotalCrashes_HO_SSO - TotalCrash_PED, data = segments_noID)
summary(fit_nb2)
# Dispersion Statistic
sum(resid(fit_nb2)^2) / (nrow(segments_noID) - length(coef(fit_nb2)) + 1) # .6947
# goodness of fit measure:
1 - pchisq(summary(fit_nb2)$deviance, summary(fit_nb2)$df.residual)


####BELOW HERE NOT WORKING ----
#### FIT ZERO INFLATED POISSON REGRESSION MODELS ----
fit_zip <- pscl::zeroinfl(SevereCrashCount ~ MVMT_5yrMillionVMT + Density_Curve + 
                            Area_Type + Speed_Limit_mph + Lane_Width + Shoulder_Type + 
                            Shoulder_Width + Median_Type + Edge_Risk + Edgeline_Striping + 
                            Sidewalks + Road_Access_Count + Residential_Access_Count + 
                            Farm_Access_Count + Alley_Other_Access_Count + Access_Density, data = segments_noID, dist = "poisson")
summary(fit_zip) #works with a simplified model? need the bar?! This controls... what is inflated?
# Dispersion Statistic
sum(resid(fit_zip, type = "pearson")^2) / (nrow(segments_noID) - length(coef(fit_zip))) # .8444
# goodness of fit:
1 - pchisq(sum(resid(fit_zip)^2), summary(fit_zip)$df.residual)

#### FIT ZERO INFLATED NEGATIVE BINOMIAL REGRESSION MODELS ----
fit_zinb <- pscl::zeroinfl(SevereCrashCount ~ MVMT_5yrMillionVMT + Density_Curve + 
                             Area_Type + Speed_Limit_mph + Lane_Width + Shoulder_Type + 
                             Shoulder_Width + Median_Type + Edge_Risk + Edgeline_Striping + 
                             Sidewalks + Road_Access_Count + Residential_Access_Count + 
                             Farm_Access_Count + Alley_Other_Access_Count + Access_Density, data = segments_noID, dist = "negbin")
summary(fit_zinb)
# Dispersion Statistic
sum(resid(fit_zinb, type = "pearson")^2) / (nrow(segments_noID) - length(coef(fit_zinb)) + 1) # .8414
# goodness of fit:
1 - pchisq(sum(resid(fit_zinb)^2), summary(fit_zinb)$df.residual)

#### Regsubsets???? Instead of step??? Use on best type of model?

#### Iteratively weighted least squares???? ----
