#load packages
library(car)
library(data.table)
library(rpart)
library(rpart.plot)
library(pscl)
library(MASS)
library(AER)
#set dir
setwd("/Users/ryanloos/Capstone/")

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

segments[, CRSP2_Unique_ID := NULL]
segments[, .N, by = SevereCrashCount]

#### FIT POISSON REGRESSION MODELS ----
#interpreting the coefficients: https://www.r-bloggers.com/2018/10/generalized-linear-models-understanding-the-link-function/
#1/exp(coef(fit_p)[2]) change in value causes an estimated X times change in dependent. Model is multiplicative.
fit_p <- glm(SevereCrashCount ~ ., data = segments, family = poisson(link = "log")) #log is the default link function
summary(fit_p) #AIC is 3042 - how to interpret against a basic LM? Null deviance of 2891, residual 1649
pchisq(fit_p$deviance, df=fit_p$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.

#check other assumptions? mean = variance?
mean(segments$SevereCrashCount) #0.37
var(segments$SevereCrashCount) #1.00 #variance > mean
par(mfrow=c(2,2))
plot(fit_p)
fit_p$deviance/fit_p$df.residual #.73, not overdispersed?
dispersiontest(fit_p) #reject null that data is not overdispersed.
#dispersion statistic .93
sum(resid(fit_p, type = "pearson")^2) / (nrow(segments) - length(coef(fit_p))) #square of all residuals/(2310-67) #obs-number of coefs
dispersiontest(fit_p) #pval .09, dispersion of 1.03.
#could just be too many predictors

#with less predictors
fit_p2 <- glm(SevereCrashCount ~ . - Seg_CrossSection_Design - Median_Type - Segment_Cross_Section -
                Rumble_Strips - Bike_Lanes - Area_Type - Context_Zone - Shoulder_Width - Median_Width -
                Parking_Allowed - Edgeline_Striping - Centerline_Striping - Mumble_Strips,
             data = segments, family = poisson(link = "log")) #log is the default link function
summary(fit_p2)
dispersiontest(fit_p2)

fit_p_step <- glm(formula = SevereCrashCount ~ MVMT_5yrMillionVMT + Area_Type + 
                    Seg_CrossSection_Design + Speed_Limit_mph + Mumble_Strips + 
                    Shoulder_Type + Edge_Risk + Parking_Allowed + Sidewalks + 
                    Commercial_Access_Count + Residential_Access_Count + Farm_Access_Count + 
                    Access_Density + TotalCrashCount + TotalCrashes_LaneDeparture + 
                    TotalCrashes_HO_SSO + TotalCrash_PED, family = poisson(link = "log"), 
                  data = segments)
summary(fit_p_step)
dispersiontest(fit_p_step)

#Vuong for non-nested inflated models!

#removing terms could cause overdispersion
#test only the terms that were critical for segments? urban/rural?
#to compare nested models look at the stats textbook page 286 X2 = -2[LL(rest)-LL(unres)]
# odTest(fit_p) # this can be used to test the NB model
#can we step through this model?
step(fit_p,direction="both")

#conclusion: Poisson model assumptions not met. Mean and variance are not equal and there is a preponderance of zeros.

#### FIT NEGATIVE BINOMIAL ----
fit_nb <- glm.nb(SevereCrashCount ~ ., data = segments)
summary(fit_nb) #AIC 3039
pchisq(fit_nb$deviance, df=fit_nb$df.residual, lower.tail=F)
#dispersion statistic .87
sum(resid(fit_nb, type = "pearson")^2) / (nrow(segments) - length(coef(fit_nb)))
fit_nb$deviance/fit_nb$df.residual #.68, not over dispersed?

#compare poisson and NB
#https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
#can use the above to test factor variables if only a few levels are significant
pchisq(2 * (logLik(fit_nb) - logLik(fit_p)), df = 1, lower.tail = FALSE) #df equals 1 if models have same # of predictors
#strongly suggests that the negative binomial model, estimating the dispersion parameter is more appropriate than the poisson.
hist(fit_nb$residuals) #not normally distributed.
#test for a smaller model
testDispersion

fit_nullNB <- glm.nb(SevereCrashCount ~ 1, data = segments)
sR2 <- (logLik(fit_nullNB)-logLik(fit_nb))/logLik(fit_nullNB) #psuedo R2 for NB model.

#is there a test for dispersion that indicates if you should use a zero inflated model?
# add exposure variable using 'offset' argument?
# other residual checks for NB model using $resid
# https://stats.stackexchange.com/questions/341655/residual-diagnostics-for-negative-binomial-regression
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

#### ZERO INFLATED POISSON
#these are good when data is not over dispersed (variance not much larger than mean)


#### ZERO INFLATED NEGATIVE BINOMIAL ----




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
pchisq(fit_p_step$deviance, df=fit_p_step$df.residual, lower.tail=F)

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

#### rootograms?
