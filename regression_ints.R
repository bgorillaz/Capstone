#load packages
library(data.table)
library(pscl)
library(MASS)
library(ndot.tools)
library(countreg)
library(cowplot)
library(ggplot2)

#set dir
setwd("/Users/ryanloos/Capstone/")
#load data
ints <- fread("clean_data/int_for_model.csv")
#refactorize columns...
for(var in colnames(ints)) {
  if (class(ints[[var]]) == "character") {
    ints[[var]] <- as.factor(ints[[var]])
  }
}

ints[, Max_Lanes_Cross := as.numeric(Max_Lanes_Cross)]
#split urban/rural
int_r <- ints[Area_Type %in% c("rural", "small town")]
int_r_id <- int_r$CRSP2_Unique_ID
int_r[, CRSP2_Unique_ID := NULL]

int_u <- ints[Area_Type %in% c("urban", "urban core", "suburban")]
int_u_id <- int_u$CRSP2_Unique_ID
int_u[, CRSP2_Unique_ID := NULL]

#### POISSON BASE ---- 
mean(int_r$Severe_Crashes) #.057
var(int_r$Severe_Crashes) #.066  #basic poisson might work!
mean(int_u$Severe_Crashes) #.19
var(int_u$Severe_Crashes) #.24

#rural
summary(fit_p_r <- glm(Severe_Crashes ~ Context_Zone + Total_Entering_ADT + Cross_Product +
                         Leg_Configuration + Alignment_Skew + Railroad_Crossing + Adjacent_Curve +
                         Adjacent_Development + Previous_Stop + Major_Speed_Limit + Maj1_Lane_Config,
                       data = int_r, family = poisson(link = "log")))
#AIC of 1760.5 Really unhelpful model...
sum(resid(fit_p_r, type = "pearson")^2) / (nrow(int_r) - length(coef(fit_p_r)))
pchisq(fit_p_r$deviance, df=fit_p_r$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(fit_p_r)
AIC(fit_p_r)
sum(dpois(0,fitted(fit_p_r)))
int_r[,.N, by = Severe_Crashes] #38

#urban
summary(fit_p_u <- glm(Severe_Crashes ~ Major_Speed_Limit + Minor_Speed_Limit + Context_Zone + Traffic_Control +
                         Total_Entering_ADT + Cross_Product + Leg_Configuration + Major_Division_Configuration + Alignment_Skew +
                         Adjacent_Development + Left_Turn_Phasing_Maj + Maj1_Lane_Config + Max_Lanes_Cross +
                         Sidewalk + Ped_Crossing, data = int_u, family = poisson(link = "log")))
#AIC 1499.9. Lane width, edge striping, Parking one side, ADT, Edge Risk, shoulder width, and design are all significant.
# Speed limit, context zone, and access density are not. Some subfactors are not - will need to test these individually.
sum(resid(fit_p_u, type = "pearson")^2) / (nrow(int_u) - length(coef(fit_p_u)))
pchisq(fit_p_u$deviance, df=fit_p_u$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(fit_p_u)
AIC(fit_p_u)
sum(dpois(0,fitted(fit_p_u)))
int_u[,.N, by = Severe_Crashes] #38

step(fit_p_u, dir ="both")

summary(step_p_u <- glm(formula = Severe_Crashes ~ Traffic_Control + Total_Entering_ADT + 
                          Leg_Configuration + Max_Lanes_Cross + Sidewalk + 
                          Major_Speed_Limit, family = poisson(link = "log"), data = int_u))

#### NEGATIVE BIOMIAL BASE ----
#rural
summary(fit_nb_r <- glm.nb(Severe_Crashes ~ Context_Zone + Total_Entering_ADT + Cross_Product +
                         Leg_Configuration + Alignment_Skew + Railroad_Crossing + Adjacent_Curve +
                         Adjacent_Development + Previous_Stop + Major_Speed_Limit + Maj1_Lane_Config,
                       data = int_r))
#AIC 1961.3. Speed Limit, ADT, Access and Curve Density are all significant. Edge Risk is not.
sum(resid(fit_nb_r, type = "pearson")^2) / (nrow(int_r) - length(coef(fit_nb_r)))
pchisq(fit_nb_r$deviance, df=fit_nb_r$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(fit_nb_r)
AIC(fit_nb_r)
sum(dnbinom(0, mu = fitted(fit_nb_r), size = fit_nb_r$theta)) #7294


#urban
summary(fit_nb_u <- glm.nb(Severe_Crashes ~ Major_Speed_Limit + Minor_Speed_Limit + Context_Zone + Traffic_Control +
                         Total_Entering_ADT + Cross_Product + Leg_Configuration + Major_Division_Configuration + Alignment_Skew +
                         Adjacent_Development + Left_Turn_Phasing_Maj + Max_Lanes_Cross +
                         Sidewalk + Ped_Crossing, data = int_u))
sum(resid(fit_nb_u, type = "pearson")^2) / (nrow(int_u) - length(coef(fit_nb_u)))
pchisq(fit_nb_u$deviance, df=fit_nb_u$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(fit_nb_u)
AIC(fit_nb_u)
sum(dnbinom(0, mu = fitted(fit_nb_u), size = fit_nb_u$theta)) #7294


#### ZERO INFLATED POISSON BASE ----
#rural
# summary(fit_zip_r <- zeroinfl(Severe_Crashes ~ Leg_Configuration + Alignment_Skew + Railroad_Crossing + Adjacent_Curve +
#                                 Adjacent_Development + Previous_Stop + Major_Speed_Limit|Total_Entering_ADT,
#                            data = int_r))
# #had to remove cross product, context zone, and major 1 leg configuration, used ADT as zero state
# fit_zip_r_null <- update(fit_zip_r, . ~ 1)
# 
# sum(resid(fit_zip_r, type = "pearson")^2) / (nrow(int_r) - length(coef(fit_zip_r)))
# pchisq(fit_zip_r$deviance, df=fit_zip_r$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
# logLik(fit_zip_r)
# AIC(fit_zip_r)
# sum(predict(fit_zip_r, type = "prob")[,1])
# 
# pchisq(2 * (logLik(fit_zip_r) - logLik(fit_zip_r_null)), df = 9, lower.tail = FALSE) #our model is significant
# vuong(fit_zip_r, fit_nb_r) #vuong test indicates that the zero inflated model is better than the NB model
# vuong(fit_zip_r, fit_p_r) #ZIP > P

#urban
# summary(fit_zip_u <- zeroinfl(Severe_Crashes ~ Major_Speed_Limit + Minor_Speed_Limit + Traffic_Control +
#                                 Alignment_Skew +
#                                 Adjacent_Development + Left_Turn_Phasing_Maj + Max_Lanes_Cross +
#                                 Sidewalk + Ped_Crossing|Total_Entering_ADT, data = int_u))
# 
# 
# fit_zip_u_null <- update(fit_zip_u, . ~ 1)
# pchisq(2 * (logLik(fit_zip_u) - logLik(fit_zip_u_null)), df = 11, lower.tail = FALSE) #our model is significant
# vuong(fit_zip_u, fit_nb_u) #vuong test indicates no evidence to use ZIP > NB
# vuong(fit_zip_u, fit_p_u) #vuong test indicates no evidence to use ZIP > P
# 
# sum(resid(fit_zip_u, type = "pearson")^2) / (nrow(int_u) - length(coef(fit_zip_u)))
# pchisq(fit_zip_u$deviance, df=fit_zip_u$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
# logLik(fit_zip_u)
# AIC(fit_zip_u)
# sum(predict(fit_zip_u, type = "prob")[,1])

#### ZERO INFLATED NEGATIVE BINOMIAL BASE ----
#https://stats.idre.ucla.edu/r/dae/zip/
#rural
#cannot run a ZINB with the data set.
# summary(fit_zinb_r <- zeroinfl(Severe_Crashes ~ Context_Zone |Total_Entering_ADT,
#                                data = int_r, dist = "negbin"))

# #had to remove nearly all the variables to test...
# fit_zinb_r_null <- update(fit_zinb_r, . ~ 1)
# 
# pchisq(2 * (logLik(fit_zinb_r) - logLik(fit_zinb_r_null)), df = 2, lower.tail = FALSE) #our model is significant
# vuong(fit_zinb_r, fit_nb_r) #vuong test indicates that the zero inflated model is better than the NB model
# vuong(fit_zinb_r, fit_test) #ZIP > P
# vuong(fit_zinb_r, fit_zip_r) #ZIP > P
# sum(resid(fit_zinb_r, type = "pearson")^2) / (nrow(int_r) - length(coef(fit_zinb_r))) #.908 handles dispersion
# 1 - pchisq(sum(resid(fit_zinb_r)^2), summary(fit_zinb_r)$df.residual) #near 1, great fit

#urban
# summary(fit_zinb_u <- zeroinfl(Severe_Crashes ~ Major_Speed_Limit + Minor_Speed_Limit + Traffic_Control +
#                                 Leg_Configuration + Major_Division_Configuration +
#                                 Adjacent_Development + Left_Turn_Phasing_Maj + Max_Lanes_Cross +
#                                 Sidewalk + Ped_Crossing|Total_Entering_ADT, data = int_u, dist = "negbin"))
#had to remove cross product, major 1 leg configuration, context zone, alignment skew
# fit_zinb_u_null <- update(fit_zinb_u, . ~ 1)
# pchisq(2 * (logLik(fit_zinb_u) - logLik(fit_zinb_u_null)), df = 11, lower.tail = FALSE) #our model is significant
# vuong(fit_zinb_u, fit_nb_u) #vuong test indicates no evidence to use zinb > NB
# vuong(fit_zinb_u, fit_p_u) #vuong test indicates no evidence to use zinb > P
# sum(resid(fit_zinb_u, type = "pearson")^2) / (nrow(int_u) - length(coef(fit_zinb_u))) #1.12 still over dispersed?
# 1 - pchisq(sum(resid(fit_zinb_u)^2), summary(fit_zinb_u)$df.residual) #near 1, great fit

#rootograms
#rural
root_p_r <- rootogram(fit_p_r, style = "hanging", plot = F)
root_nb_r <- rootogram(fit_nb_r, style = "hanging", plot = F)

autoplot(root_p_r, title = "Base Rural Segment - Poisson")
autoplot(root_nb_r)

xlims <- scale_x_continuous(breaks = 0:4, limits = c(-1,4))
plot_grid(autoplot(root_p_r) + ggtitle("Poisson") + xlims,autoplot(root_nb_r) + ggtitle("NB") + xlims)

int_base_r <- as.data.table(cbind(as.character(int_r_id), int_r$Severe_Crashes , round(fit_nb_r$fitted.values,3)))
fwrite(int_base_r, "clean_data/int_r_base_fitted.csv")

#urban
root_p_u <- rootogram(fit_p_u, style = "hanging", plot = F)
root_nb_u <- rootogram(fit_nb_u, style = "hanging", plot = F)

autoplot(root_p_u, title = "Base Rural Segment - Poisson")
autoplot(root_nb_u)

xlims <- scale_x_continuous(breaks = 0:4, limits = c(-1,5))
plot_grid(autoplot(root_p_u) + ggtitle("Poisson") + xlims,autoplot(root_nb_u) + ggtitle("NB") + xlims)

#### FINAL MODELS ---- 
int_full_r <- int_r[, c("Severe_Crashes", "Context_Zone", "Int_Type", 
                        "Total_Entering_ADT", "Design_Type", "Traffic_Control", 
                        "Leg_Configuration", "Major_Division_Configuration", "Minor_Division_Configuration", 
                        "Major_Surface_Type", "Minor_Surface_Type", "Alignment_Skew", 
                        "Railroad_Crossing", "Adjacent_Curve", "Adjacent_Development", 
                        "Lighting", "Previous_Stop", "Major_Speed_Limit", 
                        "Minor_Speed_Limit", "Flashers", "Overhead_Signal", "Maj1_Lane_Config", "Maj2_Lane_Config", 
                        "School_Crosswalk", "Legs")]
int_full_u <- int_u[, c("Severe_Crashes", "Area_Type", "Context_Zone", "Int_Type", 
                        "Total_Entering_ADT", "Design_Type", "Traffic_Control", 
                        "Leg_Configuration", "Major_Division_Configuration", "Minor_Division_Configuration", 
                        "Major_Surface_Type", "Minor_Surface_Type", "Alignment_Skew", 
                        "Railroad_Crossing", "Adjacent_Curve", "Adjacent_Development", 
                        "Street Parking", "Lighting", "Previous_Stop", "Major_Speed_Limit", 
                        "Minor_Speed_Limit", "Flashers", "Overhead_Signal", "Left_Turn_Phasing_Maj", 
                        "Left_Turn_Phasing_Min", "Maj1_Lane_Config", "Maj2_Lane_Config", 
                        "Sidewalk", "Refuge_Island", "Ped_Crossing", "Bike_Facility", 
                        "Ped_Indicator", "Transit_Adjacent", "School_Crosswalk", "Legs", 
                        "Max_Lanes_Cross")]

#rural
summary(full_nb_r <- glm.nb(Severe_Crashes ~ ., data = int_full_r))
step(full_nb_r, direction = "both")

summary(step_nb_r <- glm.nb(formula = Severe_Crashes ~ Total_Entering_ADT + Minor_Division_Configuration +
                              Major_Surface_Type + Minor_Surface_Type + Major_Speed_Limit +
                              Overhead_Signal + Legs, data = int_full_r, init.theta = 1.175872112, link = log))

summary(step_nb_r <- glm.nb(formula = Severe_Crashes ~ Total_Entering_ADT +
                              Major_Surface_Type + Minor_Surface_Type + Major_Speed_Limit +
                              Overhead_Signal + Legs, data = int_full_r, init.theta = 1.175872112, link = log))

sum(resid(step_nb_r, type = "pearson")^2) / (nrow(int_full_r) - length(coef(step_nb_r)))
pchisq(step_nb_r$deviance, df=step_nb_r$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(step_nb_r)
AIC(step_nb_r)
sum(dnbinom(0, mu = fitted(step_nb_r), size = step_nb_r$theta)) #7294

root_nb_r_full <- rootogram(step_nb_r, style = "hanging", plot = F)
autoplot(root_nb_r_full) +ggtitle("Full NB")

int_full_r <- as.data.table(cbind(as.character(int_r_id), int_r$Severe_Crashes , round(step_nb_r$fitted.values,3)))
fwrite(int_full_r, "clean_data/int_r_full_fitted.csv")

fit_compare_r <- fit_nb_r$fitted.values - step_nb_r$fitted.values
summary(fit_compare_r)
hist(fit_compare_r)
r_comp_range <- fit_compare_r[abs(fit_compare_r) <= .2] #1664

#urban
summary(full_nb_u <- glm.nb(Severe_Crashes ~ Total_Entering_ADT + Traffic_Control + Major_Division_Configuration + 
                              Minor_Division_Configuration + Major_Surface_Type +
                              Alignment_Skew + Railroad_Crossing + Adjacent_Curve + Adjacent_Development + 
                              `Street Parking` + Lighting +
                              Flashers + Overhead_Signal + Left_Turn_Phasing_Maj + 
                              Left_Turn_Phasing_Min +
                              Sidewalk + Refuge_Island + Ped_Crossing + Bike_Facility + 
                              Transit_Adjacent + School_Crosswalk + Legs + 
                              Max_Lanes_Cross, data = int_full_u))
step(full_nb_u, direction = "both")

summary(step_nb_u <- glm.nb(Severe_Crashes ~ Total_Entering_ADT + Traffic_Control +
                              Minor_Division_Configuration + Adjacent_Curve +
                              `Street Parking` + Left_Turn_Phasing_Maj + 
                              Sidewalk + Legs, data = int_full_u))
logLik(step_nb_u)
AIC(step_nb_u)

root_nb_u_full <- rootogram(step_nb_u, style = "hanging", plot = F)
autoplot(root_nb_u_full) +ggtitle("Full NB")
