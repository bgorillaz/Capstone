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
segs <- fread("clean_data/seg_for_model.csv")
#refactorize columns...
for(var in colnames(segs)) {
  if (class(segs[[var]]) == "character") {
    segs[[var]] <- as.factor(segs[[var]])
  }
}

#split urban/rural
seg_r <- segs[Area_Type %in% c("rural", "small town")]
seg_r_id <- seg_r$CRSP2_Unique_ID
seg_r[, CRSP2_Unique_ID := NULL]

seg_u <- segs[Area_Type %in% c("urban", "urban core", "suburban")]
seg_u_id <- seg_u$CRSP2_Unique_ID
seg_u[, CRSP2_Unique_ID := NULL]

#### POISSON BASE ---- 
mean(seg_r$SevereCrashCount) #.26
var(seg_r$SevereCrashCount) #.33
mean(seg_u$SevereCrashCount) #.72
var(seg_u$SevereCrashCount) #2.74
#mean is not equal to variance for either model should not use. Might be close for rural
#rural
summary(fit_p_r <- glm(SevereCrashCount ~ Speed_Limit_mph + ADT_vpd + Total_Access_County +
                         Density_Curve + Edge_Risk, data = seg_r, family = poisson(link = "log")))
#AIC of 1967.3. Speed Limit, ADT, Access and Curve Density are all significant. Edge Risk is not.
sum(resid(fit_p_r, type = "pearson")^2) / (nrow(seg_r) - length(coef(fit_p_r)))
pchisq(fit_p_r$deviance, df=fit_p_r$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(fit_p_r)
AIC(fit_p_r)
sum(dpois(0,fitted(fit_p_r)))
seg_r[,.N, by = SevereCrashCount]

#urban
summary(fit_p_u <- glm(SevereCrashCount ~ Speed_Limit_mph + Context_Zone + Lane_Width +
                         Edgeline_Striping + Parking_Allowed + ADT_vpd + Access_Density + Edge_Risk +
                       Shoulder_Width + Seg_CrossSection_Design, data = seg_u, family = poisson(link = "log")))
#AIC 1499.9. Lane width, edge striping, Parking one side, ADT, Edge Risk, shoulder width, and design are all significant.
# Speed limit, context zone, and access density are not. Some subfactors are not - will need to test these individually.
sum(resid(fit_p_u, type = "pearson")^2) / (nrow(seg_u) - length(coef(fit_p_u)))
pchisq(fit_p_u$deviance, df=fit_p_u$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(fit_p_u)
AIC(fit_p_u)
sum(dpois(0,fitted(fit_p_u)))
seg_u[,.N, by = SevereCrashCount]


#### NEGATIVE BIOMIAL BASE ----
#rural
summary(fit_nb_r <- glm.nb(SevereCrashCount ~ Speed_Limit_mph + ADT_vpd + Total_Access_County +
                         Density_Curve + Edge_Risk, data = seg_r))
#AIC 1961.3. Speed Limit, ADT, Access and Curve Density are all significant. Edge Risk is not.
sum(resid(fit_nb_r, type = "pearson")^2) / (nrow(seg_r) - length(coef(fit_nb_r))) #under 1
1 - pchisq(sum(resid(fit_nb_r)^2), summary(fit_nb_r)$df.residual) #large pval, great fit
pchisq(2 * (logLik(fit_nb_r) - logLik(fit_p_r)), df = 1, lower.tail = FALSE) #significant evidence that NB model > Poisson

logLik(fit_nb_r)
AIC(fit_nb_r)
sum(dnbinom(0, mu = fitted(fit_nb_r), size = fit_nb_r$theta)) #7294

#urban
summary(fit_nb_u <- glm.nb(SevereCrashCount ~ Speed_Limit_mph + Context_Zone + Lane_Width +
                         Edgeline_Striping + Parking_Allowed + ADT_vpd + Access_Density + Edge_Risk +
                         Shoulder_Width + Seg_CrossSection_Design, data = seg_u))
sum(resid(fit_nb_u, type = "pearson")^2) / (nrow(seg_u) - length(coef(fit_nb_u))) #1.014 very close to 1
#AIC 1318.8 Edge striping, Parking one side, ADT, Edge Risk, shoulder width, and design are all significant.
# Speed limit, context zone, lane width and access density are not. Some subfactors are not - will need to test these individually.
1 - pchisq(sum(resid(fit_nb_u)^2), summary(fit_nb_u)$df.residual) #large pval, great fit
pchisq(2 * (logLik(fit_nb_u) - logLik(fit_p_u)), df = 1, lower.tail = FALSE) #significant evidence that NB model > Poisson

logLik(fit_nb_u)
AIC(fit_nb_u)
sum(dnbinom(0, mu = fitted(fit_nb_u), size = fit_nb_u$theta)) #7294


#### ZERO INFLATED POISSON BASE ----
#rural
summary(fit_zip_r <- zeroinfl(SevereCrashCount ~ Speed_Limit_mph + Total_Access_County +
                             Density_Curve + Edge_Risk | ADT_vpd, data = seg_r))
fit_zip_r_null <- update(fit_zip_r, . ~ 1)
sum(resid(fit_zip_r, type = "pearson")^2) / (nrow(seg_r) - length(coef(fit_zip_r))) #.868 handles dispersion
1 - pchisq(sum(resid(fit_zip_r)^2), summary(fit_zip_r)$df.residual) #near 1, great fit

pchisq(2 * (logLik(fit_zip_r) - logLik(fit_zip_r_null)), df = 5, lower.tail = FALSE) #our model is significant
vuong(fit_zip_r, fit_nb_r) #vuong test indicates that the zero inflated model is better than the NB model

logLik(fit_zip_r)
AIC(fit_zip_r)
sum(predict(fit_zip_r, type = "prob")[,1])

#urban
summary(fit_zip_u <- zeroinfl(SevereCrashCount ~ Speed_Limit_mph + Context_Zone + Lane_Width +
                             Edgeline_Striping + Parking_Allowed  + Access_Density + Edge_Risk +
                             Shoulder_Width + Seg_CrossSection_Design | ADT_vpd, data = seg_u))
fit_zip_u_null <- update(fit_zip_u, . ~ 1)
sum(resid(fit_zip_u, type = "pearson")^2) / (nrow(seg_u) - length(coef(fit_zip_u))) #1.246 doesn't handle dispersion
1 - pchisq(sum(resid(fit_zip_u)^2), summary(fit_zip_u)$df.residual) #small p, not a good fit

pchisq(2 * (logLik(fit_zip_u) - logLik(fit_zip_u_null)), df = 10, lower.tail = FALSE) #our model is significant
vuong(fit_zip_u, fit_nb_u) #vuong test indicates that NB model is better than the ZIP model

logLik(fit_zip_u)
AIC(fit_zip_u)
sum(predict(fit_zip_u, type = "prob")[,1])


#### ZERO INFLATED NEGATIVE BINOMIAL BASE ----
#rural
summary(fit_zinb_r <- zeroinfl(SevereCrashCount ~ Speed_Limit_mph + Total_Access_County +
                                Density_Curve + Edge_Risk | ADT_vpd, data = seg_r, dist = "negbin"))
fit_zinb_r_null <- update(fit_zinb_r, . ~ 1)
sum(resid(fit_zinb_r, type = "pearson")^2) / (nrow(seg_r) - length(coef(fit_zinb_r)) + 1) #.86, handles dispersion
1 - pchisq(sum(resid(fit_zinb_r)^2), summary(fit_zinb_r)$df.residual) #near 1, great fit
pchisq(2 * (logLik(fit_zinb_r) - logLik(fit_zinb_r_null)), df = 5, lower.tail = FALSE) #our model is significant
vuong(fit_zinb_r, fit_zip_r) #vuong test indicates that there is no evidence to take the ZINB > ZIP
vuong(fit_zinb_r, fit_nb_r) #vuong test indicates that there is no evidence to take the ZINB > ZIP

logLik(fit_zinb_r)
AIC(fit_zinb_r)
sum(predict(fit_zinb_r, type = "prob")[,1]) #7292.4

#urban
summary(fit_zinb_u <- zeroinfl(SevereCrashCount ~ Speed_Limit_mph + Context_Zone + Lane_Width +
                                Edgeline_Striping + Parking_Allowed  + Access_Density + Edge_Risk +
                                Shoulder_Width + Seg_CrossSection_Design | MVMT_5yrMillionVMT, data = seg_u, dist = "negbin"))
#had to use a difference exposure measure - used MVMT instead of ADT.
fit_zinb_u_null <- update(fit_zinb_u, . ~ 1)
pchisq(2 * (logLik(fit_nb_u) - logLik(fit_zinb_u)), df = 2, lower.tail = FALSE) #our model is significant
vuong(fit_zinb_u, fit_nb_u) #vuong test indicates  ZINB model > NB
vuong(fit_zinb_u, fit_zip_u)
#checked this comparison again if using MVMT in NB model - same result.
sum(resid(fit_zinb_u, type = "pearson")^2) / (nrow(seg_u) - length(coef(fit_zinb_u))) #.82 #handles dispersion well!
1 - pchisq(sum(resid(fit_zinb_u)^2), summary(fit_zinb_u)$df.residual) #near 1, great fit

logLik(fit_zinb_u)
AIC(fit_zinb_u)
sum(predict(fit_zinb_u, type = "prob")[,1]) #7292.4

seg_base_u <- as.data.table(cbind(as.character(seg_u_id), seg_u$SevereCrashCount , round(fit_zinb_u$fitted.values,3)))
fwrite(seg_base_u, "clean_data/seg_u_base_fitted.csv")

#### BASE RESULTS ----
# Looking only at those variables that were used to assign rankings, the best fitting models are:
# Rural = ZIP and Urban = ZINB #confirmed with page 298 of stats book. Urban might be ZIP, but the dispersion and fit tests didn't look as good.
# Come back and interpret the coefficient findings for the report.

#rural
vuong(fit_zinb_r, fit_nb_r) #raw = 2.03 #sig evidence to use zinb > nb
1/fit_nb_r$theta #.32
#vuong statistic is > 1.96 and the alpha value is < abs(1.96). This indicates a zip model.

vuong(fit_zinb_r, fit_zip_r) #no evidence to use zinb > zip
vuong(fit_zip_r, fit_nb_r) #significant evidence to use zip > nb
#rural should use zip

seg_base_r <- as.data.table(cbind(as.character(seg_r_id), seg_r$SevereCrashCount , round(fit_zip_r$fitted.values,3)))
fwrite(seg_base_r, "clean_data/seg_r_base_fitted.csv")

#urban
vuong(fit_zinb_u, fit_nb_u) #raw = 4.08 #sig evidence to use zinb > nb
1/fit_nb_u$theta #1.35
#vuong statistic is > 1.96 and the alpha value is < abs(1.96). This indicates a zip model.

vuong(fit_zinb_u, fit_zip_u) #significant evidence to use zinb > zip
vuong(fit_zip_u, fit_nb_u) #significant evidence to use zip > nb

#make case of urban to use zinb

#rootograms
root_p_r <- rootogram(fit_p_r, style = "hanging", plot = F)
root_nb_r <- rootogram(fit_nb_r, style = "hanging", plot = F)
root_zip_r <- rootogram(fit_zip_r, style = "hanging", plot = F)
root_zinb_r <- rootogram(fit_zinb_r, style = "hanging", plot = F)

autoplot(root_p_r, title = "Base Rural Segment - Poisson")
autoplot(root_nb_r)
autoplot(root_zip_r)
autoplot(root_zinb_r)
xlims <- scale_x_continuous(breaks = 0:6, limits = c(-1,6))
plot_grid(autoplot(root_p_r) + ggtitle("Poisson") + xlims,autoplot(root_nb_r) + ggtitle("NB") + xlims,
          autoplot(root_zip_r) + ggtitle("ZIP") + xlims,autoplot(root_zinb_r) + ggtitle("ZINB") + xlims)

root_p_u <- rootogram(fit_p_u, style = "hanging", plot = F)
root_nb_u <- rootogram(fit_nb_u, style = "hanging", plot = F)
root_zip_u <- rootogram(fit_zip_u, style = "hanging", plot = F)
root_zinb_u <- rootogram(fit_zinb_u, style = "hanging", plot = F)

autoplot(root_p_u)
autoplot(root_nb_u)
autoplot(root_zip_u)
autoplot(root_zinb_u)

xlims_u <- scale_x_continuous(breaks = 0:9, limits = c(-1,9)) #breaks = 0:6, limits = c(-1,6)
plot_grid(autoplot(root_p_u) + ggtitle("Poisson") + xlims_u,autoplot(root_nb_u) + ggtitle("NB") + xlims_u,
          autoplot(root_zip_u) + ggtitle("ZIP") + xlims_u,autoplot(root_zinb_u) + ggtitle("ZINB") + xlims_u)

#### FULL MODEL ---- 
#remove the crash counts - looking for characteristics not historical trends.
seg_r_full <- seg_r[, c("MVMT_5yrMillionVMT", "ADT_vpd", "Density_Curve", "Area_Type", 
                        "Context_Zone", "Segment_Cross_Section", "Segment_Design", "Seg_CrossSection_Design", 
                        "Speed_Limit_mph", "Access_Density", "Rumble_Strips", "Mumble_Strips", 
                        "Lane_Width", "Shoulder_Type", "Shoulder_Width", "Median_Type", 
                        "Median_Width", "Edge_Risk", "Edgeline_Striping", "Centerline_Striping", 
                        "Parking_Allowed", "Sidewalks", "Bike_Lanes", "Road_Access_Count", 
                        "Commercial_Access_Count", "Residential_Access_Count", "Farm_Access_Count", 
                        "Alley_Other_Access_Count", "SevereCrashCount")]
seg_u_full <- seg_u[, c("MVMT_5yrMillionVMT", "ADT_vpd", "Density_Curve", "Area_Type", 
                        "Context_Zone", "Segment_Cross_Section", "Segment_Design", "Seg_CrossSection_Design", 
                        "Speed_Limit_mph", "Access_Density", "Rumble_Strips", "Lane_Width", 
                        "Shoulder_Type", "Shoulder_Width", "Median_Type", "Median_Width", 
                        "Edge_Risk", "Edgeline_Striping", "Centerline_Striping", "Parking_Allowed", 
                        "Sidewalks", "Bike_Lanes", "Road_Access_Count", "Commercial_Access_Count", 
                        "Residential_Access_Count", "Farm_Access_Count", "Alley_Other_Access_Count", 
                        "SevereCrashCount")]

#rural ---- 
summary(full_zip_r <- zeroinfl(SevereCrashCount ~ Density_Curve+
                              Speed_Limit_mph+Access_Density+
                              Shoulder_Width+Median_Type+
                              Median_Width+Edge_Risk+Edgeline_Striping+Centerline_Striping+
                              Road_Access_Count+Commercial_Access_Count+
                              Residential_Access_Count+Farm_Access_Count|MVMT_5yrMillionVMT,
                            data = seg_r_full))
step(full_zip_r, direction = "both")
# summary(step_zip_r_test <- zeroinfl(formula = SevereCrashCount ~ Density_Curve + Speed_Limit_mph + Access_Density + Shoulder_Width + Median_Type + Median_Width + 
#                               Edge_Risk + Edgeline_Striping + Centerline_Striping + Road_Access_Count + Commercial_Access_Count + Residential_Access_Count + 
#                               Farm_Access_Count|MVMT_5yrMillionVMT, data = seg_r_full))

summary(step_zip_r <- zeroinfl(SevereCrashCount ~ Density_Curve+Shoulder_Width+
                                 Edge_Risk+Road_Access_Count+ Access_Density +
                                 Residential_Access_Count+Farm_Access_Count|MVMT_5yrMillionVMT,
                               data = seg_r_full))

sum(resid(step_zip_r, type = "pearson")^2) / (nrow(seg_r_full) - length(coef(step_zip_r))) #.868 handles dispersion
1 - pchisq(sum(resid(step_zip_r)^2), summary(step_zip_r)$df.residual) #near 1, great fit

vuong(fit_zip_r, step_zip_r) #vuong test indicates that the zero inflated model is better than the NB model

logLik(step_zip_r)
AIC(step_zip_r)
sum(predict(step_zip_r, type = "prob")[,1])

#test our full model vs our base model
vuong(step_zip_r, fit_zip_r) #use our additional data.
root_zip_r_full <- rootogram(step_zip_r, style = "hanging", plot = F)
autoplot(root_zip_r_full) + ggtitle("Full ZIP") + scale_x_continuous(breaks = 0:5)

fit_compare_r <- step_zip_r$fitted.values - fit_zip_r$fitted.values
summary(fit_compare_r)
hist(fit_compare_r)
r_comp_range <- fit_compare_r[abs(fit_compare_r) <= .2] #1664

seg_full_r <- as.data.table(cbind(as.character(seg_r_id), round(step_zip_r$fitted.values,3)))
fwrite(seg_full_r, "clean_data/seg_r_full_fitted.csv")


#urban ----
summary(full_zinb_u <- zeroinfl(SevereCrashCount ~ Lane_Width+Sidewalks+Bike_Lanes+Road_Access_Count+
                                  Residential_Access_Count|MVMT_5yrMillionVMT, data = seg_u_full, dist = 'negbin'))
#removed segment cross section design, only significant one was one-way and there are few observations with that characteristic

# summary(full_zinb_edge <- zeroinfl(SevereCrashCount ~ Edgeline_Striping|MVMT_5yrMillionVMT, data = seg_u_full, dist = 'negbin'))

# summary(full_zip_u <- zeroinfl(SevereCrashCount ~ Lane_Width+Sidewalks+Bike_Lanes+Road_Access_Count+
#                                       Residential_Access_Count|MVMT_5yrMillionVMT, data = seg_u_full))

vuong(full_zinb_u, full_zip_u) #.06 at a .1 level of significance, use the ZINB
vuong(full_zinb_u, fit_zinb_u) #use the stepwise model

sum(resid(full_zinb_u, type = "pearson")^2) / (nrow(seg_u_full) - length(coef(full_zinb_u))) #.868 handles dispersion
1 - pchisq(sum(resid(full_zinb_u)^2), summary(full_zip_u)$df.residual) #near 1, great fit

logLik(full_zinb_u)
AIC(full_zinb_u)
sum(predict(full_zinb_u, type = "prob")[,1])

root_zinb_u_full <- rootogram(full_zinb_u, style = "hanging", plot = F)
autoplot(root_zinb_u_full) + ggtitle("Full ZINB") + scale_x_continuous(breaks = 0:9)

seg_full_u <- as.data.table(cbind(as.character(seg_u_id), round(full_zinb_u$fitted.values,3)))
fwrite(seg_full_u, "clean_data/seg_u_full_fitted.csv")

fit_compare_u <- fit_zinb_u$fitted.values - full_zinb_u$fitted.values
summary(fit_compare_u)
hist(fit_compare_u)
u_comp_range <- fit_compare_u[abs(fit_compare_u) <= .2] #1664
#https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/
#use mcfaddens adjusted

# pR2(step_zinb_u)

# full_nb_u <- glm.nb(SevereCrashCount ~ Density_Curve+Area_Type+
#                       Context_Zone+Segment_Cross_Section+Segment_Design+Seg_CrossSection_Design+
#                       Speed_Limit_mph+Access_Density+Rumble_Strips+Lane_Width+
#                       Shoulder_Type+Shoulder_Width+Median_Type+Median_Width+
#                       Edge_Risk+Edgeline_Striping+Centerline_Striping+Parking_Allowed+
#                       Sidewalks+Bike_Lanes+Road_Access_Count+Commercial_Access_Count+
#                       Residential_Access_Count+Farm_Access_Count+Alley_Other_Access_Count+MVMT_5yrMillionVMT,
#                     data = seg_u_full)
# 
# step(full_nb_u, direction = "both")
# 
# summary(full_zinb_test <- zeroinfl(SevereCrashCount ~  Speed_Limit_mph + 
#                                          Edge_Risk + Edgeline_Striping + Road_Access_Count + 
#                                          Segment_Design + Centerline_Striping | MVMT_5yrMillionVMT,
#                                        data = seg_u_full, dist = 'negbin'))


