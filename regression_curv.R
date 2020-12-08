#load packages
library(data.table)
library(pscl)
library(MASS)
library(ndot.tools)

#set dir
setwd("/Users/ryanloos/Capstone/")

#load data
curv <- fread("clean_data/curv_for_model.csv")
#refactorize columns...
for(var in colnames(curv)) {
  if (class(curv[[var]]) == "character") {
    curv[[var]] <- as.factor(curv[[var]])
  }
}

curv_id <- curv$CRSP2_Unique_Curve_ID
curv[, CRSP2_Unique_Curve_ID := NULL]

#### POISSON BASE ---- 
summary(fit_p <- glm(Severe_Crashes ~ Speed_Limit + Area_Type + ADT_vpd + Radius_feet + Lane_Width +
                         Shoulder_Type + Outside_Edge_Risk + Total_Outside_Shoulder_Width + Total_Xsect_width +
                         Adjacent_Intersection + Visual_Trap + Curve_Lighting,
                     data = curv, family = poisson(link = "log")))

sum(resid(fit_p, type = "pearson")^2) / (nrow(curv) - length(coef(fit_p))) #1.13
pchisq(fit_p$deviance, df=fit_p$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(fit_p) #-779
AIC(fit_p) #1598
sum(dpois(0,fitted(fit_p))) #7284
curv[,.N, by = Severe_Crashes] #7292

#### NEGATIVE BIOMIAL BASE ----
summary(fit_nb <- glm.nb(Severe_Crashes ~ Speed_Limit + Area_Type + ADT_vpd + Radius_feet + Lane_Width +
                          Shoulder_Type + Outside_Edge_Risk + Total_Outside_Shoulder_Width + Total_Xsect_width +
                          Adjacent_Intersection + Visual_Trap + Curve_Lighting, data = curv))

sum(resid(fit_nb, type = "pearson")^2) / (nrow(curv) - length(coef(fit_nb))) #1.13
pchisq(fit_nb$deviance, df=fit_nb$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(fit_nb) #-766
AIC(fit_nb) #1573
sum(dnbinom(0, mu = fitted(fit_nb), size = fit_nb$theta)) #7294

#### ZERO INFLATED POISSON BASE ----
summary(fit_zip <- zeroinfl(Severe_Crashes ~ Speed_Limit + Area_Type + Lane_Width +
                              Shoulder_Type + Outside_Edge_Risk + Total_Outside_Shoulder_Width +
                              Adjacent_Intersection + Visual_Trap + Curve_Lighting | ADT_vpd, data = curv),
                              zeroinfl.control(maxit = 10000))

sum(resid(fit_zip, type = "pearson")^2) / (nrow(curv) - length(coef(fit_zip))) #1.13
pchisq(fit_zip$deviance, df=fit_zip$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(fit_zip) #-784
AIC(fit_zip) #1573
sum(predict(fit_zip, type = "prob")[,1])

vuong(fit_zip, fit_nb) #vuong test indicates that the zero inflated model is better than the NB model
vuong(fit_zip, fit_p)


#### ZERO INFLATED NEGATIVE BINOMIAL BASE ----
summary(fit_zinb <- zeroinfl(Severe_Crashes ~ Speed_Limit + Area_Type + Lane_Width +
                               Shoulder_Type + Outside_Edge_Risk + Total_Outside_Shoulder_Width +
                               Adjacent_Intersection + Visual_Trap + Curve_Lighting | ADT_vpd, data = curv, dist = "negbin"))

sum(resid(fit_zinb, type = "pearson")^2) / (nrow(curv) - length(coef(fit_zinb))) #1.04
pchisq(fit_zinb$deviance, df=fit_zinb$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(fit_zinb) #-773
AIC(fit_zinb) #1587
sum(predict(fit_zinb, type = "prob")[,1]) #7292

vuong(fit_zinb, fit_nb) #vuong test indicates that the zero inflated model is better than the NB model
vuong(fit_zinb, fit_p)
vuong(fit_zinb, fit_zip)

vuong(fit_zinb, fit_nb) #raw = 1.51 #sig evidence to use zinb > nb
1/fit_nb$theta #4.36
#theta value would indivate an NB model, but with a Vuong statistic of 1.51 the test is inconclusive.

#rootograms
root_p <- rootogram(fit_p, style = "hanging", plot = F)
root_nb <- rootogram(fit_nb, style = "hanging", plot = F)
root_zip <- rootogram(fit_zip, style = "hanging", plot = F)
root_zinb <- rootogram(fit_zinb, style = "hanging", plot = F)

autoplot(root_p, title = "Base Rural Segment - Poisson")
autoplot(root_nb)
autoplot(root_zip)
autoplot(root_zinb)
xlims <- scale_x_continuous(breaks = 0:4, limits = c(-1,4)) #breaks = 0:6, limits = c(-1,6)
plot_grid(autoplot(root_p) + ggtitle("Poisson") + xlims,autoplot(root_nb) + ggtitle("NB") + xlims,
          autoplot(root_zip) + ggtitle("ZIP") + xlims,autoplot(root_zinb) + ggtitle("ZINB") + xlims)

curve_base <- as.data.table(cbind(as.character(curv_id), curv$Severe_Crashes, round(fit_nb$fitted.values,3)))
fwrite(curve_base, "clean_data/curv_base_fitted.csv")

#### FINAL ----
summary(full_nb <- glm.nb(Severe_Crashes ~ ., data = curv))

step(full_nb, direction = "both")

summary(step_nb <- glm.nb(formula = Severe_Crashes ~ Length_feet + Delineation + 
                            ADT_vpd + Adjacent_Intersection + Visual_Trap + Outside_Edge_Risk + 
                            Urban_Rural, data = curv, init.theta = 0.2208415343, link = log))


sum(resid(step_nb, type = "pearson")^2) / (nrow(curv) - length(coef(step_nb))) #1.13
pchisq(step_nb$deviance, df=step_nb$df.residual, lower.tail=F) #pval of 1, do not reject null hypothesis. Model correctly specified.
logLik(step_nb) #-766
AIC(step_nb) #1573
sum(dnbinom(0, mu = fitted(step_nb), size = step_nb$theta)) #7294

vuong(step_nb, fit_nb)
root_step <- rootogram(step_nb, style = "hanging", plot = F)
autoplot(root_step)

curve_full <- as.data.table(cbind(as.character(curv_id), round(step_nb$fitted.values,3)))
fwrite(curve_full, "clean_data/curv_full_fitted.csv")

fit_compare <- fit_nb$fitted.values - step_nb$fitted.values
fit_compare <- sort(fit_compare, decreasing = T)
summary(fit_compare[1:7448])
