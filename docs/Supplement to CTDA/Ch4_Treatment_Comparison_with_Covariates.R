###################################################
# Title: R code for Chapter 4: Treatment Comparison with Covariates
# Author: DR. CG Chen
###################################################

###################################################
### chunk number 2: 
###################################################
#line 327 "R4ANCOVA.rnw"
# load the library
require(RODBC)
# get the data path
datfile    = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
# link the excel data book
getxlsbook = odbcConnectExcel2007(datfile) 
# get the data from the data sheet
dat        = sqlFetch(getxlsbook,"DBP")
odbcCloseAll()
# creat the difference
dat$diff = dat$DBP5-dat$DBP1


###################################################
### chunk number 3: ANCOVA.BaseDBP
###################################################
#line 344 "R4ANCOVA.rnw"
boxplot(DBP1~TRT, dat, las=1, 
	xlab="Treatment", ylab="DBP at Baseline")


###################################################
### chunk number 4: 
###################################################
#line 351 "R4ANCOVA.rnw"
#line 344 "R4ANCOVA.rnw#from line#351#"
boxplot(DBP1~TRT, dat, las=1, 
	xlab="Treatment", ylab="DBP at Baseline")
#line 352 "R4ANCOVA.rnw"


###################################################
### chunk number 5: 
###################################################
#line 361 "R4ANCOVA.rnw"
t.test(DBP1~TRT, dat)


###################################################
### chunk number 6: 
###################################################
#line 369 "R4ANCOVA.rnw"
# call function table to make the 2 by 2 table 
SexbyTRT = table(dat$TRT,dat$Sex)
# print it
SexbyTRT
# call prop.test to test the difference
prop.test(SexbyTRT)


###################################################
### chunk number 7: 
###################################################
#line 382 "R4ANCOVA.rnw"
# Fit the main effect model on "Sex" and "Age"
bm1=lm(DBP1~Sex+Age, dat)
# Show the result
summary(bm1)


###################################################
### chunk number 8: ANCOVA.fig4Baseline
###################################################
#line 391 "R4ANCOVA.rnw"
# plot the ``Age" to ``DBP1"
plot(DBP1~Age,las=1,pch=as.character(Sex), dat, 
	xlab="Age", ylab="Baseline DBP")
# add the regression lines using ``abline"
abline(bm1$coef[1], bm1$coef[3],lwd=2, lty=1)
abline(bm1$coef[1]+bm1$coef[2], bm1$coef[3],lwd=2, lty=4)


###################################################
### chunk number 9: 
###################################################
#line 401 "R4ANCOVA.rnw"
#line 391 "R4ANCOVA.rnw#from line#401#"
# plot the ``Age" to ``DBP1"
plot(DBP1~Age,las=1,pch=as.character(Sex), dat, 
	xlab="Age", ylab="Baseline DBP")
# add the regression lines using ``abline"
abline(bm1$coef[1], bm1$coef[3],lwd=2, lty=1)
abline(bm1$coef[1]+bm1$coef[2], bm1$coef[3],lwd=2, lty=4)
#line 402 "R4ANCOVA.rnw"


###################################################
### chunk number 10: 
###################################################
#line 414 "R4ANCOVA.rnw"
# start with full model
m0 = lm(diff~TRT*Age*Sex, dat)
# stepwise model selection
m1 = step(m0)
# output the ANOVA
anova(m1)


###################################################
### chunk number 11: 
###################################################
#line 424 "R4ANCOVA.rnw"
# fit the reduced model
m2 = lm(diff~TRT+Age, dat)
# output the anova
anova(m2)
# output the model fit
summary(m2)


###################################################
### chunk number 12: ANCOVA.fig4diff
###################################################
#line 436 "R4ANCOVA.rnw"
plot(diff~Age,las=1,pch=as.character(TRT), dat, 
xlab="Age", ylab="DBP Change")
abline(m2$coef[1], m2$coef[3],lwd=2, lty=1)
abline(m2$coef[1]+m2$coef[2], m2$coef[3],lwd=2, lty=4)


###################################################
### chunk number 13: 
###################################################
#line 444 "R4ANCOVA.rnw"
#line 436 "R4ANCOVA.rnw#from line#444#"
plot(diff~Age,las=1,pch=as.character(TRT), dat, 
xlab="Age", ylab="DBP Change")
abline(m2$coef[1], m2$coef[3],lwd=2, lty=1)
abline(m2$coef[1]+m2$coef[2], m2$coef[3],lwd=2, lty=4)
#line 445 "R4ANCOVA.rnw"


###################################################
### chunk number 14: 
###################################################
#line 461 "R4ANCOVA.rnw"
betablocker$Center = as.factor(betablocker$Center)
colnames(betablocker) = c("Deaths","Total","Center","TRT")


###################################################
### chunk number 15: 
###################################################
#line 467 "R4ANCOVA.rnw"
betablocker


###################################################
### chunk number 16: beta.glm
###################################################
#line 472 "R4ANCOVA.rnw"
# fit a logistic regression using glm
beta.glm = glm(cbind(Deaths,Total-Deaths)~TRT+Center,
		family=binomial,data=betablocker)
# print the model fitting
anova(beta.glm)


###################################################
### chunk number 17: 
###################################################
#line 481 "R4ANCOVA.rnw"
summary(beta.glm)


###################################################
### chunk number 18: 
###################################################
#line 488 "R4ANCOVA.rnw"
est.dp = sum(resid(beta.glm, type="pearson")^2)/beta.glm$df.res
est.dp


###################################################
### chunk number 19: 
###################################################
#line 494 "R4ANCOVA.rnw"
summary(beta.glm, dispersion=est.dp)


###################################################
### chunk number 20: 
###################################################
#line 499 "R4ANCOVA.rnw"
# fit quasi-likelihood for binomial data
beta.glm2 = glm(cbind(Deaths,Total- Deaths)~TRT+Center,
	family=quasibinomial,data=betablocker)
# print the model fit
summary(beta.glm2)


###################################################
### chunk number 21: 
###################################################
#line 513 "R4ANCOVA.rnw"
data(polyps)


###################################################
### chunk number 22: 
###################################################
#line 518 "R4ANCOVA.rnw"
polyps


###################################################
### chunk number 23: 
###################################################
#line 523 "R4ANCOVA.rnw"
# Poisson Regression
m0.polyps = glm(number~treat*age, polyps, family=poisson())
# print the model fit
summary(m0.polyps)


###################################################
### chunk number 24: 
###################################################
#line 531 "R4ANCOVA.rnw"
est.dp = sum(resid(m0.polyps, type="pearson")^2)/m0.polyps$df.res
est.dp


###################################################
### chunk number 25: 
###################################################
#line 536 "R4ANCOVA.rnw"
summary(m0.polyps, dispersion=est.dp)


###################################################
### chunk number 26: 
###################################################
#line 541 "R4ANCOVA.rnw"
# refit the model without interaction
m1.polyps = glm(number~treat+age, polyps, family=poisson())
# estimate the dispersion parameter
est.dp = sum(resid(m1.polyps, type="pearson")^2)/m1.polyps$df.res
# print the estimated dispersion parameter
est.dp
# print the model fit adjusting the over dispersion
summary(m1.polyps, dispersion=est.dp)


###################################################
### chunk number 27: 
###################################################
#line 554 "R4ANCOVA.rnw"
# fit the quasi Poisson
m2.polyps = glm(number~treat+age, polyps, family=quasipoisson())
# print the model fit
summary(m2.polyps)


###################################################
### chunk number 28: 
###################################################
#line 564 "R4ANCOVA.rnw"
# load the MASS library
library(MASS)
# fit the negative binomial model
m3.polyps = glm.nb(number~treat+age, polyps)
# print the model fit
summary(m3.polyps)


