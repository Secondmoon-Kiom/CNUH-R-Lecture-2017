###################################################
# Title: Chapter 6: Analysis of Longitudinal CT
# Author: Dr.DG. Chen 
###################################################


###################################################
### chunk number 2: Long.dat1
###################################################
#line 169 "R4Long.rnw"
require(RODBC)
datfile    = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
getxlsbook = odbcConnectExcel2007(datfile) 
dat        = sqlFetch(getxlsbook,"DBP")
odbcCloseAll()
# show first a few observations
head(dat)


###################################################
### chunk number 3: 
###################################################
#line 180 "R4Long.rnw"
summary(dat)


###################################################
### chunk number 4: 
###################################################
#line 188 "R4Long.rnw"
Dat = reshape(dat, direction="long", 
varying=c("DBP1","DBP2","DBP3","DBP4","DBP5"),
 idvar = c("Subject","TRT","Age","Sex"),sep="")
colnames(Dat) = c("Subject","TRT","Age","Sex","Time","DBP")
head(Dat)


###################################################
### chunk number 5: Long.figdat1.1
###################################################
#line 198 "R4Long.rnw"
library(lattice)
print(xyplot(DBP~Time|as.factor(Subject),type="l",groups=TRT,
strip=strip.custom(bg="white"), 
lty=c(1,8),lwd=2,layout=c(10,4), Dat))


###################################################
### chunk number 6: 
###################################################
#line 206 "R4Long.rnw"
#line 198 "R4Long.rnw#from line#206#"
library(lattice)
print(xyplot(DBP~Time|as.factor(Subject),type="l",groups=TRT,
strip=strip.custom(bg="white"), 
lty=c(1,8),lwd=2,layout=c(10,4), Dat))
#line 207 "R4Long.rnw"


###################################################
### chunk number 7: Long.figdat1.2
###################################################
#line 213 "R4Long.rnw"
print(xyplot(DBP~Time|TRT,type="l",Dat, groups=as.factor(Subject),
strip=strip.custom(bg="white")))


###################################################
### chunk number 8: 
###################################################
#line 219 "R4Long.rnw"
#line 213 "R4Long.rnw#from line#219#"
print(xyplot(DBP~Time|TRT,type="l",Dat, groups=as.factor(Subject),
strip=strip.custom(bg="white")))
#line 220 "R4Long.rnw"


###################################################
### chunk number 9: Long.figdat1.3
###################################################
#line 230 "R4Long.rnw"
print(bwplot(DBP~as.factor(Time)|TRT,Dat, xlab="Time",
strip=strip.custom(bg="white")))


###################################################
### chunk number 10: 
###################################################
#line 236 "R4Long.rnw"
#line 230 "R4Long.rnw#from line#236#"
print(bwplot(DBP~as.factor(Time)|TRT,Dat, xlab="Time",
strip=strip.custom(bg="white")))
#line 237 "R4Long.rnw"


###################################################
### chunk number 11: Long.intslope
###################################################
#line 244 "R4Long.rnw"
num.Subj = 40
# initiate the intercept and slope
intercept = slope = numeric(num.Subj)
# loop-over
for(i in 1:num.Subj){
# fit regression model
mod          = lm(DBP~Time, Dat[Dat$Subject==i,])
# extract the intercept and slope
intercept[i] = coef(mod)[1]
slope[i]     = coef(mod)[2]
}
# make a dataframe "dat.coef" 
dat.coef = data.frame(Subject=dat$Subject,TRT=dat$TRT,
Intercept = intercept, Slope=slope)
# print it out
dat.coef


###################################################
### chunk number 12: Long.figdat1.interceptslope
###################################################
#line 266 "R4Long.rnw"
# Make histogram for both intercept and slope
int.hist   = hist(intercept,plot=F)
slope.hist = hist(slope,plot=F)
# make layout for plotting       
top        = max(c(int.hist$counts, slope.hist$counts))
nf         = layout(matrix(c(2,0,1,3),2,2,byrow=T),
		c(3,1), c(1,3),T)
par(mar=c(5,4,1,1))
# plot the intercept and slope
plot(Slope~Intercept,las=1,dat.coef,xlab="Intercept",
  ylab="Slope",pch=as.character(TRT))
par(mar=c(0,4,1,1))
# add the intercept histogram
barplot(int.hist$counts, axes=FALSE, 
	ylim=c(0, top), space=0)
par(mar=c(5,0,1,1))
# add the slope histogram
barplot(slope.hist$counts, axes=FALSE, 
	xlim=c(0, top), space=0, horiz=TRUE)


###################################################
### chunk number 13: 
###################################################
#line 289 "R4Long.rnw"
#line 266 "R4Long.rnw#from line#289#"
# Make histogram for both intercept and slope
int.hist   = hist(intercept,plot=F)
slope.hist = hist(slope,plot=F)
# make layout for plotting       
top        = max(c(int.hist$counts, slope.hist$counts))
nf         = layout(matrix(c(2,0,1,3),2,2,byrow=T),
		c(3,1), c(1,3),T)
par(mar=c(5,4,1,1))
# plot the intercept and slope
plot(Slope~Intercept,las=1,dat.coef,xlab="Intercept",
  ylab="Slope",pch=as.character(TRT))
par(mar=c(0,4,1,1))
# add the intercept histogram
barplot(int.hist$counts, axes=FALSE, 
	ylim=c(0, top), space=0)
par(mar=c(5,0,1,1))
# add the slope histogram
barplot(slope.hist$counts, axes=FALSE, 
	xlim=c(0, top), space=0, horiz=TRUE)
#line 290 "R4Long.rnw"


###################################################
### chunk number 14: 
###################################################
#line 296 "R4Long.rnw"
# fit model 1 with interation
mod1.coef = lm(Slope~Intercept*TRT, dat.coef)
summary(mod1.coef)
# fit model 2 without interaction
mod2.coef = lm(Slope~Intercept+TRT, dat.coef)
summary(mod2.coef)


###################################################
### chunk number 15: 
###################################################
#line 307 "R4Long.rnw"
t.test(Slope~TRT, dat.coef)
t.test(Intercept~TRT, dat.coef)


###################################################
### chunk number 16: 
###################################################
#line 318 "R4Long.rnw"
# load the library lme4
library(lme4)
# Fit Model 1
mod1DBP =  lmer(DBP~TRT*Time+(Time|Subject), Dat,
 method="ML")
# Fit Model 2
mod2DBP =  lmer(DBP~TRT*Time+(1|Subject), Dat,
 method="ML")
# model comparison   
anova(mod1DBP, mod2DBP)


###################################################
### chunk number 17: 
###################################################
#line 332 "R4Long.rnw"
summary(mod2DBP)


###################################################
### chunk number 18: 
###################################################
#line 338 "R4Long.rnw"
# fit Model 3 
mod3DBP =  lmer(DBP~TRT+Time+(Time|Subject), Dat)
# fit Model 4
mod4DBP =  lmer(DBP~TRT+Time+(1|Subject), Dat)
# model comparison
anova(mod3DBP, mod4DBP)


###################################################
### chunk number 19: 
###################################################
#line 349 "R4Long.rnw"
summary(mod3DBP)


###################################################
### chunk number 20: Long.figdat1.qqmath
###################################################
#line 363 "R4Long.rnw"
print(qqmath(~resid(mod3DBP)|TRT,Dat, strip=strip.custom(bg="white"), 
	xlab="Theoretical Normal", ylab="Residuals"))


###################################################
### chunk number 21: 
###################################################
#line 369 "R4Long.rnw"
#line 363 "R4Long.rnw#from line#369#"
print(qqmath(~resid(mod3DBP)|TRT,Dat, strip=strip.custom(bg="white"), 
	xlab="Theoretical Normal", ylab="Residuals"))
#line 370 "R4Long.rnw"


###################################################
### chunk number 22: Long.figdat1.resid2
###################################################
#line 378 "R4Long.rnw"
print(bwplot(resid(mod3DBP)~as.factor(Time)|TRT,Dat, 
strip=strip.custom(bg="white"), 
xlab="Time",ylim=c(-5,5), ylab="Residuals"))


###################################################
### chunk number 23: 
###################################################
#line 385 "R4Long.rnw"
#line 378 "R4Long.rnw#from line#385#"
print(bwplot(resid(mod3DBP)~as.factor(Time)|TRT,Dat, 
strip=strip.custom(bg="white"), 
xlab="Time",ylim=c(-5,5), ylab="Residuals"))
#line 386 "R4Long.rnw"


###################################################
### chunk number 24: 
###################################################
#line 393 "R4Long.rnw"
# fit Model 3 include ``Age" effect
mod5DBP =  lmer(DBP~TRT+Time+Age+(Time|Subject), Dat,  method="ML")
# call anova to test ``Age" effect
anova(mod3DBP, mod5DBP)


###################################################
### chunk number 25: 
###################################################
#line 401 "R4Long.rnw"
# fit Model 6 including ``Age" and ``Sex"
mod6DBP =  lmer(DBP~TRT+Time+Age+Sex+(Time|Subject), Dat, method="ML")
# test the ``Sex" effect
anova(mod5DBP, mod6DBP)


###################################################
### chunk number 26: Long.dat2
###################################################
#line 417 "R4Long.rnw"
require(RODBC)
datfile    = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
getxlsbook = odbcConnectExcel2007(datfile) 
dat        = sqlFetch(getxlsbook,"Ulcer")
odbcCloseAll()
head(dat)


###################################################
### chunk number 27: 
###################################################
#line 427 "R4Long.rnw"
# total n for each TRT
n  = tapply(rep(1, dim(dat)[1]),dat$TRT,sum)
# number for time 1
n1 = tapply(dat$Time1,dat$TRT,sum)
# number for time 2
n2 = tapply(dat$Time2,dat$TRT,sum)
# number for time 4
n4 = tapply(dat$Time4,dat$TRT,sum)
print(rbind(n,n1,n2,n4))
# proportions
print( round(rbind(n1/n,n2/n,n4/n),2))


###################################################
### chunk number 28: 
###################################################
#line 443 "R4Long.rnw"
Dat     = reshape(dat, direction="long", 
varying = c("Time0","Time1","Time2","Time4"),
 idvar  = c("Subject","TRT","WeekH"),sep="")
colnames(Dat) = c("Subject","TRT","WeekH","Time","Heal")
# sort the data by Subject: very important for gee
Dat      = Dat[order(Dat$Subject),] 
# Remove the baseline for model fitting
Dat      = Dat[Dat$Time > 0,]
# make the TRT and Time as factors
Dat$TRT  = as.factor(Dat$TRT)
Dat$Time = as.factor(Dat$Time) 
# show the first 6 observations
head(Dat)


###################################################
### chunk number 29: 
###################################################
#line 460 "R4Long.rnw"
# fit Model 1: with interaction
mod1glm = glm(Heal~TRT*Time, family=binomial, Dat)
# fit Model 2: without interaction
mod2glm = glm(Heal~TRT+Time, family=binomial, data=Dat)
# test these two model using Chi-Square test
anova(mod1glm,mod2glm, test="Chi")


###################################################
### chunk number 30: 
###################################################
#line 470 "R4Long.rnw"
summary(mod2glm)


###################################################
### chunk number 31: 
###################################################
#line 477 "R4Long.rnw"
# load the ``multcomp" library
library(multcomp)
# multiple comparisons
glht.mod2glm = glht(mod2glm, mcp(TRT="Tukey", Time="Tukey"))
summary(glht.mod2glm)


###################################################
### chunk number 32: 
###################################################
#line 490 "R4Long.rnw"
# load MASS library
library(MASS)
# fit the Model 3
mod3glm = glmmPQL(Heal~TRT, random=~1|Subject, 
	family=binomial, Dat)
# print the summary
summary(mod3glm)


###################################################
### chunk number 33: 
###################################################
#line 504 "R4Long.rnw"
# fit Model 4
mod4glm = glm(Heal~TRT, family=binomial, Dat)
summary(mod4glm)


###################################################
### chunk number 34: 
###################################################
#line 515 "R4Long.rnw"
# load the ``gee" library
library(gee)
# fit the gee model with independent patient effect
fit.gee1 = gee(Heal~TRT,id=Subject,family=binomial, 
data=Dat,corstr="independence", scale.fix=T)
# print the summary
summary(fit.gee1)


###################################################
### chunk number 35: 
###################################################
#line 528 "R4Long.rnw"
fit.gee2 = gee(Heal~TRT,id=Subject,family=binomial, 
data=Dat,corstr="exchangeable", scale.fix=T)
summary(fit.gee2)


