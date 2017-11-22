###################################################
# Title: R code for Chapter 5: CT with Time-to-Event data
# Author: DR. DG. Chen 
###################################################


###################################################
### chunk number 2: Surv.dat1
###################################################
#line 499 "R4Surv.rnw"
# load the RODBC package
require(RODBC)
# the data path
datfile    = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
# connect to the excel book
getxlsbook = odbcConnectExcel2007(datfile) 
# get the data
dat        = sqlFetch(getxlsbook,"CTCarcinoma")
odbcCloseAll()
# show first a few observations
head(dat)


###################################################
### chunk number 3: Surv.km
###################################################
#line 518 "R4Surv.rnw"
# load the R library
library(survival)
# fit Kaplan-Meier 
fit.km = survfit(Surv(Time,Status==0)~TRT,
	type=c("kaplan-meier"),dat)
# print the model fitting
fit.km


###################################################
### chunk number 4: Surv.fig4dat1
###################################################
#line 532 "R4Surv.rnw"
plot(fit.km, lty=c(1,4,8),lwd=2,xlab="Time in Weeks",ylab="S(t)")
legend("bottomleft",title="Line Types",
c("S+CT","S+CT+IT","S+IT"),lty=c(1,4,8), lwd=2) 


###################################################
### chunk number 5: 
###################################################
#line 539 "R4Surv.rnw"
#line 532 "R4Surv.rnw#from line#539#"
plot(fit.km, lty=c(1,4,8),lwd=2,xlab="Time in Weeks",ylab="S(t)")
legend("bottomleft",title="Line Types",
c("S+CT","S+CT+IT","S+IT"),lty=c(1,4,8), lwd=2) 
#line 540 "R4Surv.rnw"


###################################################
### chunk number 6: Surv.fig4hazard1
###################################################
#line 591 "R4Surv.rnw"
fit.fleming =survfit(Surv(Time,Status==0)~TRT,
		dat,type='fleming') 
plot(fit.fleming,lty=c(1,4,8),lwd=2,fun="cumhaz", 
xlab="Time in Weeks", ylab="Cumulative Hazard") 
legend("topleft",title="Line Types",
c("S+CT","S+CT+IT","S+IT"),lty=c(1,4,8),lwd=2) 


###################################################
### chunk number 7: 
###################################################
#line 601 "R4Surv.rnw"
#line 591 "R4Surv.rnw#from line#601#"
fit.fleming =survfit(Surv(Time,Status==0)~TRT,
		dat,type='fleming') 
plot(fit.fleming,lty=c(1,4,8),lwd=2,fun="cumhaz", 
xlab="Time in Weeks", ylab="Cumulative Hazard") 
legend("topleft",title="Line Types",
c("S+CT","S+CT+IT","S+IT"),lty=c(1,4,8),lwd=2) 
#line 602 "R4Surv.rnw"


###################################################
### chunk number 8: 
###################################################
#line 608 "R4Surv.rnw"
# use "survdiff" to test difference
fit.diff = survdiff(Surv(Time, Status==0)~TRT,data=dat)
fit.diff


###################################################
### chunk number 9: 
###################################################
#line 622 "R4Surv.rnw"
# fit exponential model
fit.exp =survreg(Surv(Time, Status==0)~TRT,dat,
		dist="exponential")
summary(fit.exp)
# fit Weibull model
fit.Weibull =survreg(Surv(Time, Status==0)~TRT,dat)
summary(fit.Weibull)


###################################################
### chunk number 10: 
###################################################
#line 657 "R4Surv.rnw"
# fit exponential model +Age
fit.exp.age = survreg(Surv(Time, Status==0)~TRT+Age,
		dat,dist="exponential")
summary(fit.exp.age)
# fit Weibull model+Age
fit.Weibull.age = survreg(Surv(Time,Status==0)~TRT+Age,dat)
summary(fit.Weibull.age)


###################################################
### chunk number 11: 
###################################################
#line 672 "R4Surv.rnw"
# fit Cox
fit.Cox = coxph(Surv(Time, Status==0)~TRT,dat)
summary(fit.Cox)
# fit Cox +Age
fit.Cox.age = coxph(Surv(Time, Status==0)~TRT+Age,dat)
summary(fit.Cox.age)


###################################################
### chunk number 12: 
###################################################
#line 689 "R4Surv.rnw"
require(RODBC)
datfile    = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
getxlsbook = odbcConnectExcel2007(datfile) 
dat        = sqlFetch(getxlsbook,"BreastCancer")
odbcCloseAll()
head(dat)


###################################################
### chunk number 13: 
###################################################
#line 707 "R4Surv.rnw"
cria.tau = function(data){
l   = data$tL;r = data$tU
# sort all the time points
tau = sort(unique(c(l,r[is.finite(r)])))
return(tau)
}


###################################################
### chunk number 14: 
###################################################
#line 717 "R4Surv.rnw"
S.ini = function(tau){
# take the ordered time
m    = length(tau)
# fit the Kaplan-Meier
ekm  = survfit(Surv(tau[1:m-1],rep(1,m-1))~1)
# Output the estimated Survival
So   = c(1,ekm$surv)
# estimate the step
p    = -diff(So)
return(p)
}


###################################################
### chunk number 15: 
###################################################
#line 732 "R4Surv.rnw"
cria.A = function(data,tau){
tau12  = cbind(tau[-length(tau)],tau[-1])
interv = function(x,inf,sup) 
		ifelse(x[1]>=inf & x[2]<=sup,1,0)
A      = apply(tau12,1,interv,inf=data$tL,sup=data$tU)
id.lin.zero = which(apply(A==0, 1, all))
if(length(id.lin.zero)>0) A = A[-id.lin.zero, ]
return(A)
}
# Turnbull function
Turnbull = function(p, A, data, eps=1e-3, 
	iter.max=200, verbose=FALSE){
n =nrow(A);m=ncol(A);Q=matrix(1,m)
iter    = 0
repeat {
iter = iter + 1; diff = (Q-p)
maxdiff = max(abs(as.vector(diff)))
if (verbose) print(maxdiff)
if (maxdiff<eps | iter>=iter.max) break
Q  = p; C  = A%*%p; p=p*((t(A)%*%(1/C))/n)
}
cat("Iterations = ", iter,"\n")
cat("Max difference = ", maxdiff,"\n")
cat("Convergence criteria: Max difference < 1e-3","\n")
dimnames(p) = list(NULL,c("P Estimate"))
surv        = round(c(1,1-cumsum(p)),digits=5)
right       = data$tU
if(any(!(is.finite(right)))){
t = max(right[is.finite(right)])
return(list(time=tau[tau<t],surv=surv[tau<t]))
}
else return(list(time=tau,surv=surv))
}


###################################################
### chunk number 16: 
###################################################
#line 769 "R4Surv.rnw"
# get the data for TRT=1
dat1 = dat[dat$TRT==1,]
dat1$tU[is.na(dat1$tU)] = Inf
# sort the time points
tau  = cria.tau(dat1)
# Estimate the initial Survival
p    = S.ini(tau=tau)
# run Turnbull and name it as "mb1"
A    = cria.A(data=dat1,tau=tau)
mb1  = Turnbull(p,A,dat1)
# print the estimates
mb1


###################################################
### chunk number 17: 
###################################################
#line 786 "R4Surv.rnw"
dat0  = dat[dat$TRT==0,]
dat0$tU[is.na(dat0$tU)] = Inf
tau   = cria.tau(dat0)
p     = S.ini(tau=tau)
A     = cria.A(data=dat0,tau=tau)
mb0   = Turnbull(p,A,dat0)
mb0


###################################################
### chunk number 18: figSurv.Turnbull1
###################################################
#line 800 "R4Surv.rnw"
# plot the TRT=1
plot(mb1$time,mb1$surv,lty=1,lwd=2,type="s", ylim=c(0,1),
xlim=range(c(0,60)),xlab="Time in Months",ylab="S(t)")
# add a line for TRT=0
lines(mb0$time,mb0$surv,lty=4,lwd=2,type="s")
# put a legend
legend("topright",title="Line Types",lty=c(1,4),lwd=2,
c("Radiation Only","Radiation+Chemotherapy"))


###################################################
### chunk number 19: 
###################################################
#line 813 "R4Surv.rnw"
#line 800 "R4Surv.rnw#from line#813#"
# plot the TRT=1
plot(mb1$time,mb1$surv,lty=1,lwd=2,type="s", ylim=c(0,1),
xlim=range(c(0,60)),xlab="Time in Months",ylab="S(t)")
# add a line for TRT=0
lines(mb0$time,mb0$surv,lty=4,lwd=2,type="s")
# put a legend
legend("topright",title="Line Types",lty=c(1,4),lwd=2,
c("Radiation Only","Radiation+Chemotherapy"))
#line 814 "R4Surv.rnw"


###################################################
### chunk number 20: Surv.midpoint
###################################################
#line 825 "R4Surv.rnw"
# get the midpoint
time   = dat$tL+((dat$tU-dat$tL)/2)
# get the censorship
Status = ifelse(is.finite(time),1,0)
# replace the NA with left-time
time   = ifelse(is.finite(time),time,dat$tL)
# fit Kaplan-Meier model
ekm    = survfit(Surv(time, Status)~TRT,
		type=c("kaplan-meier"),dat)
# print the output
ekm


###################################################
### chunk number 21: figSurv.midpoint
###################################################
#line 841 "R4Surv.rnw"
plot(mb1$time,mb1$surv,lty=1,lwd=3,type="s",ylim=c(0,1),
xlim=range(c(0,50)), xlab="Time in Months",ylab="S(t)")
legend("bottomleft",title="Line Types",lty=c(1,4),lwd=3,
c("Radiotherapy Only","Radiotherapy+Chemotherapy"))
lines(mb0$time,mb0$surv,lty=4,lwd=3,type="s")
lines(ekm[1]$time,ekm[1]$surv,type="s",lty=4,lwd=1)
lines(ekm[2]$time,ekm[2]$surv,type="s",lty=1,lwd=1)


###################################################
### chunk number 22: 
###################################################
#line 853 "R4Surv.rnw"
#line 841 "R4Surv.rnw#from line#853#"
plot(mb1$time,mb1$surv,lty=1,lwd=3,type="s",ylim=c(0,1),
xlim=range(c(0,50)), xlab="Time in Months",ylab="S(t)")
legend("bottomleft",title="Line Types",lty=c(1,4),lwd=3,
c("Radiotherapy Only","Radiotherapy+Chemotherapy"))
lines(mb0$time,mb0$surv,lty=4,lwd=3,type="s")
lines(ekm[1]$time,ekm[1]$surv,type="s",lty=4,lwd=1)
lines(ekm[2]$time,ekm[2]$surv,type="s",lty=1,lwd=1)
#line 854 "R4Surv.rnw"


###################################################
### chunk number 23: 
###################################################
#line 868 "R4Surv.rnw"
# create a new dataset "breast" and keep "dat" for future use
breast = dat
# replace 0's with NA as left-censored
breast$tL[breast$tL==0]= NA
# print the first a few observations
head(breast)


###################################################
### chunk number 24: 
###################################################
#line 879 "R4Surv.rnw"
# fit exponential
fit.exp=survreg(Surv(tL,tU,type="interval2")~TRT,
		breast,dist="exponential")
summary(fit.exp)
# fit Weibull
fit.Weibull=survreg(Surv(tL,tU,type="interval2")~TRT,data=breast)
summary(fit.Weibull)


###################################################
### chunk number 25: 
###################################################
#line 898 "R4Surv.rnw"
library(intcox)


###################################################
### chunk number 26: 
###################################################
#line 903 "R4Surv.rnw"
fit.IntCox = intcox(Surv(tL,tU,type="interval2")~TRT,data=dat)
# print the model fit
fit.IntCox


###################################################
### chunk number 27: fig4Surv.intcox.surv
###################################################
#line 911 "R4Surv.rnw"
# the baseline survival is exp(-baseline cumulative hazard)
surv.base = exp(-fit.IntCox$lambda0)
# plot the survival function for TRT=0 
plot(fit.IntCox$time.point,surv.base,type="s",
	xlab="Time in Months",ylab="S(t)",lty=4, lwd=3)
# add the survival function for TRT=1
lines(fit.IntCox$time.point,surv.base^exp(fit.IntCox$coef),
	type="s",lty=1, lwd=3)
# add the legend
legend("bottomleft",title="Line Types",lty=c(1,4),lwd=3,
c("Radiation Only","Radiation+Chemotherapy"))


###################################################
### chunk number 28: 
###################################################
#line 926 "R4Surv.rnw"
#line 911 "R4Surv.rnw#from line#926#"
# the baseline survival is exp(-baseline cumulative hazard)
surv.base = exp(-fit.IntCox$lambda0)
# plot the survival function for TRT=0 
plot(fit.IntCox$time.point,surv.base,type="s",
	xlab="Time in Months",ylab="S(t)",lty=4, lwd=3)
# add the survival function for TRT=1
lines(fit.IntCox$time.point,surv.base^exp(fit.IntCox$coef),
	type="s",lty=1, lwd=3)
# add the legend
legend("bottomleft",title="Line Types",lty=c(1,4),lwd=3,
c("Radiation Only","Radiation+Chemotherapy"))
#line 927 "R4Surv.rnw"


###################################################
### chunk number 29: Surv.boot.IntCox
###################################################
#line 934 "R4Surv.rnw"
set.seed(12345678)
# number of bootstrapping=1000
num.boot    = 1000
boot.intcox = numeric(num.boot)
# the for-loop
for(b in 1:num.boot){
     #sample with replacement
     boot.ID=sample(1:dim(dat)[1],replace=T)
     # fit intcox for the bootstrap sample
     boot.fit = intcox(Surv(tL,tU,type="interval2")~TRT,
                  dat[boot.ID,],no.warnings = TRUE)
     # keep track the coefficient
     boot.intcox[b] = coef(boot.fit)
} # end of b-loop


###################################################
### chunk number 30: 
###################################################
#line 953 "R4Surv.rnw"
Boot.CI = quantile(boot.intcox, c(0.025,0.975))
Boot.CI 


###################################################
### chunk number 31: 
###################################################
#line 962 "R4Surv.rnw"
bias.IntCox =c(mean.bias=coef(fit.IntCox)-mean(boot.intcox),
         median.bias=coef(fit.IntCox)-median(boot.intcox))
bias.IntCox


###################################################
### chunk number 32: fig4Surv.boot.intcox
###################################################
#line 969 "R4Surv.rnw"
# Histogram from bootstrap sample
hist(boot.intcox,prob=T,las=1, 
xlab="Treatment Difference",ylab="Prob", main="")
# put vertical lines for 
abline(v=c(Boot.CI[1],fit.IntCox$coef,mean(boot.intcox),
median(boot.intcox),Boot.CI[2]), lwd=c(2,3,3,3,2), 
lty=c(4,1,2,3,4))


###################################################
### chunk number 33: 
###################################################
#line 982 "R4Surv.rnw"
#line 969 "R4Surv.rnw#from line#982#"
# Histogram from bootstrap sample
hist(boot.intcox,prob=T,las=1, 
xlab="Treatment Difference",ylab="Prob", main="")
# put vertical lines for 
abline(v=c(Boot.CI[1],fit.IntCox$coef,mean(boot.intcox),
median(boot.intcox),Boot.CI[2]), lwd=c(2,3,3,3,2), 
lty=c(4,1,2,3,4))
#line 983 "R4Surv.rnw"


