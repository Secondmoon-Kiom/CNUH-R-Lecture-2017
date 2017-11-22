###################################################
# Title: Chapter 9: Bayesian Method in CT
# Author: Dr. DG.Chen 
###################################################


###################################################
### chunk number 2: 
###################################################
#line 286 "R4Bayes.rnw"
library(R2WinBUGS)


###################################################
### chunk number 3: 
###################################################
#line 291 "R4Bayes.rnw"
library(help=R2WinBUGS)


###################################################
### chunk number 4: 
###################################################
#line 301 "R4Bayes.rnw"
library(BRugs)


###################################################
### chunk number 5: rbugs
###################################################
#line 314 "R4Bayes.rnw"
library(rbugs)


###################################################
### chunk number 6: pack
###################################################
#line 333 "R4Bayes.rnw"
library(MCMCpack)


###################################################
### chunk number 7: 
###################################################
#line 349 "R4Bayes.rnw"
# set seed to 123
set.seed(123)
# n=30 patients
n        = 30
# known sigma
sigma  = 2
# population mean
mu      = 3
# simulate data
y         =  rnorm(n, mu,sigma)
# print the data
# the mean and variance of the simulated data
mean(y)
var(y)


###################################################
### chunk number 8: 
###################################################
#line 367 "R4Bayes.rnw"
# the prior parameters
mu0 = 2; tau0 = .5 
# the weight
w           =  tau0^2/(tau0^2+sigma^2/n)
# the posterior mean
muP       =  w*mean(y) + (1-w)*mu0
# the posterior standard deviation
sigmaP = sqrt(1/(1/tau0^2+n/sigma^2))
# direct simulation of posterior normal
Bayes1.norm2norm = rnorm(10000, muP,sigmaP)


###################################################
### chunk number 9: 
###################################################
#line 381 "R4Bayes.rnw"
quantile(Bayes1.norm2norm, c(0.025,0.25,0.5,0.75,0.975))


###################################################
### chunk number 10: 
###################################################
#line 386 "R4Bayes.rnw"
# call the function
Bayes2.norm2norm = MCnormalnormal(y, sigma^2, mu0, tau0^2, 10000)
# print the summary
summary(Bayes2.norm2norm)


###################################################
### chunk number 11: Bayes.fig4norm2norm
###################################################
#line 394 "R4Bayes.rnw"
x     = seq(0,6,0.01)
plot(x, dnorm(x, mu0,tau0), type="l", lwd=1,las=1, ylim=c(0,1.4),
   xlab="mu", ylab="density")
lines(x, dnorm(x, mean(y), sigma/sqrt(n)), lty=8, lwd=1)
lines(density(Bayes1.norm2norm), lty=8, lwd=3)
legend("topright", c("Prior","Likelihood", "Posterior"), lwd=c(1,1,3), 
lty=c(1,8,4))


###################################################
### chunk number 12: 
###################################################
#line 405 "R4Bayes.rnw"
#line 394 "R4Bayes.rnw#from line#405#"
x     = seq(0,6,0.01)
plot(x, dnorm(x, mu0,tau0), type="l", lwd=1,las=1, ylim=c(0,1.4),
   xlab="mu", ylab="density")
lines(x, dnorm(x, mean(y), sigma/sqrt(n)), lty=8, lwd=1)
lines(density(Bayes1.norm2norm), lty=8, lwd=3)
legend("topright", c("Prior","Likelihood", "Posterior"), lwd=c(1,1,3), 
lty=c(1,8,4))
#line 406 "R4Bayes.rnw"


###################################################
### chunk number 13: 
###################################################
#line 418 "R4Bayes.rnw"
# total patients for each treatment
n  = c(168, 182, 165,188)
# number healed
x = c(69, 113, 120, 145)
# the observed proportion
p = x/n
p


###################################################
### chunk number 14: 
###################################################
#line 438 "R4Bayes.rnw"
# the objective function
obj = function(parm){
a = parm[1]; b = parm[2]
( pbeta(0.50,a,b) -0.75)^2 +( pbeta(0.95,a,b)- 0.85)^2
}


###################################################
### chunk number 15: 
###################################################
#line 447 "R4Bayes.rnw"
# call optim to search the root with initial values at (3,3)
out = optim(c(3,3), obj)
print(out)


###################################################
### chunk number 16: 
###################################################
#line 455 "R4Bayes.rnw"
pbeta(0.5,out$par[1], out$par[2])
pbeta(0.95,out$par[1], out$par[2])


###################################################
### chunk number 17: 
###################################################
#line 468 "R4Bayes.rnw"
# direct simulation
Bayes1.betabin = rbeta(10000, 120.062, 45.183)
# print the quantiles
quantile(Bayes1.betabin, c(0.025,0.25,0.5,0.75,0.975))


###################################################
### chunk number 18: 
###################################################
#line 478 "R4Bayes.rnw"
# keep the parameters
x3 = 120; n3 =165; a = 0.062; b=0.183
# call the MCbinomialbeta function for 10000 simulation 
Bayes2.betabin = MCbinomialbeta(x3, n3, a, b, mc=10000)
# print the summary
summary(Bayes2.betabin)


###################################################
### chunk number 19: Bayes.fig4betabin
###################################################
#line 491 "R4Bayes.rnw"
p = seq(0,1,0.01)
plot(p, dbeta(p,a, b),  lwd=3, type="l",ylim=c(0,13),
  xlab="Healing Rate", ylab="density")
lines(density(Bayes1.betabin), lty=4, lwd=3)
lines(density(Bayes2.betabin), lty=8, lwd=3)
legend("topleft", c("Prior", "Direct Simulation","MCbinomialbeta"), 
lwd=3, lty=c(1,4,8))


###################################################
### chunk number 20: 
###################################################
#line 502 "R4Bayes.rnw"
#line 491 "R4Bayes.rnw#from line#502#"
p = seq(0,1,0.01)
plot(p, dbeta(p,a, b),  lwd=3, type="l",ylim=c(0,13),
  xlab="Healing Rate", ylab="density")
lines(density(Bayes1.betabin), lty=4, lwd=3)
lines(density(Bayes2.betabin), lty=8, lwd=3)
legend("topleft", c("Prior", "Direct Simulation","MCbinomialbeta"), 
lwd=3, lty=c(1,4,8))
#line 503 "R4Bayes.rnw"


###################################################
### chunk number 21: 
###################################################
#line 520 "R4Bayes.rnw"
# get the RODBC library
require(RODBC)
# the path of data file
datfile = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
# link to the data file
getxlsbook = odbcConnectExcel2007(datfile) 
# get the data from the excel sheet
dat = sqlFetch(getxlsbook,"DBP")
# close the RODBC
odbcCloseAll()
# we are interested in the blood pressure change
dat$diff = dat$DBP5-dat$DBP1


###################################################
### chunk number 22: bayes.lm
###################################################
#line 552 "R4Bayes.rnw"
# fit the Bayes regression model with 1000 burn-in
BayesMod  = MCMCregress(diff~TRT+Age, dat)
# print the MCMC result
summary(BayesMod)


###################################################
### chunk number 23: Bayes.fig4lm
###################################################
#line 562 "R4Bayes.rnw"
# make the margin
par(mar = c(3,2,1.5,1))
# make the MCMC plot
plot(BayesMod)


###################################################
### chunk number 24: 
###################################################
#line 571 "R4Bayes.rnw"
#line 562 "R4Bayes.rnw#from line#571#"
# make the margin
par(mar = c(3,2,1.5,1))
# make the MCMC plot
plot(BayesMod)
#line 572 "R4Bayes.rnw"


###################################################
### chunk number 25: 
###################################################
#line 580 "R4Bayes.rnw"
library(flexmix)
data("betablocker")  
betablocker$Center = as.factor(betablocker$Center)


###################################################
### chunk number 26: center1
###################################################
#line 588 "R4Bayes.rnw"
# extract center 1
beta1 = betablocker[betablocker$Center == 1,
			c("Deaths","Total","Treatment")]
# print the center 1 data
beta1 


###################################################
### chunk number 27: 
###################################################
#line 597 "R4Bayes.rnw"
# make a dataframe
beta1 = data.frame(trt = c(rep("TRT", 38),rep("Cont",39)),
	death = c(rep(1,3), rep(0,38-3), rep(1,3), rep(0,39-3))) 
# print the first 6 observations
head(beta1)


###################################################
### chunk number 28: 
###################################################
#line 608 "R4Bayes.rnw"
# fit logistic regression
glm.beta = glm(death ~trt,binomial,beta1)
# print the result
summary(glm.beta)


###################################################
### chunk number 29: 
###################################################
#line 624 "R4Bayes.rnw"
 ## Call MCMClogit with default 
Bayes1.beta = MCMClogit(death~trt, data=beta1) 
# print the summary for MCMC
summary(Bayes1.beta)


###################################################
### chunk number 30: Bayes.fig4glm
###################################################
#line 634 "R4Bayes.rnw"
plot(Bayes1.beta)


###################################################
### chunk number 31: 
###################################################
#line 639 "R4Bayes.rnw"
#line 634 "R4Bayes.rnw#from line#639#"
plot(Bayes1.beta)
#line 640 "R4Bayes.rnw"


###################################################
### chunk number 32: 
###################################################
#line 648 "R4Bayes.rnw"
# fit the Bayesian logistic regression with multivariate normal prior
Bayes2.beta = MCMClogit(death~trt, B0=.001,data=beta1) 
# print the fit
summary(Bayes2.beta)


###################################################
### chunk number 33: 
###################################################
#line 660 "R4Bayes.rnw"
library(HSAUR)
data(polyps)


###################################################
### chunk number 34: 
###################################################
#line 674 "R4Bayes.rnw"
 ## Call MCMCpoissont with default 
Bayes.polyps <- MCMCpoisson(number ~ treat+age, polyps)
# print the summary for MCMC
summary(Bayes.polyps)


###################################################
### chunk number 35: Bayes.fig4poisson
###################################################
#line 686 "R4Bayes.rnw"
# set a beta margin for plotting
par(mar = c(3,2,1.5,1))
# plot the MCMC
plot(Bayes.polyps)


###################################################
### chunk number 36: 
###################################################
#line 694 "R4Bayes.rnw"
#line 686 "R4Bayes.rnw#from line#694#"
# set a beta margin for plotting
par(mar = c(3,2,1.5,1))
# plot the MCMC
plot(Bayes.polyps)
#line 695 "R4Bayes.rnw"


###################################################
### chunk number 37: 
###################################################
#line 726 "R4Bayes.rnw"
# call the hypergeo library
library(hypergeo)
# make a function call 
pXgtY = function(sx,tx,sy,ty){
tmp1 = beta(sx+sy,tx+ty)/(beta(sx,tx)*beta(sy,ty)*sy)
tmp2 = genhypergeo(U=c(sx+sy,sy+ty,1),L=c(sy+1,sx+tx+sy+ty), 
	check_mod=FALSE,z=1)
tmp1*tmp2
}


###################################################
### chunk number 38: 
###################################################
#line 740 "R4Bayes.rnw"
# compare 800 mg C to 400 mg C
p800to400 = pXgtY(x[3], n[3]-x[3], x[2],n[2]-x[2])
p800to400
# compare 800 mg C to 0 mg C
p800to0 = pXgtY(x[3], n[3]-x[3], x[1],n[1]-x[1])
p800to0
# compare 1600 mg C to 800 mg C
p1600to800 = pXgtY(x[4], n[4]-x[4], x[3],n[3]-x[3])
p1600to800
# compare 1600 mg C to 400 mg C
p1600to400 = pXgtY(x[4], n[4]-x[4], x[2],n[2]-x[2])
p1600to400


###################################################
### chunk number 39: Bayes.fig4betabin2
###################################################
#line 761 "R4Bayes.rnw"
#  make p from 0.3 to 0.9 by 100 points 
n.pts =100
p      = seq(0.3,0.9,length=n.pts)
# the prior and the distributions from 4 treatments
pb0  = dbeta(p,1,1)
pb1  = dbeta(p,x[1],n[1]-x[1])
pb2  = dbeta(p,x[2],n[2]-x[2])
pb3  = dbeta(p,x[3],n[3]-x[3])
pb4  = dbeta(p,x[4],n[4]-x[4])

# the maximum to set the yaxis limit
ymax= max(pb1,pb2,pb3,pb4)
# plot the prior and posterior
plot(p,pb0, lwd=1, lty=8, las=1,type="l",xlab="Healing Probability",
 ylab="Density",ylim=c(0,ymax))
lines(p,pb1, lwd=3, lty=1)
lines(p,pb2, lwd=3, lty=2)
lines(p,pb3, lwd=3, lty=3)
lines(p,pb4, lwd=3, lty=4)
legend("topleft", c("0 mg C","400 mg C", "800 mg C","1600 mg C"), 
lwd=c(3,3,3,3), lty=c(1,2,3,4))


###################################################
### chunk number 40: 
###################################################
#line 786 "R4Bayes.rnw"
#line 761 "R4Bayes.rnw#from line#786#"
#  make p from 0.3 to 0.9 by 100 points 
n.pts =100
p      = seq(0.3,0.9,length=n.pts)
# the prior and the distributions from 4 treatments
pb0  = dbeta(p,1,1)
pb1  = dbeta(p,x[1],n[1]-x[1])
pb2  = dbeta(p,x[2],n[2]-x[2])
pb3  = dbeta(p,x[3],n[3]-x[3])
pb4  = dbeta(p,x[4],n[4]-x[4])

# the maximum to set the yaxis limit
ymax= max(pb1,pb2,pb3,pb4)
# plot the prior and posterior
plot(p,pb0, lwd=1, lty=8, las=1,type="l",xlab="Healing Probability",
 ylab="Density",ylim=c(0,ymax))
lines(p,pb1, lwd=3, lty=1)
lines(p,pb2, lwd=3, lty=2)
lines(p,pb3, lwd=3, lty=3)
lines(p,pb4, lwd=3, lty=4)
legend("topleft", c("0 mg C","400 mg C", "800 mg C","1600 mg C"), 
lwd=c(3,3,3,3), lty=c(1,2,3,4))
#line 787 "R4Bayes.rnw"


