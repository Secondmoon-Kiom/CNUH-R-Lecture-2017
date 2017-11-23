###################################################
# Title: R code for Chapter 1: Introduction to R
# Author: DR. DG. Chen
###################################################

###################################################
### chunk number 2: RI-welcome
###################################################
#line 108 "RIntro.rnw"
cat("\n>\n")
options(prompt = "> ")


###################################################
### chunk number 3: RI-x
###################################################
#line 114 "RIntro.rnw"
x = 1+2


###################################################
### chunk number 4: RI-printx
###################################################
#line 119 "RIntro.rnw"
print(x)

###################################################
### chunk number 9: RI-simuNorm
###################################################
#line 295 "RIntro.rnw"
help(rnorm)


###################################################
### chunk number 10: RI-input
###################################################
#line 308 "RIntro.rnw"
# simulated input values
n      = 100
mu     = 100
sd     = 10
mu.d   = 20
age.mu = 50
age.sd = 10


###################################################
### chunk number 11: RI-placebo
###################################################
#line 321 "RIntro.rnw"
# fix the seed for random number generation 
set.seed(123)
# use "rnorm" to generate random normal
age         = rnorm(n, age.mu, age.sd)
bp.base     = rnorm(n,mu,sd)
bp.end      = rnorm(n,mu,sd)
# take the difference between endpoint and baseline
bp.diff     = bp.end-bp.base
# put the data together using "cbind" to column-bind
dat4placebo = round(cbind(age,bp.base,bp.end,bp.diff))


###################################################
### chunk number 12: RI-headplacebo
###################################################
#line 338 "RIntro.rnw"
head(dat4placebo)


###################################################
### chunk number 13: RI-drug
###################################################
#line 345 "RIntro.rnw"
age      = rnorm(n, age.mu, age.sd)
bp.base  = rnorm(n,mu,sd)
bp.end   = rnorm(n,mu-mu.d,sd)
bp.diff  = bp.end-bp.base
dat4drug = round(cbind(age,bp.base,bp.end,bp.diff))


###################################################
### chunk number 14: RI-dat
###################################################
#line 355 "RIntro.rnw"
# make a dataframe to hold all data
dat     = data.frame(rbind(dat4placebo,dat4drug))
# make "trt" as a factor for treatment.
dat$trt = as.factor(rep(c("Placebo", "Drug"), each=n))


###################################################
### chunk number 15: 
###################################################
#line 367 "RIntro.rnw"
# check the data dimension
dim(dat)
# print the first 6 obervations to see the variable names
head(dat)


###################################################
### chunk number 16: RI.boxplot4placebo
###################################################
#line 379 "RIntro.rnw"
# call boxplot
boxplot(dat4placebo, las=1, main="Placebo")


###################################################
### chunk number 17: 
###################################################
#line 388 "RIntro.rnw"
#line 379 "RIntro.rnw#from line#388#"
# call boxplot
boxplot(dat4placebo, las=1, main="Placebo")
#line 389 "RIntro.rnw"


###################################################
### chunk number 18: RI.boxplot4drug
###################################################
#line 397 "RIntro.rnw"
boxplot(dat4drug, las=1, main="Drug")


###################################################
### chunk number 19: 
###################################################
#line 402 "RIntro.rnw"
#line 397 "RIntro.rnw#from line#402#"
boxplot(dat4drug, las=1, main="Drug")
#line 403 "RIntro.rnw"


###################################################
### chunk number 20: RI.xyplot.dat
###################################################
#line 425 "RIntro.rnw"
#load the lattice library
library(lattice)
# call xyplot function and print it
print(xyplot(bp.diff~age|trt, data=dat,xlab="Age", 
strip=strip.custom(bg="white"), 
ylab="Blood Pressure Difference",lwd=3,cex=1.3,pch=20,
type=c("p", "r")))


###################################################
### chunk number 21: 
###################################################
#line 436 "RIntro.rnw"
#line 425 "RIntro.rnw#from line#436#"
#load the lattice library
library(lattice)
# call xyplot function and print it
print(xyplot(bp.diff~age|trt, data=dat,xlab="Age", 
strip=strip.custom(bg="white"), 
ylab="Blood Pressure Difference",lwd=3,cex=1.3,pch=20,
type=c("p", "r")))
#line 437 "RIntro.rnw"


###################################################
### chunk number 22: RI-lm
###################################################
#line 461 "RIntro.rnw"
lm1 = lm(bp.diff~trt*age, data=dat)
summary(lm1)


###################################################
### chunk number 23: tablm
###################################################
#line 474 "RIntro.rnw"
# load the xtable library
library(xtable)
# call xtable to make the table
print(xtable(lm1, caption="ANOVA Table for Simulated 
Clinical Trial Data", label = "tab4RI.coef"),
table.placement = "htbp",caption.placement = "top")


###################################################
### chunk number 24: RI.fig4lm
###################################################
#line 484 "RIntro.rnw"
layout(matrix(1:4, nrow=2))
plot(lm1)


###################################################
### chunk number 25: 
###################################################
#line 490 "RIntro.rnw"
#line 484 "RIntro.rnw#from line#490#"
layout(matrix(1:4, nrow=2))
plot(lm1)
#line 491 "RIntro.rnw"


