###################################################
# Title: R code for Chapter 3: Treatment Comparison 
# Author: Dr. DG. Chen 
###################################################

###################################################
### chunk number 2: 
###################################################
#line 55 "R4ANOVA.rnw"
require(RODBC)
datfile    = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
getxlsbook = odbcConnectExcel2007(datfile) 
dat        = sqlFetch(getxlsbook,"DBP")
odbcCloseAll()


###################################################
### chunk number 3: 
###################################################
#line 63 "R4ANOVA.rnw"
library(xtable)
print(xtable(dat, digits=0, align=rep("c", 1+ncol(dat)),
caption="Diastolic Blood Pressure Trial Data.",label = "ANOVA.data.DBP"),
table.placement = "htbp",caption.placement = "top",include.rownames=FALSE)


###################################################
### chunk number 4: 
###################################################
#line 247 "R4ANOVA.rnw"
# load the library
require(RODBC)
# get the data path
datfile    = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
# link the excel data book
getxlsbook = odbcConnectExcel2007(datfile) 
# get the data from the data sheet
dat        = sqlFetch(getxlsbook,"DBP")
odbcCloseAll()
# create the difference
dat$diff = dat$DBP5-dat$DBP1
# print the first a few observation
head(dat)


###################################################
### chunk number 5: ANOVA.fig4DBP1
###################################################
#line 265 "R4ANOVA.rnw"
# call boxplot
boxplot(diff~TRT, dat, xlab="Treatment", 
	ylab="DBP Changes", las=1)


###################################################
### chunk number 6: 
###################################################
#line 272 "R4ANOVA.rnw"
#line 265 "R4ANOVA.rnw#from line#272#"
# call boxplot
boxplot(diff~TRT, dat, xlab="Treatment", 
	ylab="DBP Changes", las=1)
#line 273 "R4ANOVA.rnw"


###################################################
### chunk number 7: 
###################################################
#line 283 "R4ANOVA.rnw"
# call t-test with equal variance 
t.test(diff~TRT, dat, var.equal=T)


###################################################
### chunk number 8: 
###################################################
#line 290 "R4ANOVA.rnw"
t.test(diff~TRT, dat, var.equal=F)


###################################################
### chunk number 9: 
###################################################
#line 295 "R4ANOVA.rnw"
var.test(diff~TRT, dat)


###################################################
### chunk number 10: 
###################################################
#line 302 "R4ANOVA.rnw"
wilcox.test(diff~TRT, dat)


###################################################
### chunk number 11: 
###################################################
#line 308 "R4ANOVA.rnw"
# data from treatment A
diff.A = dat[dat$TRT=="A",]$diff
# data from treatment B
diff.B = dat[dat$TRT=="B",]$diff
# call t.test for one-sided test
t.test(diff.A, diff.B,alternative="less")


###################################################
### chunk number 12: ANOVA.boot
###################################################
#line 324 "R4ANOVA.rnw"
# load the library "bootstrap"
library(bootstrap)
# define a function to calculate the mean difference 
#	between treatment groups A to B:
mean.diff = function(bn,dat) 
  diff(tapply(dat[bn,]$diff, dat[bn,]$TRT,mean))


###################################################
### chunk number 13: 
###################################################
#line 334 "R4ANOVA.rnw"
# number of bootstrap
nboot     = 1000
# call "bootstrap" function
boot.mean = bootstrap(1:dim(dat)[1], nboot, mean.diff,dat)


###################################################
### chunk number 14: ANOVA.fig4DBP1.boot
###################################################
#line 343 "R4ANOVA.rnw"
# extract the mean differences  
x = boot.mean$thetastar
# calcualte the bootstrap quantiles
x.quantile = quantile(x, c(0.025,0.5, 0.975))
# show the quantiles
print(x.quantile)
# make a histogram
hist(boot.mean$thetastar, xlab="Mean Differences", main="")
# add the vertical lines for the quantiles 
abline(v=x.quantile,lwd=2, lty=c(4,1,4)) 


###################################################
### chunk number 15: 
###################################################
#line 357 "R4ANOVA.rnw"
#line 343 "R4ANOVA.rnw#from line#357#"
# extract the mean differences  
x = boot.mean$thetastar
# calcualte the bootstrap quantiles
x.quantile = quantile(x, c(0.025,0.5, 0.975))
# show the quantiles
print(x.quantile)
# make a histogram
hist(boot.mean$thetastar, xlab="Mean Differences", main="")
# add the vertical lines for the quantiles 
abline(v=x.quantile,lwd=2, lty=c(4,1,4)) 
#line 358 "R4ANOVA.rnw"


###################################################
### chunk number 16: 
###################################################
#line 371 "R4ANOVA.rnw"
aggregate(dat[,3:7], list(TRT=dat$TRT), mean)


###################################################
### chunk number 17: 
###################################################
#line 378 "R4ANOVA.rnw"
# call reshpae
Dat = reshape(dat, direction="long", 
varying=c("DBP1","DBP2","DBP3","DBP4","DBP5"),
 idvar = c("Subject","TRT","Age","Sex","diff"),sep="")
colnames(Dat) = c("Subject","TRT","Age","Sex","diff","Time","DBP")
Dat$Time = as.factor(Dat$Time)
# show the first 6 observations
head(Dat)


###################################################
### chunk number 18: aov
###################################################
#line 396 "R4ANOVA.rnw"
# test treatment "A"
datA   = Dat[Dat$TRT=="A",]
test.A = aov(DBP~Time, datA)
summary(test.A)
# test treatment "B"
datB   = Dat[Dat$TRT=="B",]
test.B = aov(DBP~Time, datB)
summary(test.B)


###################################################
### chunk number 19: tukey
###################################################
#line 408 "R4ANOVA.rnw"
TukeyHSD(test.A)
TukeyHSD(test.B)


###################################################
### chunk number 20: 
###################################################
#line 422 "R4ANOVA.rnw"
mod2 = aov(DBP~ TRT*Time, Dat)
summary(mod2)


###################################################
### chunk number 21: ANOVA.fig4DBP1.interaction
###################################################
#line 428 "R4ANOVA.rnw"
par(mfrow=c(2,1),mar=c(5,3,1,1))
with(Dat,interaction.plot(Time,TRT,DBP,las=1,legend=T))
with(Dat,interaction.plot(TRT,Time,DBP,las=1,legend=T))


###################################################
### chunk number 22: 
###################################################
#line 435 "R4ANOVA.rnw"
#line 428 "R4ANOVA.rnw#from line#435#"
par(mfrow=c(2,1),mar=c(5,3,1,1))
with(Dat,interaction.plot(Time,TRT,DBP,las=1,legend=T))
with(Dat,interaction.plot(TRT,Time,DBP,las=1,legend=T))
#line 436 "R4ANOVA.rnw"


###################################################
### chunk number 23: 
###################################################
#line 445 "R4ANOVA.rnw"
TukeyHSD(aov(DBP ~ TRT*Time,Dat)) 


###################################################
### chunk number 24: 
###################################################
#line 472 "R4ANOVA.rnw"
n  = c(168, 182, 165,188) 
p4 = c(.41, .62, .73, .77)
x4 = c(69, 113, 120, 145)


###################################################
### chunk number 25: 
###################################################
#line 480 "R4ANOVA.rnw"
prop.test(x4, n)


###################################################
### chunk number 26: 
###################################################
#line 487 "R4ANOVA.rnw"
prop.test(x4[c(1,3)], n[c(1,3)])


###################################################
### chunk number 27: 
###################################################
#line 491 "R4ANOVA.rnw"
prop.test(x4[c(2,3)], n[c(2,3)])


###################################################
### chunk number 28: 
###################################################
#line 495 "R4ANOVA.rnw"
prop.test(x4[c(3,4)], n[c(3,4)])


###################################################
### chunk number 29: 
###################################################
#line 503 "R4ANOVA.rnw"
# create a dataframe for the Ulcer trial
Ulcer = data.frame(
# use ``factor" to create the treatment factor
trt   = factor(rep(c("0 mg C","400 mg C","800 mg C","1600 mg C"), 
each=2),levels=c("0 mg C","400 mg C","800 mg C","1600 mg C") ), 
Heal  = c("Yes","No","Yes","No","Yes","No","Yes","No"), 
y     = c(x4[1],n[1]-x4[1],x4[2],n[2]-x4[2],x4[3],
	 n[3]-x4[3],x4[4],n[4]-x4[4]))
Ulcer


###################################################
### chunk number 30: 
###################################################
#line 516 "R4ANOVA.rnw"
tab.Ulcer = xtabs(y~trt+Heal,Ulcer)
tab.Ulcer


###################################################
### chunk number 31: ANOVA.fig4ulcer1
###################################################
#line 522 "R4ANOVA.rnw"
# layout for the plot
par(mfrow=c(1,2), mar=c(4,2,1,1))
# call ``dotchart"
dotchart(tab.Ulcer)
# call ``mosaicplot"
mosaicplot(tab.Ulcer,color=T,las=1, main=" ", 
	xlab="Treatment",ylab="Heal Status" )


###################################################
### chunk number 32: 
###################################################
#line 533 "R4ANOVA.rnw"
#line 522 "R4ANOVA.rnw#from line#533#"
# layout for the plot
par(mfrow=c(1,2), mar=c(4,2,1,1))
# call ``dotchart"
dotchart(tab.Ulcer)
# call ``mosaicplot"
mosaicplot(tab.Ulcer,color=T,las=1, main=" ", 
	xlab="Treatment",ylab="Heal Status" )
#line 534 "R4ANOVA.rnw"


###################################################
### chunk number 33: 
###################################################
#line 541 "R4ANOVA.rnw"
margin.table(tab.Ulcer,1)


###################################################
### chunk number 34: 
###################################################
#line 546 "R4ANOVA.rnw"
summary(tab.Ulcer)


