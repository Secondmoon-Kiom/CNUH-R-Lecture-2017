###################################################
# Title: Chapter 10: Bioequivalence CT
# Author: Dr. DG. Chen 
###################################################


###################################################
### chunk number 4: BE.dat2
###################################################
#line 102 "R4Bioeq.rnw"
require(RODBC)
datfile = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
getxlsbook = odbcConnectExcel2007(datfile) 
dat = sqlFetch(getxlsbook,"Cimetidine")
odbcCloseAll()


###################################################
### chunk number 5: 
###################################################
#line 109 "R4Bioeq.rnw"
library(xtable)
print(xtable(dat, digits= c(0,0,0,0,0,2,2,1),
caption="AUC, CMAX and TMAX data from Cimetidine" , 
label = "BE.Peacedat"),table.placement = "thbp", 
caption.placement="top",include.rownames=FALSE)


###################################################
### chunk number 6: BE.dat
###################################################
#line 390 "R4Bioeq.rnw"
# get the library
require(RODBC)
# point to the excel file
datfile = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
# connect to the excel book
getxlsbook = odbcConnectExcel2007(datfile) 
# read in the data in that sheet
dat = sqlFetch(getxlsbook,"ChowLiuTab361")
odbcCloseAll()


###################################################
### chunk number 7: BE.meantab
###################################################
#line 406 "R4Bioeq.rnw"
# use ``aggregate" to the sample size
tab.n    = aggregate(dat$AUC,list(seq=dat$Sequence,
                              prd=dat$Period),length)
n1       = tab.n[tab.n$seq==1 & tab.n$prd==1,]$x
n2       = tab.n[tab.n$seq==2 & tab.n$prd==1,]$x
n        = n1+n2
# use ``aggregate" to get the mean
tab.mean = aggregate(dat$AUC,list(seq=dat$Sequence,
                              prd=dat$Period),mean)
# use ``aggregate" to get the variance
tab.var  = aggregate(dat$AUC,list(seq=dat$Sequence,
                              prd=dat$Period),var)
# make a dataframe for the summary data
summaryTab = data.frame(Sequence=tab.mean$seq,
Period=tab.mean$prd, numSample = tab.n$x, 
Mean = tab.mean$x, Var=tab.var$x)
# print the summary table
round(summaryTab,2)


###################################################
### chunk number 8: 
###################################################
#line 446 "R4Bioeq.rnw"
tapply(dat$AUC, dat[,c("Sequence","Period")], mean)


###################################################
### chunk number 9: BE.Uik
###################################################
#line 466 "R4Bioeq.rnw"
Uik           = aggregate(dat$AUC,
               list(seq = dat$Sequence,sub=dat$Subject), sum) 
colnames(Uik) = c("seq", "sub","Uik")


###################################################
### chunk number 10: BE.mUk
###################################################
#line 478 "R4Bioeq.rnw"
mUk  = aggregate(Uik$Uik, list(seq=Uik$seq), mean) 
colnames(mUk) = c("seq", "mUk")
print(mUk)


###################################################
### chunk number 11: BE.C
###################################################
#line 490 "R4Bioeq.rnw"
hatC = mUk[2,2]-mUk[1,2]
hatC


###################################################
### chunk number 12: BE.var
###################################################
#line 506 "R4Bioeq.rnw"
dU    = merge(Uik, mUk)
sigu2 = sum((dU$Uik-dU$mUk)^2)/(n1+n2-2) 
sigu2


###################################################
### chunk number 13: 
###################################################
#line 517 "R4Bioeq.rnw"
se.sigu = sqrt(sigu2*(1/n1+1/n2)) 
TC      = hatC/se.sigu
TC


###################################################
### chunk number 14: BE.pC
###################################################
#line 527 "R4Bioeq.rnw"
pC = 2*(1-pt(abs(TC), n1+n2-2))  
pC


###################################################
### chunk number 15: BE.dd
###################################################
#line 539 "R4Bioeq.rnw"
dik          = aggregate(dat$AUC,
               list(sub=dat$Subject,seq=dat$Sequence),diff)
dik$x        = dik$x/2  
colnames(dik)= c("sub", "seq","dik")
dik


###################################################
### chunk number 16: 
###################################################
#line 553 "R4Bioeq.rnw"
mdk           = aggregate(dik$dik, list(seq=dik$seq), mean) 
colnames(mdk) = c("seq", "mdk")
hatF          = mdk[1,2]-mdk[2,2]
hatF


###################################################
### chunk number 17: BE.vard
###################################################
#line 573 "R4Bioeq.rnw"
dF    = merge(dik, mdk)
sigd2 = sum((dF$dik-dF$mdk)^2)/(n1+n2-2) 
sigd2


###################################################
### chunk number 18: 
###################################################
#line 584 "R4Bioeq.rnw"
se.sigd = sqrt(sigd2*(1/n1+1/n2)) 
TF      = hatF/se.sigd
TF


###################################################
### chunk number 19: 
###################################################
#line 594 "R4Bioeq.rnw"
pF = 2*(1-pt(abs(TF), n1+n2-2))  
pF


###################################################
### chunk number 20: BE.ANOVA
###################################################
#line 609 "R4Bioeq.rnw"
# cat("We first re-format the data into R dataframe","\n")
Data = data.frame(subj = as.factor(dat$Subject),
                   drug = as.factor(dat$Formulation), 
                   seq  = as.factor(dat$Sequence), 
                   prd  = as.factor(dat$Period),
                   AUC  = dat$AUC)
# cat("Then call R function aov for ANOVA Table", "\n")
summary(aov(AUC ~ seq*drug + Error(subj), data = Data))


###################################################
### chunk number 21: BE.decisionCI
###################################################
#line 636 "R4Bioeq.rnw"
# get the mean for AUC by Formulation
mdrug    = tapply(dat$AUC, list(drug=dat$Formulation), mean)
# extract the means
ybarT    = mdrug["T"]
ybarR    = mdrug["R"]
# make the decision CI
dec2.low = theta.L = -0.2*ybarR
dec2.up  = theta.U = 0.25*ybarR
cat("DecisionCI.mean=(",dec2.low,",",dec2.up,")",sep="","\n")


###################################################
### chunk number 22: BE.CI12
###################################################
#line 659 "R4Bioeq.rnw"
# the confidence coefficient: alpha
alphaCI  = .1
# the t-value
qt.alpha = qt(1-alphaCI, n1+n2-2)
qt.alpha
# the lower and upper limits for CI1
low1 = (ybarT-ybarR)-qt.alpha*sqrt(sigd2)*sqrt(1/n1+1/n2)
up1  = (ybarT-ybarR)+qt.alpha*sqrt(sigd2)*sqrt(1/n1+1/n2)
cat("The classical CI1=(", round(low1,3),",", 
round(up1,3),")", sep=" ","\n\n")
# the lower and upper limits for CI2
low2 = (low1/ybarR+1)*100
up2  = (up1/ybarR+1)*100
cat("The Ratio CI2=(", round(low2,3),",", 
round(up2,3),")", sep=" ","\n\n")


###################################################
### chunk number 23: 
###################################################
#line 686 "R4Bioeq.rnw"
k12 = 2*(ybarR-ybarT)/sqrt( sigd2*(1/n1+1/n2))


###################################################
### chunk number 24: 
###################################################
#line 692 "R4Bioeq.rnw"
k2 = uniroot(function(k2) pt(k12-k2,n1+n2-2)- pt(k2,n1+n2-2)
-(1-alphaCI),lower = -10, upper = 10, tol = 0.0001)$root
k1 =k12-k2
cat("The Westlake k1=",k1," and k2=",k2,sep=" ", "\n\n")


###################################################
### chunk number 25: 
###################################################
#line 700 "R4Bioeq.rnw"
low.west = k2*sqrt(sigd2*(1/n1+1/n2))-(ybarR-ybarT)
up.west  = k1*sqrt(sigd2*(1/n1+1/n2))-(ybarR-ybarT)
cat("The Westlake CI for mu_T-mu_A is 
(",low.west,",",up.west,")",sep=" ", "\n\n")


###################################################
### chunk number 26: 
###################################################
#line 712 "R4Bioeq.rnw"
TL = (ybarT-ybarR-theta.L)/sqrt(sigd2*(1/n1+1/n2)) 
TU = (ybarT-ybarR-theta.U)/sqrt(sigd2*(1/n1+1/n2))


###################################################
### chunk number 27: 
###################################################
#line 723 "R4Bioeq.rnw"
pL     = 1-pt(abs(TL), n1+n2-2)
pU     = pt(TU,n1+n2-2)
p1side = max(pL, pU)


###################################################
### chunk number 28: BE.bayes
###################################################
#line 736 "R4Bioeq.rnw"
tL  = (theta.L -(ybarT-ybarR))/sqrt(sigd2*(1/n1+1/n2)) 
tU  = (theta.U -(ybarT-ybarR))/sqrt(sigd2*(1/n1+1/n2))
pRD = pt(tU, n1+n2-2) - pt(tL, n1+n2-2)
pRD


###################################################
### chunk number 29: 
###################################################
#line 753 "R4Bioeq.rnw"
dR   = dat[dat$Formulation=="R",c("Subject","AUC")]
dT   = dat[dat$Formulation=="T",c("Subject","AUC")]
colnames(dR) = c("Subject","AUC4R")
colnames(dT) = c("Subject","AUC4T")
dRT  = merge(dR,dT)
rT2R = dRT$AUC4T/dRT$AUC4R
rT2R


###################################################
### chunk number 30: 
###################################################
#line 764 "R4Bioeq.rnw"
k       = 1/sqrt(1-.9)
rbar    = mean(rT2R)
sigrbar = sqrt(var(rT2R)/n)
rbar
sigrbar


###################################################
### chunk number 31: BE.BTCI
###################################################
#line 773 "R4Bioeq.rnw"
low.BT = rbar-k*sigrbar
up.BT  = rbar+k*sigrbar
cat("The Tchebycheff CI for mu_T/mu_A is 
(",low.BT,",",up.BT,")",sep="", "\n\n")


###################################################
### chunk number 32: BE.boot
###################################################
#line 791 "R4Bioeq.rnw"
# B=number of bootstrap
B     = 2000
# boota and bootb to keep track the bootstrap results
boota = bootb = NULL
for(b in 1:B){
# Boottrap the observed individual ratios
boota[b] = mean(sample(rT2R, replace=T))
# boottrap the individuals and calculate the means
tmp = dRT[sample(1:n, replace=T),]
bootb[b] = mean(tmp$AUC4T)/mean(tmp$AUC4R)
}


###################################################
### chunk number 33: 
###################################################
#line 805 "R4Bioeq.rnw"

qxa = quantile(boota, c(0.05, 0.95))
qxa
qxb = quantile(bootb, c(0.05, 0.95))
qxb


###################################################
### chunk number 34: BE.boot1plot
###################################################
#line 818 "R4Bioeq.rnw"
hist(boota,nclass=30, freq=F,las=1, 
xlab="Mean of the Ratios", ylab="Density", main="")
box()
den = density(boota)
lines(den, lwd=2)
qya = approx(den$x, den$y, c(qxa,rbar))$y
segments(qxa[1],0,qxa[1],qya[1], lwd=5)
segments(qxa[2],0,qxa[2],qya[2], lwd=5)
segments(rbar,0,rbar,qya[3],lty=4, lwd=5)


###################################################
### chunk number 35: 
###################################################
#line 832 "R4Bioeq.rnw"
#line 818 "R4Bioeq.rnw#from line#832#"
hist(boota,nclass=30, freq=F,las=1, 
xlab="Mean of the Ratios", ylab="Density", main="")
box()
den = density(boota)
lines(den, lwd=2)
qya = approx(den$x, den$y, c(qxa,rbar))$y
segments(qxa[1],0,qxa[1],qya[1], lwd=5)
segments(qxa[2],0,qxa[2],qya[2], lwd=5)
segments(rbar,0,rbar,qya[3],lty=4, lwd=5)
#line 833 "R4Bioeq.rnw"


###################################################
### chunk number 36: BE.boot2plot
###################################################
#line 840 "R4Bioeq.rnw"
hist(bootb,nclass=30, freq=F,las=1, 
xlab="Ratio of the Means", ylab="Density", main="")
box()
den = density(bootb)
lines(den, lwd=2)
rmean = mean(dRT$AUC4T)/mean(dRT$AUC4R)
qyb = approx(den$x, den$y, c(qxb,rmean))$y
segments(qxb[1],0,qxb[1],qyb[1], lwd=5)
segments(qxb[2],0,qxb[2],qyb[2], lwd=5)
segments(rmean,0,rmean,qyb[3],lty=4, lwd=5)


###################################################
### chunk number 37: 
###################################################
#line 855 "R4Bioeq.rnw"
#line 840 "R4Bioeq.rnw#from line#855#"
hist(bootb,nclass=30, freq=F,las=1, 
xlab="Ratio of the Means", ylab="Density", main="")
box()
den = density(bootb)
lines(den, lwd=2)
rmean = mean(dRT$AUC4T)/mean(dRT$AUC4R)
qyb = approx(den$x, den$y, c(qxb,rmean))$y
segments(qxb[1],0,qxb[1],qyb[1], lwd=5)
segments(qxb[2],0,qxb[2],qyb[2], lwd=5)
segments(rmean,0,rmean,qyb[3],lty=4, lwd=5)
#line 856 "R4Bioeq.rnw"


###################################################
### chunk number 38: 
###################################################
#line 865 "R4Bioeq.rnw"
shapiro.test(bootb)


###################################################
### chunk number 39: BE.qqnorm
###################################################
#line 869 "R4Bioeq.rnw"
qqnorm(bootb, main="")
qqline(bootb)


###################################################
### chunk number 40: 
###################################################
#line 876 "R4Bioeq.rnw"
#line 869 "R4Bioeq.rnw#from line#876#"
qqnorm(bootb, main="")
qqline(bootb)
#line 877 "R4Bioeq.rnw"


###################################################
### chunk number 41: BE.pdat
###################################################
#line 889 "R4Bioeq.rnw"
require(RODBC)
datfile    = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
getxlsbook = odbcConnectExcel2007(datfile) 
datRaw0    = sqlFetch(getxlsbook,"CimetidineRaw", colnames=F)
odbcCloseAll()


###################################################
### chunk number 42: BE.pdat
###################################################
#line 909 "R4Bioeq.rnw"
require(RODBC)
datfile    = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
getxlsbook = odbcConnectExcel2007(datfile) 
datRaw0    = sqlFetch(getxlsbook,"CimetidineRaw", colnames=F)
odbcCloseAll()
print(datRaw0)


###################################################
### chunk number 43: 
###################################################
#line 922 "R4Bioeq.rnw"
datRaw           = reshape(datRaw0, direction="long", 
		    varying=-1,idvar = "Subject",sep="")
datRaw$time      = datRaw$time/10
colnames(datRaw) = c("subj","time","conc")
head(datRaw)


###################################################
### chunk number 44: BE.fig.pdat
###################################################
#line 932 "R4Bioeq.rnw"
library(lattice)
print(xyplot(conc~time,group=subj,datRaw,xlab="Time(HR)", 
xlim=c(0,13),auto.key = list(corner=c(1,1),lines = TRUE) ,
ylab="Concentration(mCG/ML)",type=c("p","a")))


###################################################
### chunk number 45: 
###################################################
#line 941 "R4Bioeq.rnw"
#line 932 "R4Bioeq.rnw#from line#941#"
library(lattice)
print(xyplot(conc~time,group=subj,datRaw,xlab="Time(HR)", 
xlim=c(0,13),auto.key = list(corner=c(1,1),lines = TRUE) ,
ylab="Concentration(mCG/ML)",type=c("p","a")))
#line 942 "R4Bioeq.rnw"


###################################################
### chunk number 46: BE.fig.mpdat
###################################################
#line 952 "R4Bioeq.rnw"
# make the mean concentration
dat.mean= aggregate(datRaw$conc, list(time=datRaw$time), mean)
# plot it with a line
plot(conc~time,las=1,type="n",datRaw,xlab="Time",xlim=c(0,13),
ylim=c(0, 4), ylab="Mean Concentration")
lines(x~time,dat.mean, lty=1, lwd=3)


###################################################
### chunk number 47: 
###################################################
#line 963 "R4Bioeq.rnw"
#line 952 "R4Bioeq.rnw#from line#963#"
# make the mean concentration
dat.mean= aggregate(datRaw$conc, list(time=datRaw$time), mean)
# plot it with a line
plot(conc~time,las=1,type="n",datRaw,xlab="Time",xlim=c(0,13),
ylim=c(0, 4), ylab="Mean Concentration")
lines(x~time,dat.mean, lty=1, lwd=3)
#line 964 "R4Bioeq.rnw"


###################################################
### chunk number 48: BE.fn.beta
###################################################
#line 973 "R4Bioeq.rnw"
# function `make.beta' with argument `dt'
make.beta = function(dt){
# terminal elimination beta to find the slope for R2(k+1) <R2(k)
n = length(dt$conc) # get the length of data

# end the loop at tmax
tmax = which.max(dt$conc)

# loop over starting from the last time-conc point to tmax
for(k in n:tmax){ 
dt1 = dt[((k-2):n),] # start with last 3 pts and move on
dt2 = dt[((k-3):n),] # start with last 4 pts and move on
# some date have 0s at the end of t-c curve and make the lm crash
# so make this dataframe at least 3 data points
if( dim(dt1[dt1$conc>0,])[1]>= 3 ){
# fit log(conc) to time and track the r-square
m1    = lm(log(conc)~time, dt1[(dt1$conc>0),]) 
m2    = lm(log(conc)~time, dt2[(dt2$conc>0),]) 
betat = m1$coef[[2]]
#cat("Check=",summary(m1)$r.squared > summary(m2)$r.squared," 
#and Stopped at", k, "with beta=",betat,sep=" ","\n\n")
if(summary(m1)$r.squared > summary(m2)$r.squared) break
	} # end of if-loop
} # end of k-for-loop
#cat("final beta=",betat,"\n\n")
# return
betat
} # end of make-beta function


###################################################
### chunk number 49: BE.fn.make
###################################################
#line 1005 "R4Bioeq.rnw"
make   = function(dt){
time   = dt$time; conc = dt$conc 
#  calculate AUC
t.dif  = diff(time) # the t(i)-t(i-1)
c.mean = (conc[-1]+conc[-length(conc)])/2
auc    = sum(t.dif*c.mean)
# Cmax
cmax   = max(conc)
# tmax 
tmax   = dt[which.max(dt$conc),]$time
# terminal elimination beta to find the slope for R2(k+1) <R2(k)
betat  = make.beta(dt)
# terminal halflife
t5     = round(-log(2)/betat*2.303,1)
# AUC infinite
aucinf = auc+ conc[length(conc)]/betat 
# return the results.
c(auc,cmax,tmax, betat, t5, aucinf)
}


###################################################
### chunk number 50: 
###################################################
#line 1028 "R4Bioeq.rnw"
name.subj   = sort(unique(datRaw$subj))
num.subj    = length(name.subj)
endpts      = matrix(0, nrow=num.subj, ncol=7)
colnames(endpts) = c("subj","AUC","CMAX","TMAX",
	"betat","t5","AUCinf") 
for(id in 1:num.subj){
tmp         = datRaw[(datRaw$subj == name.subj[id]),
		c("time","conc")]
endpts[id,] = c(name.subj[id],make(tmp))
}
endpts


###################################################
### chunk number 51: 
###################################################
#line 1051 "R4Bioeq.rnw"
require(RODBC)
datfile = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
getxlsbook = odbcConnectExcel2007(datfile) 
dat = sqlFetch(getxlsbook,"Cimetidine", colnames=F)
odbcCloseAll()


###################################################
### chunk number 52: BE.pANOVA
###################################################
#line 1062 "R4Bioeq.rnw"
Data = data.frame(subj = as.factor(dat$Subject),
                   drug = as.factor(dat$Formulation), 
                   seq  = as.factor(dat$Sequence), 
                   prd  = as.factor(dat$Period),
                   AUC  = dat$AUC, lAUC= log(dat$AUC),
		   CMAX = dat$CMAX, lCMAX= log(dat$CMAX))


###################################################
### chunk number 53: 
###################################################
#line 1072 "R4Bioeq.rnw"
nsubj = tapply(dat$Subject, list(dat$Sequence), length)/2
n1    = nsubj[1]
n2    = nsubj[2]
n     = n1+n2


###################################################
### chunk number 54: 
###################################################
#line 1089 "R4Bioeq.rnw"
# the fixed model using lm for "formulation" and "period" 
mdAUC   = lm(AUC ~ seq + subj:seq + prd + drug,data = Data)
print(anova(mdAUC))
# the random effect model using aov for carryover and other effects
mdAUC.R = aov(AUC ~ prd * drug + Error(subj), data = Data)
print(summary(mdAUC.R))


###################################################
### chunk number 55: 
###################################################
#line 1104 "R4Bioeq.rnw"
# the fixed model using lm for "formulation" and "period" 
mdlAUC  = lm(lAUC ~ seq + subj:seq + prd + drug,data = Data)
print(anova(mdlAUC))
# the random effect model using aov for carryover and other effects mdlAUC.R = aov(lAUC ~ prd * drug + Error(subj), data = Data) print(summary(mdlAUC.R))


###################################################
### chunk number 56: 
###################################################
#line 1112 "R4Bioeq.rnw"
# the fixed model using lm for "formulation" and "period" 
mdCMAX  = lm(CMAX ~ seq + subj:seq + prd + drug,data = Data)
print(anova(mdCMAX))
# the random effect model using aov for carryover and other effects
mdCMAX.R = aov(CMAX ~ prd * drug + Error(subj), data = Data)
print(summary(mdCMAX.R))


###################################################
### chunk number 57: 
###################################################
#line 1122 "R4Bioeq.rnw"
# the fixed model using lm for "formulation" and "period" 
mdlCMAX  = lm(lCMAX ~ seq + subj:seq + prd + drug,data = Data)
print(anova(mdlCMAX))
# the random effect model using aov for carryover and other effects
mdlCMAX.R = aov(lCMAX ~ prd * drug + Error(subj), data = Data)
print(summary(mdlCMAX.R))


###################################################
### chunk number 58: BE.decisionCI
###################################################
#line 1142 "R4Bioeq.rnw"
mdrug    = tapply(dat$AUC, list(drug=dat$Formulation), mean)
ybarT    = mdrug["T"]
ybarR    = mdrug["R"]
dec2.low = theta.L = -0.2*ybarR
dec2.up  = theta.U = 0.25*ybarR
cat("DecisionCI.mean=(",dec2.low,",",dec2.up,")",sep="","\n")


###################################################
### chunk number 59: BE.CI12
###################################################
#line 1160 "R4Bioeq.rnw"
# the confidence coefficient: alpha
alphaCI  = .1
# the t-value
qt.alpha = qt(1-alphaCI, n1+n2-2)
qt.alpha
# the sigma using the ANOVA model instead
sigd2 = anova(mdAUC)[5,3]/2
# the lower and upper limits for CI1
low1 = (ybarT-ybarR)-qt.alpha*sqrt(sigd2)*sqrt(1/n1+1/n2)
up1  = (ybarT-ybarR)+qt.alpha*sqrt(sigd2)*sqrt(1/n1+1/n2)
cat("The classical CI1=(", round(low1,3),",", 
round(up1,3),")", sep=" ","\n\n")
# the lower and upper limits for CI2
low2 = (low1/ybarR+1)*100
up2  = (up1/ybarR+1)*100
cat("The Ratio CI2=(", round(low2,3),",", 
round(up2,3),")", sep=" ","\n\n")


###################################################
### chunk number 60: 
###################################################
#line 1189 "R4Bioeq.rnw"
k12 = 2*(ybarR-ybarT)/sqrt( sigd2*(1/n1+1/n2))


###################################################
### chunk number 61: 
###################################################
#line 1195 "R4Bioeq.rnw"
k2 = uniroot(function(k2) pt(k12-k2,n1+n2-2)- pt(k2,n1+n2-2)
-(1-alphaCI),lower = -10, upper = 10, tol = 0.0001)$root
k1 =k12-k2
cat("The Westlake k1=",k1," and k2=",k2,sep=" ", "\n\n")


###################################################
### chunk number 62: 
###################################################
#line 1203 "R4Bioeq.rnw"
low.west = k2*sqrt(sigd2*(1/n1+1/n2))-(ybarR-ybarT)
up.west  = k1*sqrt(sigd2*(1/n1+1/n2))-(ybarR-ybarT)
cat("The Westlake CI for mu_T-mu_A is 
(",low.west,",",up.west,")",sep=" ", "\n\n")


###################################################
### chunk number 63: 
###################################################
#line 1215 "R4Bioeq.rnw"
TL = (ybarT-ybarR-theta.L)/sqrt(sigd2*(1/n1+1/n2)) 
TU = (ybarT-ybarR-theta.U)/sqrt(sigd2*(1/n1+1/n2))


###################################################
### chunk number 64: 
###################################################
#line 1226 "R4Bioeq.rnw"
pL     = 1-pt(abs(TL), n1+n2-2)
pU     = pt(TU,n1+n2-2)
p1side = max(pL, pU)


###################################################
### chunk number 65: BE.bayes
###################################################
#line 1239 "R4Bioeq.rnw"
tL  = (theta.L -(ybarT-ybarR))/sqrt(sigd2*(1/n1+1/n2)) 
tU  = (theta.U -(ybarT-ybarR))/sqrt(sigd2*(1/n1+1/n2))
pRD = pt(tU, n1+n2-2) - pt(tL, n1+n2-2)
pRD


###################################################
### chunk number 66: 
###################################################
#line 1256 "R4Bioeq.rnw"
dR   = dat[dat$Formulation=="R",c("Subject","AUC")]
dT   = dat[dat$Formulation=="T",c("Subject","AUC")]
colnames(dR) = c("Subject","AUC4R")
colnames(dT) = c("Subject","AUC4T")
dRT  = merge(dR,dT)
rT2R = dRT$AUC4T/dRT$AUC4R
rT2R


###################################################
### chunk number 67: 
###################################################
#line 1267 "R4Bioeq.rnw"
k       = 1/sqrt(1-.9)
rbar    = mean(rT2R)
sigrbar = sqrt(var(rT2R)/n)
rbar
sigrbar


###################################################
### chunk number 68: BE.BTCI
###################################################
#line 1276 "R4Bioeq.rnw"
low.BT = rbar-k*sigrbar
up.BT  = rbar+k*sigrbar
cat("The Tchebycheff CI for mu_T/mu_A is 
(",low.BT,",",up.BT,")",sep="", "\n\n")


###################################################
### chunk number 69: BE.boot
###################################################
#line 1293 "R4Bioeq.rnw"
# B=number of bootstrap
B     = 2000
# boota and bootb to keep track the bootstrap results
boota = bootb = NULL
for(b in 1:B){
# Boottrap the observed individual ratios
boota[b] = mean(sample(rT2R, replace=T))
# boottrap the individuals and calculate the means
tmp = dRT[sample(1:n, replace=T),]
bootb[b] = mean(tmp$AUC4T)/mean(tmp$AUC4R)
}


###################################################
### chunk number 70: 
###################################################
#line 1308 "R4Bioeq.rnw"
qxa = quantile(boota, c(0.05, 0.95))
qxa
qxb = quantile(bootb, c(0.05, 0.95))
qxb


###################################################
### chunk number 71: BE.boot1plot
###################################################
#line 1321 "R4Bioeq.rnw"
hist(boota,nclass=30, freq=F,las=1, 
xlab="Mean of the Ratios", ylab="Density", main="")
box()
den = density(boota)
lines(den, lwd=2)
qya = approx(den$x, den$y, c(qxa,rbar))$y
segments(qxa[1],0,qxa[1],qya[1], lwd=5)
segments(qxa[2],0,qxa[2],qya[2], lwd=5)
segments(rbar,0,rbar,qya[3],lty=4, lwd=5)


###################################################
### chunk number 72: 
###################################################
#line 1335 "R4Bioeq.rnw"
#line 1321 "R4Bioeq.rnw#from line#1335#"
hist(boota,nclass=30, freq=F,las=1, 
xlab="Mean of the Ratios", ylab="Density", main="")
box()
den = density(boota)
lines(den, lwd=2)
qya = approx(den$x, den$y, c(qxa,rbar))$y
segments(qxa[1],0,qxa[1],qya[1], lwd=5)
segments(qxa[2],0,qxa[2],qya[2], lwd=5)
segments(rbar,0,rbar,qya[3],lty=4, lwd=5)
#line 1336 "R4Bioeq.rnw"


###################################################
### chunk number 73: BE.boot2plot
###################################################
#line 1343 "R4Bioeq.rnw"
hist(bootb,nclass=30, freq=F,las=1, 
xlab="Ratio of the Means", ylab="Density", main="")
box()
den = density(bootb)
lines(den, lwd=2)
rmean = mean(dRT$AUC4T)/mean(dRT$AUC4R)
qyb = approx(den$x, den$y, c(qxb,rmean))$y
segments(qxb[1],0,qxb[1],qyb[1], lwd=5)
segments(qxb[2],0,qxb[2],qyb[2], lwd=5)
segments(rmean,0,rmean,qyb[3],lty=4, lwd=5)


###################################################
### chunk number 74: 
###################################################
#line 1358 "R4Bioeq.rnw"
#line 1343 "R4Bioeq.rnw#from line#1358#"
hist(bootb,nclass=30, freq=F,las=1, 
xlab="Ratio of the Means", ylab="Density", main="")
box()
den = density(bootb)
lines(den, lwd=2)
rmean = mean(dRT$AUC4T)/mean(dRT$AUC4R)
qyb = approx(den$x, den$y, c(qxb,rmean))$y
segments(qxb[1],0,qxb[1],qyb[1], lwd=5)
segments(qxb[2],0,qxb[2],qyb[2], lwd=5)
segments(rmean,0,rmean,qyb[3],lty=4, lwd=5)
#line 1359 "R4Bioeq.rnw"


