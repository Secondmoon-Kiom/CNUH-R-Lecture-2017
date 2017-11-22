###################################################
# Title: Chapter 8: Meta-Analysis
# Author: Dr.DG.Chen 
###################################################


###################################################
### chunk number 2: 
###################################################
#line 68 "R4Meta.rnw"
library(rmeta)
data(cochrane)


###################################################
### chunk number 3: 
###################################################
#line 73 "R4Meta.rnw"
library(xtable)
print(xtable(cochrane, digits=0, align=rep("c", 1+ncol(cochrane)),
caption="Data for Cochrane Collaboration Logo.",label = "meta.data.steriod"),
table.placement = "htbp",caption.placement = "top",include.rownames=FALSE)


###################################################
### chunk number 5: rmeta
###################################################
#line 304 "R4Meta.rnw"
library(rmeta)


###################################################
### chunk number 6: 
###################################################
#line 313 "R4Meta.rnw"
# Get the data from the ``beta"
n.trt   = betablocker[betablocker$Treatment=="Treated",]$Total
n.ctrl  = betablocker[betablocker$Treatment=="Control",]$Total
ev.trt  = betablocker[betablocker$Treatment=="Treated",]$Deaths
ev.ctrl = betablocker[betablocker$Treatment=="Control",]$Deaths
# call the meta.MH for calculations
betaOR  = meta.MH(n.trt,n.ctrl,ev.trt,ev.ctrl,
	names=paste("Center",1:22,sep=" "))
# print the summary from meta.MH
summary(betaOR)


###################################################
### chunk number 7: meta.beta.fig4fixed
###################################################
#line 333 "R4Meta.rnw"
plot(betaOR, ylab="Center")


###################################################
### chunk number 8: 
###################################################
#line 338 "R4Meta.rnw"
#line 333 "R4Meta.rnw#from line#338#"
plot(betaOR, ylab="Center")
#line 339 "R4Meta.rnw"


###################################################
### chunk number 9: meta.beta.fig4fixed.funnel
###################################################
#line 350 "R4Meta.rnw"
funnelplot(betaOR)


###################################################
### chunk number 10: 
###################################################
#line 355 "R4Meta.rnw"
#line 350 "R4Meta.rnw#from line#355#"
funnelplot(betaOR)
#line 356 "R4Meta.rnw"


###################################################
### chunk number 11: 
###################################################
#line 367 "R4Meta.rnw"
# Call the meta.DSL for calculations
betaDSL  = meta.DSL(n.trt,n.ctrl,ev.trt,ev.ctrl,
	names=paste("Center",1:22,sep=" "))
# Print the summary from meta.DSL
summary(betaDSL)


###################################################
### chunk number 12: meta.beta.fig4random
###################################################
#line 381 "R4Meta.rnw"
plot(betaDSL, ylab="Center")


###################################################
### chunk number 13: 
###################################################
#line 386 "R4Meta.rnw"
#line 381 "R4Meta.rnw#from line#386#"
plot(betaDSL, ylab="Center")
#line 387 "R4Meta.rnw"


###################################################
### chunk number 14: meta.beta.forest
###################################################
#line 399 "R4Meta.rnw"
# Create the ``text" to include all the outputs
text = cbind(c("","Center",betaOR$names,NA,"Summary"),
             c("Deaths","(Betablockers)",ev.trt,NA,NA),
             c("Deaths","(Placebo)", ev.ctrl, NA,NA),
	     c("","OR",format(exp(betaOR$logOR),digits=2),
		    NA,format(exp(betaOR$logMH),digits=2)))
# Generate the OR and 95\% CI
mean  = c(NA,NA,betaOR$logOR,NA,betaOR$logMH)
sterr = c(NA,NA,betaOR$selogOR,NA,betaOR$selogMH)
l     = mean-1.96*sterr
u     = mean+1.96*sterr
# Call forestplot with a few options
forestplot(text,mean,l,u,zero=0,is.summary=c(TRUE,TRUE,rep(FALSE,22),TRUE),
   clip=c(log(0.1),log(2.5)), xlog=TRUE)


###################################################
### chunk number 15: 
###################################################
#line 417 "R4Meta.rnw"
#line 399 "R4Meta.rnw#from line#417#"
# Create the ``text" to include all the outputs
text = cbind(c("","Center",betaOR$names,NA,"Summary"),
             c("Deaths","(Betablockers)",ev.trt,NA,NA),
             c("Deaths","(Placebo)", ev.ctrl, NA,NA),
	     c("","OR",format(exp(betaOR$logOR),digits=2),
		    NA,format(exp(betaOR$logMH),digits=2)))
# Generate the OR and 95\% CI
mean  = c(NA,NA,betaOR$logOR,NA,betaOR$logMH)
sterr = c(NA,NA,betaOR$selogOR,NA,betaOR$selogMH)
l     = mean-1.96*sterr
u     = mean+1.96*sterr
# Call forestplot with a few options
forestplot(text,mean,l,u,zero=0,is.summary=c(TRUE,TRUE,rep(FALSE,22),TRUE),
   clip=c(log(0.1),log(2.5)), xlog=TRUE)
#line 418 "R4Meta.rnw"


###################################################
### chunk number 16: 
###################################################
#line 429 "R4Meta.rnw"
# Load the data
data(cochrane)
# print it
cochrane


###################################################
### chunk number 17: 
###################################################
#line 438 "R4Meta.rnw"
# Fit the fixed-effects model
steroid = meta.MH(n.trt, n.ctrl, ev.trt, ev.ctrl,
                        names=name, data=cochrane)
# Print the model fit
summary(steroid)


###################################################
### chunk number 18: meta.beta.cochrane
###################################################
#line 452 "R4Meta.rnw"
# Create the ``tabletext" to include all the outputs
tabletext = cbind(c("","Study",steroid$names,NA,"Summary"),
                 c("Deaths","(Steroid)",cochrane$ev.trt,NA,NA),
                 c("Deaths","(Placebo)",cochrane$ev.ctrl, NA,NA),
		 c("","OR",format(exp(steroid$logOR),digits=2),
			NA,format(exp(steroid$logMH),digits=2)))
# Generate the CI
mean   = c(NA,NA,steroid$logOR,NA,steroid$logMH)
stderr = c(NA,NA,steroid$selogOR,NA,steroid$selogMH)
l      = mean-1.96*stderr
u      = mean+1.96*stderr
# Call forestplot
forestplot(tabletext,mean,l,u,zero=0,
	is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),
   	clip=c(log(0.1),log(2.5)), xlog=TRUE)


###################################################
### chunk number 19: 
###################################################
#line 471 "R4Meta.rnw"
#line 452 "R4Meta.rnw#from line#471#"
# Create the ``tabletext" to include all the outputs
tabletext = cbind(c("","Study",steroid$names,NA,"Summary"),
                 c("Deaths","(Steroid)",cochrane$ev.trt,NA,NA),
                 c("Deaths","(Placebo)",cochrane$ev.ctrl, NA,NA),
		 c("","OR",format(exp(steroid$logOR),digits=2),
			NA,format(exp(steroid$logMH),digits=2)))
# Generate the CI
mean   = c(NA,NA,steroid$logOR,NA,steroid$logMH)
stderr = c(NA,NA,steroid$selogOR,NA,steroid$selogMH)
l      = mean-1.96*stderr
u      = mean+1.96*stderr
# Call forestplot
forestplot(tabletext,mean,l,u,zero=0,
	is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),
   	clip=c(log(0.1),log(2.5)), xlog=TRUE)
#line 472 "R4Meta.rnw"


###################################################
### chunk number 20: 
###################################################
#line 485 "R4Meta.rnw"
library(meta)


###################################################
### chunk number 21: 
###################################################
#line 491 "R4Meta.rnw"
library(help=meta)


###################################################
### chunk number 22: 
###################################################
#line 497 "R4Meta.rnw"
# Load the library
require(RODBC)
# Get the data path
datfile    = "c:/R4CTDA/dataset/datR4CTDA.xlsx"
# link the excel data book
getxlsbook = odbcConnectExcel2007(datfile)
# Get the data from the data sheet
angina        = sqlFetch(getxlsbook,"angina")
# Close the ODBC
odbcCloseAll()
# Print the data
angina


###################################################
### chunk number 23: 
###################################################
#line 518 "R4Meta.rnw"
# Fit fixed-effect model
fixed.angina = metacont(nE, meanE, sqrt(varE),nC,meanC,sqrt(varC),
         data=angina,studlab=Protocol,comb.random=FALSE)
# Print the fitted model
fixed.angina


###################################################
### chunk number 24: meta.angina.fixed
###################################################
#line 530 "R4Meta.rnw"
plot(fixed.angina)


###################################################
### chunk number 25: 
###################################################
#line 535 "R4Meta.rnw"
#line 530 "R4Meta.rnw#from line#535#"
plot(fixed.angina)
#line 536 "R4Meta.rnw"


###################################################
### chunk number 26: meta.angina.fixed2
###################################################
#line 545 "R4Meta.rnw"
# Round to 2-digit
fixed.angina$mean.e = round(fixed.angina$mean.e,2)
fixed.angina$sd.e = round(fixed.angina$sd.e,2)
fixed.angina$mean.c = round(fixed.angina$mean.c,2)
fixed.angina$sd.c = round(fixed.angina$sd.c,2)
# Call forest to make plot
forest(fixed.angina)


###################################################
### chunk number 27: 
###################################################
#line 556 "R4Meta.rnw"
#line 545 "R4Meta.rnw#from line#556#"
# Round to 2-digit
fixed.angina$mean.e = round(fixed.angina$mean.e,2)
fixed.angina$sd.e = round(fixed.angina$sd.e,2)
fixed.angina$mean.c = round(fixed.angina$mean.c,2)
fixed.angina$sd.c = round(fixed.angina$sd.c,2)
# Call forest to make plot
forest(fixed.angina)
#line 557 "R4Meta.rnw"


###################################################
### chunk number 28: meta.angina.funnel
###################################################
#line 565 "R4Meta.rnw"
funnel(fixed.angina)


###################################################
### chunk number 29: 
###################################################
#line 570 "R4Meta.rnw"
#line 565 "R4Meta.rnw#from line#570#"
funnel(fixed.angina)
#line 571 "R4Meta.rnw"


###################################################
### chunk number 30: 
###################################################
#line 582 "R4Meta.rnw"
metabias(fixed.angina)


###################################################
### chunk number 31: 
###################################################
#line 594 "R4Meta.rnw"
# fit random-effects model
random.angina = metacont(nE, meanE, sqrt(varE),nC,meanC,sqrt(varC),
         data=angina,studlab=Protocol,comb.random=T)
# print the summary fit
random.angina


