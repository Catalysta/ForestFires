#################FOREST FIRES ANALYSIS########################
##################C. Sousa####################################

#Read in data
forest <-read.csv("forestfires.csv")
#split off testing set
set.seed(6950)
indices<-sample(1:517,388)
forest<-forest[indices,]
test<-forest[-indices,]

attach(forest)

#split fires into burn area categories as defined by Ozbayoglu, Bozer
#1.32, 63.86, 273.05, 773.17
#5 categories
forest$size <- cut(forest$area, c(-.01,1.32,63.86,273.05,773.17,2000))
plot(log(area+1)~size, 
     names = c("Very Small","Small","Medium","Large","Very Large"),
     main = "Log(area) vs. Fire Size", data = forest)
#large and small categories
forest$large <- cut(forest$area, c(-.01,63.86,2000))

#zero and nonzero categories
forest$nonzero <- cut(forest$area, c(-.01,0,2000))
levels(forest$nonzero) <- c("zero","nonzero")

###################################DATA CLEANING##############################
#log transform the area variable
forest$Larea<-log(forest$area + 1)

#put area and Larea variables at start of data set
forest<-cbind(forest$area,forest$Larea,forest[,5:11],forest[,c(1:4,12,14:16)])
#change X and Y to factor variables
forest$X<-as.factor(forest$X)
forest$Y<-as.factor(forest$Y)
#change rain to binomial variable
forest$rain<-ifelse(forest$rain>0,1,0)
forest$rain<-as.factor(forest$rain)
levels(forest$rain) <- c("no rain","rain")
#transform FFMC
forest$FFMC <- log(-forest$FFMC+ max(forest$FFMC)+1)
colnames(forest)[3]<-"TrFFMC"
#remove outlier in ISI
forest<-forest[which(forest$ISI<40),]
colnames(forest)[1:2]<-c("area","Larea")


############################EDA###################################

#pairs plot
plot(forest[,2:9])

#correlation matrix
cor(forest[,2:9])

#factor variables
par(mfrow=c(2,3))
for(i in 10:14){
  plot(forest[,i],forest$Larea, main = colnames(forest)[i],
       names=levels(forest[,i]), ylab = "log(area)", col = "lightblue")
}

#ANOVA tests for significance of factor variables
summary(lm(Larea~month, data = forest))
summary(lm(Larea~day, data = forest))
summary(lm(Larea~rain, data = forest))

#Plot distributions of continuous variables by large or small.
#get indexes of zero and nonzero fires
zero<-which(forest$area>0)
par(mfrow=c(2,3))
for (i in 3:9){
  column<-forest[,i]
  hist(column[zero], col = rgb(1,0,0,0.5), probability = TRUE, 
       main=colnames(forest)[i], xlab = colnames(forest)[i], breaks = 20)
  lines(density(column[zero], adjust = 2), col = rgb(1,0,0,0.5))
  
  hist(column[-zero], col = rgb(0,0,1,0.5), probability = TRUE, 
       add = TRUE, breaks = 20)
  lines(density(column[-zero], adjust = 2), col = rgb(0,0,1,0.5))
  legend("topright", cex = 0.75, legend = c("Zero Area", "Nonzero Area"), 
         lty = c(1,1), col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
}

#Two-sample t-tests to detect differences in means
for (i in 3:9){
  column<-forest[,i]
  test<-t.test(column[zero],column[-zero],alternative = "two.sided")
  print(paste("The p-value for ", colnames(forest)[i], " was", test$p.value))
}

#DC is highly non-normal, so use Wilcoxan rank-sum
wilcox.test(forest$DC[zero],forest$DC[-zero], alternative = "two.sided")
#p-value = 0.07461

#Scatterplots colored by zero vs. nonzero
par(mfrow=c(2,2))
for (i in 3:9){
  plot(forest[,i],log(forest$area+1),col=forest$nonzero,
       xlab = paste(colnames(forest)[i]),
       main = paste(colnames(forest)[i], "vs. log(area)"))
}

#Attempt simple linear regressions on each of the continuous predictors to see
#if anything useful emerges.
par(mfrow=c(2,4))
for (i in 3:9){
  X<-forest[,i]
  plot(forest[,i],forest$Larea, xlab = paste(colnames(forest)[i]),
       main = paste(colnames(forest)[i], "vs. log(area)"))
  abline(lm(forest$Larea~X), col="blue", lwd=2)
}

#Generate Interaction Plots for continuous variables
par(mfrow=c(2,3))
for (i in 3:8){
  for (j in (i+1):9){
    y<-forest$Larea
    Xr<-as.matrix(cbind(forest[,c(i,j)],forest[,i]*forest[,j]))
    label1<-colnames(forest)[i]
    label2<-colnames(forest)[j]
    temp.model<-lm(y~Xr, data = forest)
    
    # Interaction
    b0=temp.model$coefficients[1]
    b1=temp.model$coefficients[2]
    b2=temp.model$coefficients[3]
    b3=temp.model$coefficients[4]
    n=387
    x1=Xr[,1]
    x2=Xr[,2]
    X=as.matrix(expand.grid(x1,x2))
    yt=b0+b1*X[,1]+b2+X[,2]+b3*X[,1]*X[,2]
    yp=b0+b1*X[,1]+b2+X[,2]
    
    # Let's look at the interaction in a lower-dimensional plot by conditioning
    # on values of X1 and varying it.  
    x1=c(mean(x1)-sd(x1),mean(x1),mean(x1)+sd(x1))
    k=1
    yt=(b0+b2*X[,2])+(b1+b3*X[,2])*x1[k]
    plot(X[,2],yt,ylab="Predicted(Larea)", xlab = label2, ylim=c(0,2),
         main=paste("Interaction Plot for", label1, " and", 
                    label2),col=k,lwd=2,type='l')
    for(k in 2:3)
    {
      yt=(b0+b2*X[,2])+(b1+b3*X[,2])*x1[k]
      lines(X[,2],yt,col=k)
    }
    legend("topleft",col=c(1,2,3),legend=c(paste("-1SD ", label1),
                                           paste("Mean ", label1),
                                           paste("+1SD ", label1)),cex=1,lty=1)
    Sys.sleep(1)
  }
}


############################MLR Analysis#################################
#predictors that came out of EDA: TrFFMC, DMC, wind, temp, month
#we first try MLR on these

MLR.eda.model<-lm(Larea~TrFFMC+DMC+wind+temp+month, data = forest)
summary(MLR.eda.model)

#enumerate full model space
require(leaps)
y<-forest$Larea
X<-cbind(forest$TrFFMC, forest$DMC, forest$DC, forest$ISI, 
         forest$temp, forest$RH, forest$wind, forest$month, forest$day)
regsubsets.out=regsubsets(y ~ X,
                          data = as.data.frame(cbind(y,X)),
                          nbest = 2,       # 2 best models for each number of predictors
                          nvmax = NULL,    # NULL for no limit on number of variables
                          force.in = NULL, force.out = NULL,
                          method = "exhaustive") 
regsubsets.out
sum.reg<-summary(regsubsets.out)
sum.reg

#plot regsubsets out, suggests we should drop TrFFMC, DC, and temp
par(mfrow=c(1,3))
plot(regsubsets.out, scale = "r2", main = "R^2",cex.axis=2,cex.lab=2,cex=2)
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2",cex.axis=2,cex.lab=2,cex=2)
plot(regsubsets.out, scale = "bic", main = "BIC",cex.axis=2,cex.lab=2,cex=2)

reg.subsets.model<-lm(Larea~DMC+ISI+RH+wind+month, data=forest)
summary(reg.subsets.model)

#use forward selection, backward selection, and stepwise regression
#to select a model #exclude spatial variables
fit0<-lm(Larea~1,dat=forest)
fitfull<-lm(Larea~.,dat=forest[,-c(1,9,11,15,16,17)])

# Forward selection/Stepwise regression/Backwards Elim yield the same
# Note per documentation, default criterion is BIC
step(fit0,scope=list(lower=fit0,upper=fitfull),direction="forward") 

fwd.model<-lm(Larea ~ DMC + month + temp + DC, data = forest)
summary(fwd.model)

BIC(MLR.eda.model, reg.subsets.model, fwd.model)

#View diagnostics on the forward selection model
par(mfrow=c(1,2))
#Residual plot
resids<-resid(fwd.model)
fit.val<-fitted(fwd.model)
plot(fit.val,resids, main = "Fitted Values vs. Residuals for 
     Forward Selection Model")
abline(h=0)
#Q-Q Plot
qqnorm(resids)
qqline(resids, col = "blue")

#Condition this model on nonzero burn area
nz<-which(forest$area>0)
fwd.nz.model<-lm(Larea ~ DMC + month + temp + DC, data = forest[nz,])
summary(fwd.nz.model)

#View diagnostics for this nonzero conditioned model
par(mfrow=c(1,2))
#Residual plot
resids<-resid(fwd.nz.model)
fit.val<-fitted(fwd.nz.model)
plot(fit.val,resids, main = "Fitted Values vs. Residuals for 
     Forward Selection Model")
abline(h=0)
#Q-Q Plot
qqnorm(resids)
qqline(resids, col = "blue")

#Other diagnostics
#Durbin Watson test



############################MLR Analysis with Interactions#################################
#predictors that came out of EDA: TrFFMC, DMC, wind, temp, month
#we first try MLR on these, including interaction terms suggested by the interaction plots

MLR.eda.int.model<-lm(Larea~TrFFMC+DMC+wind+temp+month+TrFFMC:DMC+TrFFMC:temp+TrFFMC:wind+
                    DMC:temp+DMC:wind+temp:wind, data = forest)
summary(MLR.eda.int.model)

#enumerate full model space
require(leaps)
attach(forest)
y<-Larea
X<-cbind(TrFFMC, DMC, DC, ISI, temp, RH, wind, month, day, TrFFMC*DMC, TrFFMC*temp, TrFFMC*wind,
         DMC*temp,DMC*RH, DMC*wind, DC*temp, DC*RH, DC*wind, ISI*temp, ISI*wind, temp*wind, RH*wind)
regsubsets.out=regsubsets(y ~ X,
                          data = as.data.frame(cbind(y,X)),
                          nbest = 2,       # 2 best models for each number of predictors
                          nvmax = NULL,    # NULL for no limit on number of variables
                          force.in = NULL, force.out = NULL,
                          method = "exhaustive") 
regsubsets.out
sum.reg<-summary(regsubsets.out)
sum.reg

#plot regsubsets out, suggests we should drop TrFFMC, DC, and temp
par(mfrow=c(1,3))
plot(regsubsets.out, scale = "bic", main = "BIC",cex.axis=1,cex.lab=2,cex=2)

#Create custom algorithm to condition on hierarchy
init.model<-lm(Larea~1,data=forest[,-c(1,10)])
for(i in 3:)

#use forward selection, backward selection, and stepwise regression
#to select a model #exclude spatial variables
fit0<-lm(Larea~1,dat=forest)
fitfull<-lm(Larea~.,dat=forest[,-c(1,9,11,15,16,17)])

# Forward selection/Stepwise regression/Backwards Elim yield the same
# Note per documentation, default criterion is BIC
step(fit0,scope=list(lower=fit0,upper=fitfull),direction="forward") 

fwd.model<-lm(Larea ~ DMC + month + temp + DC, data = forest)
summary(fwd.model)

BIC(MLR.eda.model, reg.subsets.model, fwd.model)

#View diagnostics on the forward selection model
par(mfrow=c(1,2))
#Residual plot
resids<-resid(fwd.model)
fit.val<-fitted(fwd.model)
plot(fit.val,resids, main = "Fitted Values vs. Residuals for 
     Forward Selection Model")
abline(h=0)
#Q-Q Plot
qqnorm(resids)
qqline(resids, col = "blue")

#Condition this model on nonzero burn area
nz<-which(forest$area>0)
fwd.nz.model<-lm(Larea ~ DMC + month + temp + DC, data = forest[nz,])
summary(fwd.nz.model)

#View diagnostics for this nonzero conditioned model
par(mfrow=c(1,2))
#Residual plot
resids<-resid(fwd.nz.model)
fit.val<-fitted(fwd.nz.model)
plot(fit.val,resids, main = "Fitted Values vs. Residuals for 
     Forward Selection Model")
abline(h=0)
#Q-Q Plot
qqnorm(resids)
qqline(resids, col = "blue")

#Other diagnostics
#Durbin Watson test





#forward selection on interaction terms yielded
fwd.intx.model<-lm(formula = Larea ~ ISI:temp:RH + DMC:ISI + TrFFMC:DMC:ISI + 
                     day, data = forest[nz,])
summary(fwd.intx.model)
BIC(fwd.intx.model) # this actually does beat the existing model

resids<-resid(fwd.intx.model)
fit.val<-fitted(fwd.intx.model)
plot(fit.val,resids, main = "Fitted Values vs. Residuals for 
     Forward Selection Model")
abline(h=0)
#Q-Q Plot
qqnorm(resids)
qqline(resids, col = "blue")

#setup the same model hierarchically, it's basically the same with worse
#BIC, but may perform better on the testing set
fwd.intx.h.model<-lm(formula = Larea ~ ISI+ temp + RH+ DC + TrFFMC+ 
                       ISI:temp + temp:RH + ISI:RH+ TrFFMC:ISI + DMC:ISI +
                       TrFFMC:ISI + ISI:temp:RH + DMC:ISI + TrFFMC:DMC:ISI + 
                       day, data = forest[nz,])
summary(fwd.intx.h.model)
BIC(fwd.intx.h.model) # this actually does beat the existing model

resids<-resid(fwd.intx.h.model)
fit.val<-fitted(fwd.intx.h.model)
plot(fit.val,resids, main = "Fitted Values vs. Residuals for 
     Forward Selection Model")
abline(h=0)
#Q-Q Plot
qqnorm(resids)
qqline(resids, col = "blue")


############################PENALIZED REGRESSION##############################


