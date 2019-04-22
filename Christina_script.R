#################FOREST FIRES ANALYSIS########################
##################C. Sousa####################################

#Read in data
forest <-read.csv("forestfires.csv")
#split off testing set
set.seed(6950)
indices<-sample(1:517,388)
forest<-forest[indices,]
testdat<-forest[-indices,]

#split fires into burn area categories as defined by Ozbayoglu, Bozer
#1.32, 63.86, 273.05, 773.17
#5 categories
forest$size <- cut(forest$area, c(-.01,1.32,63.86,273.05,773.17,2000))

#large and small categories
forest$large <- cut(forest$area, c(-.01,63.86,2000))

#zero and nonzero categories
forest$nonzero <- cut(forest$area, c(-.01,0,2000))
levels(forest$nonzero) <- c("zero","nonzero")

###################################DATA CLEANING##############################
#log transform the area variable
forest$Larea<-log(forest$area + 1)

#put area and Larea variables at start of data set
forest<-cbind(forest$area,forest$Larea,forest[,5:11],forest[,c(3,4,12,14:16)])
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
forest$day<-factor(forest$day,levels=levels(forest$day)[c(2,6,7,5,1,3,4)])
forest$month<-factor(forest$month, levels=levels(forest$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])

############################EDA###################################

#pairs plot
plot(forest[,2:9])

#correlation matrix
cor(forest[,2:9])

#check VIF
cts.pred<-forest[,3:10]
q<-ncol(cts.pred)
r2k<-numeric(q)
for (i in 1:q){
  formula<-as.formula(paste(colnames(cts.pred)[i],"~."))
  model<-lm(formula,data = cts.pred)
  r2k[i]<-summary(model)$r.squared
}
vifk<-1/(1-r2k)
vifk

#factor variables
par(mfrow=c(1,3))
plot(forest[,10],forest$Larea, main = colnames(forest)[10],
     names=levels(forest[,10]), ylab = "log(area)", col = "lightblue")
plot(forest[,11],forest$Larea, main = colnames(forest)[11],
     names=levels(forest[,11]), ylab = "log(area)", col = "lightblue")
plot(forest[,12],forest$Larea, main = colnames(forest)[12],
     names=levels(forest[,12]), ylab = "log(area)", col = "lightblue")

#ANOVA tests for significance of factor variables
summary(lm(Larea~month, data = forest))
summary(lm(Larea~day, data = forest))
summary(lm(Larea~rain, data = forest))

#Plot distributions of continuous variables by large or small.
#get indexes of zero and nonzero fires
nz<-which(forest$area>0)
par(mfrow=c(1,4))
for (i in 3:9){
  column<-forest[,i]
  hist(column[nz], col = rgb(1,0,0,0.5), probability = TRUE, 
       main=colnames(forest)[i], xlab = colnames(forest)[i], breaks = 20)
  lines(density(column[nz], adjust = 2), col = rgb(1,0,0,0.5))
  
  hist(column[-nz], col = rgb(0,0,1,0.5), probability = TRUE, 
       add = TRUE, breaks = 20)
  lines(density(column[-nz], adjust = 2), col = rgb(0,0,1,0.5))
  legend("topright", cex = 0.75, legend = c("Nonzero Area", "Zero Area"), 
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

#create separate function to get only desired plots
intx.plots<-function(i,j){
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
       main=paste(label1, " and", label2),col=k,lwd=2,type='l')
  for(k in 2:3)
  {
    yt=(b0+b2*X[,2])+(b1+b3*X[,2])*x1[k]
    lines(X[,2],yt,col=k)
  }
  legend("topleft",col=c(1,2,3),legend=c(paste("-1SD ", label1),
                                         paste("Mean ", label1),
                                         paste("+1SD ", label1)),cex=1,lty=1)
}

par(mfrow=c(1,4))
intx.plots(3,4)
intx.plots(3,7)
intx.plots(3,9)
intx.plots(4,7)
intx.plots(4,8)
intx.plots(4,9)
intx.plots(5,7)
intx.plots(5,8)
intx.plots(5,9)
intx.plots(6,7)
intx.plots(6,9)
intx.plots(7,9)
intx.plots(8,9)

############################MLR Analysis with Interactions#################################
#predictors that came out of EDA: TrFFMC, DMC, wind, temp
#we do not consider month and day because they are not meteorological variables
#we first try MLR on these, including interaction terms suggested by the interaction plots

MLR.eda.int.model<-lm(Larea~TrFFMC+DMC+wind+temp+TrFFMC:DMC+TrFFMC:temp+TrFFMC:wind+
                        DMC:temp+DMC:wind+temp:wind, data = forest)
summary(MLR.eda.int.model)

#enumerate full model space
require(leaps)
attach(forest)
y<-Larea
X<-cbind(TrFFMC, DMC, DC, ISI, temp, RH, wind, TrFFMC*DMC, TrFFMC*temp, 
         TrFFMC*wind,DMC*temp,DMC*RH, DMC*wind, DC*temp, DC*RH, DC*wind, ISI*temp, 
         ISI*wind, temp*wind, RH*wind)
regsubsets.out=regsubsets(y ~ X,
                          data = as.data.frame(cbind(y,X)),
                          nbest = 2,       # 2 best models for each number of predictors
                          nvmax = NULL,    # NULL for no limit on number of variables
                          force.in = NULL, force.out = NULL,
                          method = "exhaustive") 

#plot regsubsets out, suggests we should drop DC, temp, 1st, 4th, 7,8,9th interactions
par(mfrow=c(1,1))
plot(regsubsets.out, scale = "bic", main = "BIC",cex.axis=1,cex.lab=2,cex=2)

leaps.int.model<-lm(Larea~TrFFMC+DMC+ISI+RH+wind+TrFFMC*temp+TrFFMC*wind+DMC*RH+
                      DMC*wind+ISI*temp+ISI*wind+temp*wind+RH*wind)
summary(leaps.int.model)

#Use add1() and drop1() to condition on hierarchically appropriate models
init.model<-lm(Larea~1,data=forest)
add1(init.model,Larea~TrFFMC+DC+ISI+temp+RH+wind+rain,test="F") #nothing significant
  
add1.model<-lm(Larea~1, data=forest)
summary(add1.model)

fitfull<-lm(Larea~TrFFMC+DMC+DC+ISI+temp+RH+wind+TrFFMC*DMC+TrFFMC*temp+
              TrFFMC*wind+DMC*temp+DMC*RH+DMC*wind+DC*temp+DC*RH+DC*wind+ISI*temp+ 
              ISI*wind+temp*wind+RH*wind)

drop1(fitfull, test="F") #drop temp:wind
model<-lm(Larea~TrFFMC+DMC+DC+ISI+temp+RH+wind+TrFFMC*DMC+TrFFMC*temp+
            TrFFMC*wind+DMC*temp+DMC*RH+DMC*wind+DC*temp+DC*RH+DC*wind+ISI*temp+ 
            ISI*wind+RH*wind)
drop1(model, test="F") #drop nothing

drop1.model<-model
summary(drop1.model)

#use forward selection, backward selection, and stepwise regression
#to select a model #exclude spatial variables
fit0<-lm(Larea~1,dat=forest)

# Forward selection/Stepwise regression/Backwards Elim yield the same
# Note per documentation, default criterion is BIC
step(fit0,scope=list(lower=fit0,upper=fitfull),direction="forward") 
fwd.int.model<-lm(formula = Larea ~ DMC + wind, data = forest) #same as stepwise
summary(fwd.int.model)

step(fit0,scope=list(lower=fit0,upper=fitfull),direction="backward")
mean.model<-lm(formula = Larea ~ 1, data = forest)
summary(mean.model)

BIC(MLR.eda.int.model,leaps.int.model,add1.model, drop1.model, fwd.int.model,mean.model)

#View diagnostics on the forward selection model
par(mfrow=c(1,2))
#Residual plot
resids<-resid(fwd.int.model)
fit.val<-fitted(fwd.int.model)
plot(fit.val,resids, main = "Fitted Values vs. Residuals for 
     Forward Selection Model")
abline(h=0)
#Q-Q Plot
qqnorm(resids)
qqline(resids, col = "blue")

#Condition this model on nonzero burn area
nz<-which(forest$area>0)
fwd.nz.int.model<-lm(Larea ~ DMC + wind, data = forest[nz,])
summary(fwd.nz.int.model)

#View diagnostics for this nonzero conditioned model
par(mfrow=c(1,2))
#Residual plot
resids<-resid(fwd.nz.int.model)
fit.val<-fitted(fwd.nz.int.model)
plot(fit.val,resids, main = "Fitted Values vs. Residuals for 
     Forward Selection Model")
abline(h=0)
#Q-Q Plot
qqnorm(resids)
qqline(resids, col = "blue")

#Other diagnostics
#Durbin Watson test
require(car)
durbinWatsonTest(fwd.nz.int.model) #fail to reject, so no autocorrelatin
#Cook's Distance
cd<-cooks.distance(fwd.nz.int.model)
which(cd>qf(0.5,ncol(forest[nz,])+1,nrow(forest[nz,])-ncol(forest[nz,])-1)) #these also look fine
#K-S test for normality of errors (basically rejects!)
ks.test(resids,"pnorm", mean = mean(resids), sd=sd(resids),alternative = "two.sided")
#Shapiro-Wilks test for normality
shapiro.test(resids)

#View diagnostics on the Leaps model
par(mfrow=c(1,2))
#Residual plot
resids<-resid(leaps.int.model)
fit.val<-fitted(leaps.int.model)
plot(fit.val,resids, main = "Fitted Values vs. Residuals for 
     Leaps Model")
abline(h=0)
#Q-Q Plot
qqnorm(resids)
qqline(resids, col = "blue")

#Condition this model on nonzero burn area
nz<-which(forest$area>0)
leaps.nz.int.model<-lm(Larea~TrFFMC+DMC+ISI+RH+wind+TrFFMC*temp+TrFFMC*wind+DMC*RH+
                         DMC*wind+ISI*temp+ISI*wind+temp*wind+RH*wind, data = forest[nz,])
summary(leaps.nz.int.model)

#View diagnostics for this nonzero conditioned model
par(mfrow=c(1,2))
#Residual plot
resids<-resid(leaps.nz.int.model)
fit.val<-fitted(leaps.nz.int.model)
plot(fit.val,resids, main = "Fitted Values vs. Residuals for 
     Leaps Model")
abline(h=0)
#Q-Q Plot
qqnorm(resids)
qqline(resids, col = "blue")

#Other diagnostics
#Durbin Watson test
require(car)
durbinWatsonTest(leaps.nz.int.model) #fail to reject, so no autocorrelatin
#Cook's Distance
cd<-cooks.distance(leaps.nz.int.model)
which(cd>qf(0.5,ncol(forest[nz,])+1,nrow(forest[nz,])-ncol(forest[nz,])-1)) #these also look fine
#K-S test for normality of errors (basically rejects!)
ks.test(resids,"pnorm", mean = mean(resids), sd=sd(resids),alternative = "two.sided")


############################TESTING MLR MODELS##############################
detach(forest)
#repeat data cleaning on testdat set
#split fires into burn area categories as defined by Ozbayoglu, Bozer
#1.32, 63.86, 273.05, 773.17
#5 categories
testdat$size <- cut(testdat$area, c(-.01,1.32,63.86,273.05,773.17,2000))

#large and small categories
testdat$large <- cut(testdat$area, c(-.01,63.86,2000))

#zero and nonzero categories
testdat$nonzero <- cut(testdat$area, c(-.01,0,2000))
levels(testdat$nonzero) <- c("zero","nonzero")

#log transform the area variable
testdat$Larea<-log(testdat$area + 1)

#put area and Larea variables at start of data set
testdat<-cbind(testdat$area,testdat$Larea,testdat[,5:11],
               testdat[,c(3,4,12,14:16)])
#change rain to binomial variable
testdat$rain<-ifelse(testdat$rain>0,1,0)
testdat$rain<-as.factor(testdat$rain)
levels(testdat$rain) <- c("no rain","rain")
#transform FFMC
testdat$FFMC <- log(-testdat$FFMC+ max(testdat$FFMC)+1)
colnames(testdat)[3]<-"TrFFMC"
#remove outlier in ISI
testdat<-testdat[which(testdat$ISI<40),]
colnames(testdat)[1:2]<-c("area","Larea")

#get predictions for all models
model.list<-list(MLR.eda.int.model,leaps.int.model,drop1.model, 
                 fwd.int.model, mean.model)

in.sample.MSEs<-numeric(5)
pred.MSEs<-numeric(5)

for(i in 1:5){
  yhat<-predict(model.list[[i]],X=forest)
  in.sample.MSEs[i]<-mean((forest$Larea-yhat)^2)
  yhat2<-predict(model.list[[i]],newdata=testdat)
  pred.MSEs[i]<-mean((testdat$Larea-yhat2)^2)
}
test.results<-cbind(in.sample.MSEs,pred.MSEs)
test.results

############################LASSO#######################################
#try LASSO

two.way.interaction<-matrix(nrow=nrow(forest),ncol=21)
labels2<-vector()
#create interaction terms
k=1
for(i in 3:8){
  for (j in (i+1):9){
    two.way.interaction[,k]<-forest[,i]*forest[,j]
    labels2[k]<-paste(colnames(forest)[i],colnames(forest)[j], sep = "*")
    k<-(k+1)
  }
}
colnames(two.way.interaction)<-labels2

lasso.dat<-cbind(forest[,2:9],two.way.interaction)

require(glmnet)
X<-model.matrix(Larea~.-1,data=lasso.dat)
y<-lasso.dat$Larea

cvfit<-cv.glmnet(X, y, nfolds=5)
plot(cvfit)
coef(cvfit, s = "lambda.min")

fit.lasso<-glmnet(X,y,alpha=1)
plot(fit.lasso,xvar="lambda",label=TRUE)

#get in-sample MSE
yhat<-predict(cvfit,X,s="lambda.min")
mean((y-yhat)^2)

#get test MSE

two.way.interaction<-matrix(nrow=nrow(testdat),ncol=21)
labels2<-vector()
#create interaction terms
k=1
for(i in 3:8){
  for (j in (i+1):9){
    two.way.interaction[,k]<-testdat[,i]*testdat[,j]
    labels2[k]<-paste(colnames(testdat)[i],colnames(testdat)[j], sep = "*")
    k<-(k+1)
  }
}
colnames(two.way.interaction)<-labels2

lasso.testdat<-cbind(testdat[,2:9],two.way.interaction)

Xtest<-model.matrix(Larea~.-1,data=lasso.testdat)
ytest<-lasso.testdat$Larea

yhat2<-predict(cvfit,Xtest,s="lambda.min")
mean((ytest-yhat2)^2)
