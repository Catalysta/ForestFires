
temp.model<-lm(Larea~DMC+ISI+DMC:ISI, data = forest)

# Interaction
b0=temp.model$coefficients[1]
b1=temp.model$coefficients[2]
b2=temp.model$coefficients[3]
b3=temp.model$coefficients[4]
n=387
x1=forest$DMC
x2=forest$ISI
X=as.matrix(expand.grid(x1,x2))
yt=b0+b1*X[,1]+b2+X[,2]+b3*X[,1]*X[,2]
yp=b0+b1*X[,1]+b2+X[,2]
y=forest$Larea

# Let's look at the interaction in a lower-dimensional plot by conditioning
# on values of X1 and varying it.  
x1=c(mean(x1)-sd(x1),mean(x1),mean(x1)+sd(x1))
i=1
yt=(b0+b2*X[,2])+(b1+b3*X[,2])*x1[i]
plot(X[,2],yt,ylab="Predicted(Larea)", xlab = "ISI", ylim=c(0,2),
     main="Interaction Plot for DMC and ISI",col=i,lwd=2,type='l')
for(i in 2:3)
{
  yt=(b0+b2*X[,2])+(b1+b3*X[,2])*x1[i]
  lines(X[,2],yt,col=i)
}
legend("topleft",col=c(1,2,3),legend=c("-1SD DMC","Mean DMC","+1SD DMC"),lty=1)
