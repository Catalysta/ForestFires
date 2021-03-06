
library(rpart)

forestx <- forest9
forestx$FFMC <- log(max(forestx$FFMC)-forestx$FFMC)
colnames(forestx)[6] <- "TrFFMC"
set.seed(1110)
fit = rpart(large ~ rainbin+
              wind+TrFFMC+temp+RH+DC+ISI+DMC,
            dat=forestx,method="class",
            control = rpart.control(cp = 0.03))

#fit
plot(fit,margin = 0.1)
text(fit,use.n=TRUE,cex=0.7)  # here cex will size the text labelling of nodes/leafs.

printcp(fit)

plotcp(fit)


fit.pruned = prune(fit,cp=.03) 
fit.pruned
#summary(fit.pruned)
plot(fit.pruned,margin = 0.1)
text(fit.pruned,use.n=TRUE,cex=1.2)


min = matrix(rep(0,20),nrow = 10, ncol = 2)
dev.min = matrix(rep(1000,20),nrow = 10, ncol = 2)
devs <- rep(0,1023)


for (i in 1:1023) {
    train.x <- train4[,c(as.integer(intToBits(i))[1:10]==1,T)]
    
    mylogit<-glm(formula = large ~.,
                 family = "binomial", data = train.x)
    summary(mylogit)
    
    j = sum(as.integer(intToBits(i))[1:10])
    
    
    if(dev.min[j,2] > mylogit$deviance) {
        min[j,2] = i
        dev.min[j,2] = mylogit$deviance
    }
    
    
    if(dev.min[j,1] > dev.min[j,2]) {
      min[j,] = min[j,c(2,1)]
      dev.min[j,] = dev.min[j,c(2,1)]
    }
    
}


dev.min <- dev.min[,c(2,1)]

g <- rbind(1:10,1:10)
x.vec <- as.vector(g)
x.vec[2*(1:10)] <- x.vec[2*(1:10)]+0.1
x.vec[2*(1:10)-1] <- x.vec[2*(1:10)-1]-0.1
x.vec

devs <- as.vector(t(dev.min))
x.vec <- x.vec[c(-19)]
devs <- devs[c(-19)]

x.vec[1] <- 0
devs[1] <- mylogit$null.deviance
plot(x.vec,devs,type = "l")
points(x.vec,devs,pch=18)

i = min[6,2]


train.x <- train4[,c(as.integer(intToBits(i))[1:10]==1,T)]

mylogit<-glm(formula = large ~.,
             family = "binomial", data = train.x)
summary(mylogit)












train.4 <- train4[,c(-1,-2)]


min = matrix(rep(0,16),nrow = 8, ncol = 2)
dev.min = matrix(rep(1000,16),nrow = 8, ncol = 2)
devs <- rep(0,255)


for (i in 1:255) {
  train.x <- train.4[,c(as.integer(intToBits(i))[1:8]==1,T)]
  
  mylogit<-glm(formula = large ~.,
               family = "binomial", data = train.x)
  summary(mylogit)
  
  j = sum(as.integer(intToBits(i))[1:8])
  
  
  if(dev.min[j,2] > mylogit$deviance) {
    min[j,2] = i
    dev.min[j,2] = mylogit$deviance
  }
  
  
  if(dev.min[j,1] > dev.min[j,2]) {
    min[j,] = min[j,c(2,1)]
    dev.min[j,] = dev.min[j,c(2,1)]
  }
  
}


dev.min <- dev.min[,c(2,1)]

g <- rbind(1:8,1:8)
x.vec <- as.vector(g)
x.vec[2*(1:8)] <- x.vec[2*(1:8)]+0.1
x.vec[2*(1:8)-1] <- x.vec[2*(1:8)-1]-0.1
x.vec

devs <- as.vector(t(dev.min))
x.vec <- x.vec[c(-15)]
devs <- devs[c(-15)]

x.vec[1] <- 0
devs[1] <- mylogit$null.deviance

plot(x.vec,devs,type = "l")
points(x.vec,devs,pch=18)

i = min[4,2]


train.x <- train.4[,c(as.integer(intToBits(i))[1:8]==1,T)]

mylogit<-glm(formula = large ~1,
             family = "binomial", data = train.x)
summary(mylogit)





library(rpart)

forestx <- forest9
forestx$FFMC <- log(max(forestx$FFMC)-forestx$FFMC)
colnames(forestx)[6] <- "TrFFMC"

fit = rpart(large ~ week+season+rain+
              wind+TrFFMC+temp+RH+DC+ISI+DMC,
            dat=forestx,method="class",
            control = rpart.control(cp = 0.03))

fit
plot(fit,margin = 0.1)
text(fit,use.n=TRUE,cex=0.7)  # here cex will size the text labelling of nodes/leafs.

printcp(fit)

plotcp(fit)


fit.pruned = prune(fit,cp=.03) 
fit.pruned
summary(fit.pruned)
plot(fit.pruned,margin = 0.1)
text(fit.pruned,use.n=TRUE,cex=1.2)


min = matrix(rep(0,20),nrow = 10, ncol = 2)
dev.min = matrix(rep(1000,20),nrow = 10, ncol = 2)
devs <- rep(0,1023)


for (i in 1:1023) {
    train.x <- train4[,c(as.integer(intToBits(i))[1:10]==1,T)]
    
    mylogit<-glm(formula = large ~.,
                 family = "binomial", data = train.x)
    summary(mylogit)
    
    j = sum(as.integer(intToBits(i))[1:10])
    
    
    if(dev.min[j,2] > mylogit$deviance) {
        min[j,2] = i
        dev.min[j,2] = mylogit$deviance
    }
    
    
    if(dev.min[j,1] > dev.min[j,2]) {
      min[j,] = min[j,c(2,1)]
      dev.min[j,] = dev.min[j,c(2,1)]
    }
    
}

