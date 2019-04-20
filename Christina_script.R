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
par(mfrow=c(3,2))
for(i in 10:14){
  plot(forest[,i],forest$Larea, main = colnames(forest)[i],
       names=levels(forest[,i]), ylab = "log(area)", col = "lightblue")
}


#Plot distributions of continuous variables by large or small.
#get indexes of zero and nonzero fires
zero<-which(forest$area>0)
par(mfrow=c(2,2))
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

#Scatterplots colored by zero vs. nonzero
par(mfrow=c(2,2))
for (i in 3:9){
  plot(forest[,i],log(forest$area+1),col=forest$nonzero,
       xlab = paste(colnames(forest)[i]),
       main = paste(colnames(forest)[i], "vs. log(area)"))
}

#Attempt simple linear regressions on each of the continuous predictors to see
#if anything useful emerges.
par(mfrow=c(2,2))
for (i in 3:9){
  X<-forest[,i]
  plot(forest[,i],forest$Larea, xlab = paste(colnames(forest)[i]),
       main = paste(colnames(forest)[i], "vs. log(area)"))
  abline(lm(forest$Larea~X), col="blue", lwd=2)
}
