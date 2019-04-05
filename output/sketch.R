forest2<-forest[which(!forest$area==0),]
forest3<-cbind(forest2[,-13],log(forest2$area))

which(forest3$rain>5)
forest3[262,]

cooks.distance(lm(log(forest2$area)~.,dat=forest3))[262]
qf(0.5,ncol(forest3)+1,nrow(forest3)-ncol(forest3)-1)

data.tree <- tree(L.area~., data=forest3)
plot(data.tree, type="uniform")
text(data.tree)

model1<-lm(L.area~.,data=forest3)
summary(model1)
