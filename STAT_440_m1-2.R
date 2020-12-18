
train_1<-read.csv("/Users/jacobzhu/Desktop/R code/module1/train-1.csv")
test_1<-read.csv("/Users/jacobzhu/Desktop/R code/module1/test-1.csv")
sample_data<-read.csv("/Users/jacobzhu/Desktop/R code/module1/sample-1.csv")
train_plot=train_1[is.nan(train_1$SnowDepth) & is.nan(train_1$TempAir) & is.nan(train_1$TempSurf) ,]
plot(density(train_plot$Temp100cm),main="Density Graph of Temp100cm",xlab = "Temp100cm",col="blue")
lines(density(train_1$Temp100cm),col="red")
legend("top",legend=c("One or more missing","No missing"),col=c("blue","red"),lwd =1)
library(psych)
pairs.panels(train_1, 
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE,  
             ellipses = TRUE 
)





test_1=test_1[,2:6]
test_1$Temp100cm=NaN
train_2=train_1
train_1=rbind(train_1,test_1)
train_1$CMTemp100cm=NaN
train_1$CMTempAir=NaN
for (i in (1:12)){
  pa=train_2[train_2$Month==i,"Temp100cm"]
  train_1$CMTemp100cm[train_1$Month==i]=mean(pa[!is.nan(pa)])  
}

for (i in (1:12)){
  pa=train_2[train_2$Month==i,"TempAir"]
  train_1$CMTempAir[train_1$Month==i] = mean(pa[!is.nan(pa)])  
}

for (i in (1:12)){
  pa=train_2[train_2$Month==i,"TempAir"]
  train_1$CMTempAir[train_1$Month==i] = mean(pa[!is.nan(pa)])  
}
for (i in (1:12)){
  pa=train_2[train_2$Month==i,"SnowDepth"]
  train_1$CMSnowDepth[train_1$Month==i]=mean(pa[!is.nan(pa)])   
}




test_1=train_1[2749:nrow(train_1),]
train_1=train_1[1:2748,]





list=sample.int(nrow(train_1),as.integer(nrow(train_1)*0.2))
train=train_1[-list,]
test=train_1[list,]


train_null=train
test_null=test

#*****
train_model=train
test_model=test
train_model=train[!is.nan(train$SnowDepth) & !is.nan(train$TempAir) & !is.nan(train$TempSurf) ,]
test_model=test[!is.nan(test$SnowDepth) & !is.nan(test$TempAir) & !is.nan(test$TempSurf) ,]
data2=train_model
data2$Year=as.factor(data2$Year)
data2$Month=as.factor(data2$Month)
m1=lm(SnowDepth~Year+Month+
        TempAir+TempSurf+
        Month:(TempAir+TempSurf)+
        Year:(TempAir+TempSurf)
      ,data2)
bma2=step(m1,scope=list(lower=~1, 
                        upper=~Year+Month+
                          TempAir+TempSurf+
                          Month:(TempAir+TempSurf)+
                          Year:(TempAir+TempSurf)
                        ,direction="both",data2))

train$Year=as.factor(train$Year)
train$Month=as.factor(train$Month)
test$Year=as.factor(test$Year)
test$Month=as.factor(test$Month)
train[is.nan(train$SnowDepth)  & !is.nan(train$TempAir) & !is.nan(train$TempSurf),"SnowDepth"]=predict.lm(bma2,train[is.nan(train$SnowDepth) & !is.nan(train$TempAir) & !is.nan(train$TempSurf),c(1,2,4,5,7)])
test[is.nan(test$SnowDepth)  & !is.nan(test$TempAir) & !is.nan(test$TempSurf),"SnowDepth"]=predict.lm(bma2,test[is.nan(test$SnowDepth) & !is.nan(test$TempAir) & !is.nan(test$TempSurf),c(1,2,4,5,7)])
train[!is.nan(train$SnowDepth) & train$SnowDepth<0,"SnowDepth"]=0;
test[!is.nan(test$SnowDepth) & test$SnowDepth<0,"SnowDepth"]=0;
Y_hat=test[,"Temp100cm"]
test$Temp100cm=NaN



total=rbind(train,test)
total[1:nrow(train),]=knnImputation(total[1:nrow(train),])
library(DMwR)
total2=rbind(train_null,test_null)
total$ct1=as.factor(as.integer(is.nan(total2$SnowDepth)))
total$ct2=as.factor(as.integer(is.nan(total2$TempAir)))
total$ct3=as.factor(as.integer(is.nan(total2$TempSurf)))
total[,c(1:5,7:12)]=knnImputation(total[,c(1:5,7:12)])
##############
total=knnImputation(total)
#solution1=total$Temp100cm[2749:nrow(total)]
solution1=total$Temp100cm[(nrow(train)+1):2748]


train=total[1:nrow(train),]
test=total[(nrow(train)+1):nrow(total),]
test$Year=as.factor(test$Year)
test$Month=as.factor(test$Month)
test$ct1=as.factor(test$ct1)
test$ct2=as.factor(test$ct2)
test$ct3=as.factor(test$ct3)
train$ct1=as.factor(train$ct1)
train$ct2=as.factor(train$ct2)
train$ct3=as.factor(train$ct3)




######################################################################################
######################################################################################
######################################################################################
library(randomForest)
library(mlbench)
library(caret)
data2=train
data2$Month=as.factor(data2$Month)
data2$Year=as.factor(data2$Year)
model = randomForest(Temp100cm~Year+Month+
                       SnowDepth+TempAir+TempSurf+ ct1+ct2+ct3+CMTemp100cm+CMTempAir+CMSnowDepth+
                       Month:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                       Year:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                        ct1:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                       ct2:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                       ct3:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth),
                     train)
solution = predict(model, data2)
sqrt(1/nrow(train)*sum((train$Temp100cm-solution)^2))
solution2 = predict(model, test)

#sqrt(1/nrow(test)*sum((Y_hat-solution)^2))

#solution=predict(model,test,n.trees=3000)
#out1=cbind(1:660,solution)
#out1=as.data.frame(out1)
#colnames(out1)=c("ID","Temp100cm")
#write.csv(out1,file = "out3.csv",row.names = FALSE)
#####


########################################################################
###################################################################
#####






library(gbm)

data2=train

data2$Month=as.factor(data2$Month)
model=gbm(Temp100cm~Year+Month+
            SnowDepth+TempAir+TempSurf+ ct1+ct2+ct3+CMTemp100cm+CMTempAir+CMSnowDepth+
            Month:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
            Year:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
            ct1:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
            ct2:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
            ct3:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)
          ,data=data2,distribution = "gaussian",n.trees = 10000,
          shrinkage = 0.01, interaction.depth = 24)
sqrt(1/nrow(data2)*sum((data2$Temp100cm-predict(model,train,n.trees=10000))^2))
#sqrt(1/nrow(test)*sum((Y_hat-predict(model,test,n.trees=3000))^2))
solution3=predict(model,test,n.trees=10000)
#
########################################################################
###################################################################
#####
nnet_model = nnet(Temp100cm~Year+Month+
                    SnowDepth+TempAir+TempSurf+ ct1+ct2+ct3+CMTemp100cm+CMTempAir+CMSnowDepth+
                    Month:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                    Year:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                    ct1:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                    ct2:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                    ct3:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth), 
                  data=data2,size=2,linout=T,skip=T,maxit=10000,decay=.0001)
sqrt(1/nrow(data2)*sum((data2$Temp100cm-predict(nnet_model,train))^2))
sqrt(1/nrow(test)*sum((Y_hat-predict(nnet_model,test))^2))
solution4=predict(nnet_model,test)
#########



out=cbind(solution1,solution2,solution3,solution4)
colnames(out)=c("knn","randf","GB","Nuetral")
out=as.data.frame(out)


out$Y=Y_hat
mregression=lm(Y~.,out)


########-----------------------------------------------------------###########
########-----------------------------------------------------------###########
########-----------------------------------------------------------###########
########-----------------------------------------------------------###########
########-----------------------------------------------------------###########
########-----------------------------------------------------------###########
########-----------------------------------------------------------###########
########-----------------------------------------------------------###########
########-----------------------------------------------------------###########
########-----------------------------------------------------------###########

########-----------------------------------------------------------###########






train_1<-read.csv("/Users/jacobzhu/Desktop/R code/module1/train-1.csv")
test_1<-read.csv("/Users/jacobzhu/Desktop/R code/module1/test-1.csv")
sample_data<-read.csv("/Users/jacobzhu/Desktop/R code/module1/sample-1.csv")
test_1=test_1[,2:6]
test_1$Temp100cm=NaN
train_2=train_1
train_1=rbind(train_1,test_1)
train_1$CMTemp100cm=NaN
train_1$CMTempAir=NaN
for (i in (1:12)){
  pa=train_2[train_2$Month==i,"Temp100cm"]
  train_1$CMTemp100cm[train_1$Month==i]=mean(pa[!is.nan(pa)])  
}

for (i in (1:12)){
  pa=train_2[train_2$Month==i,"TempAir"]
  train_1$CMTempAir[train_1$Month==i] = mean(pa[!is.nan(pa)])  
}

for (i in (1:12)){
  pa=train_2[train_2$Month==i,"TempAir"]
  train_1$CMTempAir[train_1$Month==i] = mean(pa[!is.nan(pa)])  
}
for (i in (1:12)){
  pa=train_2[train_2$Month==i,"SnowDepth"]
  train_1$CMSnowDepth[train_1$Month==i]=mean(pa[!is.nan(pa)])   
}



test_1=train_1[2749:nrow(train_1),]
train_1=train_1[1:2748,]





train=train_1
test=test_1

train_null=train
test_null=test

#*****
train_model=train
test_model=test
train_model=train[!is.nan(train$SnowDepth) & !is.nan(train$TempAir) & !is.nan(train$TempSurf) ,]
test_model=test[!is.nan(test$SnowDepth) & !is.nan(test$TempAir) & !is.nan(test$TempSurf) ,]
data2=train_model
data2$Year=as.factor(data2$Year)
data2$Month=as.factor(data2$Month)
m1=lm(SnowDepth~Year+Month+
        TempAir+TempSurf+
        Month:(TempAir+TempSurf)+
        Year:(TempAir+TempSurf)
      ,data2)
bma2=step(m1,scope=list(lower=~1, 
                        upper=~Year+Month+
                          TempAir+TempSurf+
                          Month:(TempAir+TempSurf)+
                          Year:(TempAir+TempSurf)
                        ,direction="both",data2))

train$Year=as.factor(train$Year)
train$Month=as.factor(train$Month)
test$Year=as.factor(test$Year)
test$Month=as.factor(test$Month)
train[is.nan(train$SnowDepth)  & !is.nan(train$TempAir) & !is.nan(train$TempSurf),"SnowDepth"]=predict.lm(bma2,train[is.nan(train$SnowDepth) & !is.nan(train$TempAir) & !is.nan(train$TempSurf),c(1,2,4,5,7)])
test[is.nan(test$SnowDepth)  & !is.nan(test$TempAir) & !is.nan(test$TempSurf),"SnowDepth"]=predict.lm(bma2,test[is.nan(test$SnowDepth) & !is.nan(test$TempAir) & !is.nan(test$TempSurf),c(1,2,4,5,7)])
train[!is.nan(train$SnowDepth) & train$SnowDepth<0,"SnowDepth"]=0;
test[!is.nan(test$SnowDepth) & test$SnowDepth<0,"SnowDepth"]=0;

test$Temp100cm=NaN



total=rbind(train,test)
total[1:nrow(train),]=knnImputation(total[1:nrow(train),])
library(DMwR)
total2=rbind(train_null,test_null)
total$ct1=as.factor(as.integer(is.nan(total2$SnowDepth)))
total$ct2=as.factor(as.integer(is.nan(total2$TempAir)))
total$ct3=as.factor(as.integer(is.nan(total2$TempSurf)))
total[,c(1:5,7:12)]=knnImputation(total[,c(1:5,7:12)])
##############
total=knnImputation(total)
solution1=total$Temp100cm[2749:nrow(total)]



train=total[1:nrow(train),]
test=total[(nrow(train)+1):nrow(total),]
test$Year=as.factor(test$Year)
test$Month=as.factor(test$Month)
test$ct1=as.factor(test$ct1)
test$ct2=as.factor(test$ct2)
test$ct3=as.factor(test$ct3)
train$ct1=as.factor(train$ct1)
train$ct2=as.factor(train$ct2)
train$ct3=as.factor(train$ct3)




######################################################################################
######################################################################################
######################################################################################
library(randomForest)
library(mlbench)
library(caret)
data2=train
data2$Month=as.factor(data2$Month)
data2$Year=as.factor(data2$Year)
model = randomForest(Temp100cm~Year+Month+
                       SnowDepth+TempAir+TempSurf+ ct1+ct2+ct3+CMTemp100cm+CMTempAir+CMSnowDepth+
                       Month:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                       Year:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                       ct1:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                       ct2:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                       ct3:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth),
                     train)
solution = predict(model, data2)
sqrt(1/nrow(train)*sum((train$Temp100cm-solution)^2))
solution2 = predict(model, test)

#sqrt(1/nrow(test)*sum((Y_hat-solution)^2))

#solution=predict(model,test,n.trees=3000)
#out1=cbind(1:660,solution)
#out1=as.data.frame(out1)
#colnames(out1)=c("ID","Temp100cm")
#write.csv(out1,file = "out3.csv",row.names = FALSE)
#####


########################################################################
###################################################################
#####







library(gbm)

data2=train

data2$Month=as.factor(data2$Month)
model=gbm(Temp100cm~Year+Month+
            SnowDepth+TempAir+TempSurf+ ct1+ct2+ct3+CMTemp100cm+CMTempAir+CMSnowDepth+
            Month:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
            Year:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
            ct1:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
            ct2:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
            ct3:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)
          ,data=data2,distribution = "gaussian",n.trees = 10000,
          shrinkage = 0.01, interaction.depth = 24)
sqrt(1/nrow(data2)*sum((data2$Temp100cm-predict(model,train,n.trees=10000))^2))
#sqrt(1/nrow(test)*sum((Y_hat-predict(model,test,n.trees=3000))^2))
solution3=predict(model,test,n.trees=10000)
#
########################################################################
###################################################################
#####
nnet_model = nnet(Temp100cm~Year+Month+
                    SnowDepth+TempAir+TempSurf+ ct1+ct2+ct3+CMTemp100cm+CMTempAir+CMSnowDepth+
                    Month:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                    Year:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                    ct1:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                    ct2:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth)+
                    ct3:(SnowDepth+TempAir+TempSurf+CMTemp100cm+CMTempAir+CMSnowDepth), 
                  data=data2,size=2,linout=T,skip=T,maxit=10000,decay=.0001)
sqrt(1/nrow(data2)*sum((data2$Temp100cm-predict(nnet_model,train))^2))
#sqrt(1/nrow(test)*sum((Y_hat-predict(nnet_model,test))^2))
solution4=predict(nnet_model,test)

#########



pred=cbind(solution1,solution2,solution3,solution4)
colnames(pred)=c("knn","randf","GB","Nuetral")
pred=as.data.frame(pred)
mregression=lm(Y~.,out)
mregression
final=predict(mregression,pred)
out1=cbind(1:660,final)
out1=as.data.frame(out1)
colnames(out1)=c("ID","Temp100cm")
write.csv(out1,file = "final_v3.csv",row.names = FALSE)
  