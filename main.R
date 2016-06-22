options(digits=15)
train<-read.csv('result_didi2.csv')
test<-read.csv('final_testing.csv')
library(datastandard)
library(AMORE)
train1<-f_standard(train)
ma<-sapply(train,max)
mi<-sapply(train,min)
f<-function(test,train_ma,train_mi){
    result1<-test-train_mi
    result2<-train_ma-train_mi
    result3<-result1/result2
    return(result3)
}
test2<-sapply(test[3],f,train_ma=ma[3],train_mi=mi[3])
test3<-as.data.frame(test2)
for(i in 4:ncol(test)){
  test2<-sapply(test[i],f,train_ma=ma[i],train_mi=mi[i])
  test2<-as.data.frame(test2)
  test3<-cbind(test3,test2)
}
test4<-matrix(unlist(test3),nrow=nrow(test3))
predict_deta=ma[1]-mi[1]
result<-matrix(data=0,nrow = nrow(test3),ncol = 4)
for(hh in 1){
  train1<-train1[sample(row.names(train1),floor(length(row.names(train1))*0.8)),]
  train2<-matrix(unlist(train1),nrow=nrow(train1))
  ncol1<-ncol(train2)-2
  ncol2<-ncol1+2
  net<-newff(n.neurons = c(ncol1,16,1),learning.rate.global = 0.1,error.criterium = 'LMS',Stao = NA,hidden.layer = 'sigmoid',output.layer = 'sigmoid',method='ADAPTgd')
  train_set<-train(net=net,train2[,3:ncol2],train2[,1],n.shows = 500,show.step = 500)
  # transform matrix
  predict_set<-sim.MLPnet(train_set$net,test4[,1:ncol(test3)])
  predict_set2<-predict_set*predict_deta+mi[1]
  result[1:nrow(test3),hh]<-predict_set2
  
}
re<-as.data.frame(result)
write.csv(re,'result1.csv')
#result,first time only one column,other is zero


