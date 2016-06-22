con<-file('poi.txt','r')
matrix<-matrix(data=0,nrow=26,ncol=25)
list<-list()
f<-function(x,matrix){
  num1<-length(grep("#",x))
  if(num1>0){
    re<-unlist(strsplit(unlist(strsplit(x,'#')),':'))
  }else{
    re<-unlist(strsplit(x,':'))
  }
  return(re)
}
line<-readLines(con,n=1)
line<-unlist(strsplit(line,'\t'))
index<-sapply(line,f)
num2<-length(index)
for(i in 1:num2){
  v2<-length(unlist(index[i]))
  if(v2==3){
    v1<-as.numeric(unlist(index[i]))
    a1<-as.integer(v1[1])
    a2<-as.integer(v1[2])
    a3<-as.integer(v1[3])
    matrix[a1,a2]<-a3
  }else if(v2==2){
    v1<-as.numeric(unlist(index[i]))
    a1<-as.integer(v1[1])
    a2<-as.integer(v1[2])
    matrix[26,a1]<-a2
  }
}
list[[1]]=matrix
k<-1
while(length(line)!=0){
  line<-readLines(con,n=1)
  line<-unlist(strsplit(line,'\t'))
  rm(index)
  matrix<-matrix(data=0,nrow=26,ncol=25)
  index<-sapply(line,f)
  num2<-length(index)
  for(i in 1:num2){
    v2<-length(unlist(index[i]))
    if(v2==3){
      v1<-as.numeric(unlist(index[i]))
      a1<-as.integer(v1[1])
      a2<-as.integer(v1[2])
      a3<-as.integer(v1[3])
      matrix[a1,a2]<-a3
    }else if(v2==2){
      v1<-as.numeric(unlist(index[i]))
      a1<-as.integer(v1[1])
      a2<-as.integer(v1[2])
      matrix[26,a1]<-a2
    }
  }
  k<-k+1
  list[[k]]=matrix
}
close(con)

#transform
result<-matrix(data=0,nrow=66,ncol=650)
for(j in 1:66){
  a1<-list[[j]]
  result[j,]=as.numeric(t(a1))
}
re<-data.frame(result)
write.csv(re,file='result_poi.csv')
