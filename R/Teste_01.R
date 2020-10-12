#teste

a<-sample(1:6,size=1000,replace=TRUE,prob=rep(1/6,6))
(b<-table(a)/1000)
hist(a)
