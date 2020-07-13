#bradley terry sandbox
library(data.table)

dt=data.table(tm1=c('A','A','B'),tm2=c('B','C','C'),W1=c(1,0,1),W2=c(2,3,1))

x=rbindlist(list(dt,dt[,.(tm1=tm2,tm2=tm1,W1=W2,W2=W1)]))
x[,totW:=sum(W1),'tm1']
x[,rtg:=.5]
x=merge(x,x[,.(tm1=tm2,tm2=tm1,opp_rtg=rtg)],c('tm1','tm2'))

a=2
b=1
for (i in 1:1000) {
  x[,denom:=(W1+W2)/(rtg+opp_rtg)]
  x[,new_rtg:=(a-1+sum(W1))/(b+sum(denom)),'tm1']
  max_delta=max(x[,.(delta=abs(new_rtg-rtg))])
  print (paste(i,max_delta))
  x[,rtg:=new_rtg]
  x[,opp_rtg:=NULL]
  x=merge(x,x[,.(tm1=tm2,tm2=tm1,opp_rtg=rtg)],c('tm1','tm2'))
}
