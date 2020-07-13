#scotties/brier monte carlo
#men
poolA=c(250,25604,32,3784,671,3044,637,15871)
poolB=c(21999,249,11055,11484,258,278,26410,2083)
#women
poolA=c(16827,265,13918,2025,22611,17588,3884,23025)
poolB=c(10078,166,262,984,25422,292,22261,1006)

#get ratings
rtg=fread('csv/Women.csv')
a=rtg[tm %in% poolA,.(tm,name=paste(fn,ln),ability)]
b=rtg[tm %in% poolB,.(tm,name=paste(fn,ln),ability)]

tourney=data.table(rep(b$tm,8),rep(b$tm,each=8))
tourney[,gm:=paste0(pmin(V1,V2),'_',pmax(V1,V2))]
tourney[,n:=1:.N,gm]
tourney=tourney[n==1 & V1!=V2]
tourney[,c('gm','n'):=NULL]

gms=merge(tourney[,.(tm1=V1,tm2=V2)],rtg[,.(tm1=tm,rtg1=ability)],'tm1')
gms=merge(gms,rtg[,.(tm2=tm,rtg2=ability)],'tm2')
gms[,prob1:=exp(rtg1-rtg2)/(1+exp(rtg1-rtg2))]

#round robin stage
draws=data.table(sim=rep(1:100000,each=nrow(gms)),tm1=gms$tm1,tm2=gms$tm2,
                 rtg1=gms$rtg1,rtg2=gms$rtg2,prob1=gms$prob1)
draws[,p:=runif(nrow(draws))]
draws[,':='(res1=1*(p<prob1),res2=1*(p>prob1))]
wins=rbindlist(list(draws[,.(W=sum(res1)),.(tm1,sim)],draws[,.(W=sum(res2)),.(tm2,sim)]),use.names = F)
wins=wins[,.(W=sum(W)),.(tm1,sim)]
wins[,rk:=frank(-W,ties.method = 'min'),sim]
wins[rk<=4,.N,tm1]

#second round robin

#page playoff

