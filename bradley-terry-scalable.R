#bradley terry sandbox
library(BradleyTerry2)
library(brglm2)
library(data.table)
library(RMySQL)
gender='Women'
last_date=as.Date('2020-02-15')
#get W/L's for this season 
q=("select e.Season,e.event_id,e.end_date,g.Total,g.opp_team_id,g.Total_opp,
case t1.skip when 4 then t1.id4 when 3 then t1.id3 when 2 then t1.id2 when 1 then t1.id1 else t1.id4 end team1,
case t1.skip when 4 then t1.first_name4 when 3 then t1.first_name3 when 2 then t1.first_name2 when 1 then t1.first_name1 else t1.first_name4 end first_name1,
case t1.skip when 4 then t1.last_name4 when 3 then t1.last_name3 when 2 then t1.last_name2 when 1 then t1.last_name1 else t1.last_name4 end last_name1,
case t2.skip when 4 then t2.id4 when 3 then t2.id3 when 2 then t2.id2 when 1 then t2.id1 else t1.id4 end team2,
case t2.skip when 4 then t2.first_name4 when 3 then t2.first_name3 when 2 then t2.first_name2 when 1 then t2.first_name1 else t2.first_name4 end first_name2,
case t2.skip when 4 then t2.last_name4 when 3 then t2.last_name3 when 2 then t2.last_name2 when 1 then t2.last_name1 else t2.last_name4 end last_name2,
t1.country country1,t2.country country2,t1.junior junior1,t2.junior junior2
from events e
join games g on g.event_id=e.event_id
join teams t1 on g.team_id=t1.team_id
join teams t2 on g.opp_team_id=t2.team_id where gender in ")
if (gender=='Men') {
  q=paste0(q,"('men','junior men')")
} else {
  q=paste0(q,"('women','junior women')")
}
con_local=try(dbConnect(RMySQL::MySQL(),dbname='curling',username="dbmasteruser",password='j)n$#P)xg:5kV|A1r[40r9+?:h`BVBTL',host='ls-22fe63cc95c378303961f16ce9e5e616e5d48bdd.cwyvyahhslya.ca-central-1.rds.amazonaws.com'))
games.orig=data.table(dbGetQuery(con_local,q))

gms=games.orig
gms=gms[Total!=Total_opp]
gms[,end_date:=as.Date(end_date)]
gms=gms[end_date<=last_date]
maxDate=max(gms$end_date)
gms[,days_old:=as.numeric(difftime(maxDate,end_date,units = 'days'))]
gms[,weight:=(730-days_old)/730]
gms[,Season:=as.numeric(Season)]

results=gms[weight>0,.(sum(weight*(Total>Total_opp)),sum(weight*(Total<Total_opp))),c('team1','team2')]
lev=factor(unique(c(results$team1,results$team2)))
results[,team1:=factor(team1,levels=lev)]
results[,team2:=factor(team2,levels=lev)]
btdata
btfit(results,1)


m=BTm(cbind(V1, V2), team1, team2, data = results, id = "tm",br=T)
x=BTabilities(m)
#stop()
x=cbind(row.names(x),data.table(BTabilities(m)))
setnames(x,'V1','tm')
x=x[order(-ability)]
tms=unique(rbindlist(list(gms[,.(end_date,tm=team1,fn=first_name1,ln=last_name1)],
                          gms[,.(end_date,tm=team2,fn=first_name2,ln=last_name2)])))
tms=tms[order(-end_date)]
tms[,n:=1:.N,tm]
tms=tms[n==1,.(fn,ln),tm]

#find junior teams
thisSeason=gms[,max(Season)]
juniors=unique(rbindlist(list(gms[Season==thisSeason & junior1==1,.(tm=team1,jr=junior1)],
                              gms[Season==thisSeason & junior2==1,.(tm=team2,jr=junior2)])))

tms=merge(tms,juniors,'tm',all.x=T)
tms[is.na(jr),jr:=0]

wins=merge(
  gms[Season==thisSeason,.(sum(Total>Total_opp),.N-sum(Total>Total_opp)),'team1'],
  gms[Season==thisSeason,.(.N-sum(Total>Total_opp),sum(Total>Total_opp)),'team2'],
  by.x='team1',by.y='team2',all=T)
wins[is.na(wins)]=0

wins=wins[,.(tm=team1,W0=V1.x+V1.y,L0=V2.x+V2.y)]

x[,tm:=as.integer(tm)]
y=merge(x,tms,'tm')[order(-ability)]
y=merge(y,wins,'tm',all.x=T)
y[is.na(y)]=0

#figure out minimum number of games to rank ~250 teams
y[,rankwins:=frank(-W0-L0,ties.method = 'min')]
minGames=y[rankwins<200,min(W0+L0)]
y[,rk:=NA_integer_]

#if minGames is 0 then get records from previous season
if (minGames==0) {
  wins=merge(
    gms[Season>=thisSeason-1,.(sum(Total>Total_opp),.N-sum(Total>Total_opp)),'team1'],
    gms[Season>=thisSeason-1,.(.N-sum(Total>Total_opp),sum(Total>Total_opp)),'team2'],
    by.x='team1',by.y='team2',all=T)
  wins[is.na(wins)]=0
  wins=wins[,.(tm=team1,W1=V1.x+V1.y,L1=V2.x+V2.y)]
  y=merge(y,wins,'tm')[order(-ability)]
  #y=merge(y,wins,'tm',all.x=T)
  y[is.na(y)]=0
  y[,rankwins:=frank(-W1-L1,ties.method = 'min')]
  minGames=y[rankwins<200,min(W1+L1)]
  if (minGames>20) {minGames=20}
  y[W1+L1>=minGames,rk:=frank(-ability)]
} else {
  if (minGames>20) {minGames=20}
  y[W0+L0>=minGames,rk:=frank(-ability)]
}
y[,rankwins:=NULL]
y[rk==0,rk:=NA]
#give team ranked 100 a rating of 10
bias=y[rk==100,ability]
y[,ability:=ability-bias+10]
y=y[order(rk)]
y=y[,.(tm,ability,fn,ln,jr,W=W0,L=L0,rk)]

z = merge(gms,y[,.(team1=tm,rtg1=ability,rk1=rk,fn1=fn,ln1=ln)],'team1')
z = merge(z,y[,.(team2=tm,rtg2=ability,rk2=rk,fn2=fn,ln2=ln)],'team2')
z[,result:=(Total>Total_opp)]
wins2=merge(
  z[Season==thisSeason,.(T10W=sum(result==1 & rk2<=10,na.rm=T),T10L=sum(result==0 & rk2<=10,na.rm=T),
                         T25W=sum(result==1 & rk2>10 & rk2<=25,na.rm=T),T25L=sum(result==0 & rk2>10 & rk2<=25,na.rm=T),
                         totSOS=sum(rtg2),G=.N),'team1'],
  z[Season==thisSeason,.(T10W=sum(result==0 & rk1<=10,na.rm=T),T10L=sum(result==1 & rk1<=10,na.rm=T),
                   T25W=sum(result==0 & rk1>10 & rk1<=25,na.rm=T),T25L=sum(result==1 & rk1>10 & rk1<=25,na.rm=T),
                   totSOS=sum(rtg1),G=.N),'team2'],
  by.x='team1',by.y='team2',all.x=T)
wins2[is.na(wins2)]=0
wins2=wins2[,.(tm=team1,T10W=T10W.x+T10W.y,T10L=T10L.x+T10L.y,T25W=T25W.x+T25W.y,T25L=T25L.x+T25L.y,
               SOS=(totSOS.x+totSOS.y)/(G.x+G.y))]
y=merge(y,wins2,'tm',all.x=T)
y[is.na(T10W),c('T10W','T10L','T25W','T25L'):=0]
y[!is.na(rk),rkSOS:=frank(-SOS,ties.method = 'min')]
y=y[order(rk)]

y[,Date:=maxDate]

f=paste0('csv/',gender,'.csv')
write.csv(y[,.(Date,tm,rk,round(ability,3),W,L,T10W,T10L,T25W,T25L,round(SOS,3),rkSOS,jr)],
          f,row.names = F,na='NULL')

q=paste0("load data local infile '",f,"' REPLACE INTO TABLE ratings_",tolower(gender), 
" FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"'
LINES TERMINATED BY '\n' IGNORE 1 LINES")

dbSendStatement(con_local,q)
dbDisconnect(con_local)
