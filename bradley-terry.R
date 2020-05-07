#bradley terry sandbox
library(BradleyTerry2)
library(brglm)
library(data.table)
library(RMySQL)
#get W/L's for this season 
q=("select e.Season,e.event_id,e.end_date,g.Total,g.opp_team_id,g.Total_opp,
case t1.skip when 4 then t1.id4 when 3 then t1.id3 when 2 then t1.id2 when 1 then t1.id1 end team1,
case t1.skip when 4 then t1.first_name4 when 3 then t1.first_name3 when 2 then t1.first_name2 when 1 then t1.first_name1 end first_name1,
case t1.skip when 4 then t1.last_name4 when 3 then t1.last_name3 when 2 then t1.last_name2 when 1 then t1.last_name1 end last_name1,
case t2.skip when 4 then t2.id4 when 3 then t2.id3 when 2 then t2.id2 when 1 then t2.id1 end team2,
case t2.skip when 4 then t2.first_name4 when 3 then t2.first_name3 when 2 then t2.first_name2 when 1 then t2.first_name1 end first_name2,
case t2.skip when 4 then t2.last_name4 when 3 then t2.last_name3 when 2 then t2.last_name2 when 1 then t2.last_name1 end last_name2,
t1.country country1,t2.country country2
from events e
join games g on g.event_id=e.event_id
join teams t1 on g.team_id=t1.team_id
join teams t2 on g.opp_team_id=t2.team_id where gender in ('men','junior men')")
con_local=try(dbConnect(RMySQL::MySQL(),dbname='curling',username="root",password='',host='localhost'))
games.orig=data.table(dbGetQuery(con_local,q))

gms=games.orig
results=gms[Season==2020,.(sum(Total>Total_opp),sum(Total<Total_opp)),c('team1','team2')]
lev=factor(unique(c(results$team1,results$team2)))
results[,team1:=factor(team1,levels=lev)]
results[,team2:=factor(team2,levels=lev)]
m=BTm(cbind(V1, V2), team1, team2, data = results, id = "tm",br=T)
x=BTabilities(m)

x=cbind(row.names(x),data.table(BTabilities(m)))
setnames(x,'V1','tm')
x=x[order(-ability)]
tms=unique(rbindlist(list(gms[,.(end_date,tm=team1,fn=first_name1,ln=last_name1)],
                          gms[,.(end_date,tm=team2,fn=first_name2,ln=last_name2)])))
tms=tms[order(-end_date)]
tms[,n:=1:.N,tm]
tms=tms[n==1,.(fn,ln),tm]

x[,tm:=as.integer(tm)]
y=merge(x,tms,'tm')[order(-ability)]

wins=merge(
  gms[Season==2020,.(sum(Total>Total_opp),.N-sum(Total>Total_opp)),'team1'],
  gms[Season==2020,.(.N-sum(Total>Total_opp),sum(Total>Total_opp)),'team2'],
  by.x='team1',by.y='team2')
wins=wins[,.(tm=team1,W=V1.x+V1.y,L=V2.x+V2.y)]

y=merge(y,wins,'tm')
y[W+L>=20,rk:=frank(-ability)]
y=y[order(rk)]

z = merge(gms,y[,.(team1=tm,rtg1=ability,rk1=rk,fn1=fn,ln1=ln)],'team1')
z = merge(z,y[,.(team2=tm,rtg2=ability,rk2=rk,fn2=fn,ln2=ln)],'team2')
z[,result:=(Total>Total_opp)]
wins2=merge(
  z[Season==2020,.(T10W=sum(result==1 & rk2<=10,na.rm=T),T10L=sum(result==0 & rk2<=10,na.rm=T),
                         T25W=sum(result==1 & rk2>10 & rk2<=25,na.rm=T),T25L=sum(result==0 & rk2>10 & rk2<=25,na.rm=T),
                         totSOS=sum(rtg2),G=.N),'team1'],
  z[Season==2020,.(T10W=sum(result==0 & rk1<=10,na.rm=T),T10L=sum(result==1 & rk1<=10,na.rm=T),
                   T25W=sum(result==0 & rk1>10 & rk1<=25,na.rm=T),T25L=sum(result==1 & rk1>10 & rk1<=25,na.rm=T),
                   totSOS=sum(rtg1),G=.N),'team2'],
  by.x='team1',by.y='team2',all.x=T)
wins2[is.na(wins2)]=0
wins2=wins2[,.(tm=team1,T10W=T10W.x+T10W.y,T10L=T10L.x+T10L.y,T25W=T25W.x+T25W.y,T25L=T25L.x+T25L.y,
               SOS=(totSOS.x+totSOS.y)/(G.x+G.y))]

y=merge(y,wins2,'tm')
y[!is.na(rk),rkSOS:=frank(-SOS)]
y=y[order(rk)]
