library(elo)
library(data.table)
library(RMySQL)
library(dplyr)

q=("select e.Season,gender,g.*,e.end_date,
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
games.orig=games.orig[!is.na(team1)]
dbDisconnect(con_local)
gms=games.orig
gms[,Date:=as.Date(end_date)]
gms=gms[order(end_date,Game)]

tms=rbindlist(list(gms[,.(Season,Date,team=team1,gender)],gms[,.(Season,Date,team=team2,gender)]))
tms[,n:=1:.N,team]
tms=tms[n==1]
tms[,n:=NULL]
minSeason=tms[,min(Season)]
tms[Season==minSeason,elo:=1500]
tms[Season>minSeason,elo:=1450]
tms[grepl('Junior',gender),elo:=1200]
tms=tms[,.(team,elo)]

gms=gms[,.(Season,Date,Game,Team1=team1,Score1=Total,Team2=team2,Score2=Total_opp,ends_scheduled)]
gms[,result:=1*(Score1>Score2)]
gms[Score1==Score2,result:=0.5]

games=gms[!is.na(result),.(Season,Date,Game,Team1,Team2,result,ends_scheduled,elo1=NA_real_,elo2=NA_real_)]

for (i in seq_len(nrow(games))) {
  print(paste0(i,'/',nrow(games)))
  gm=games[i]
  tm1_elo=subset(tms,team==gm$Team1)$elo
  tm2_elo=subset(tms,team==gm$Team2)$elo
  
  k_val=30
  if (gm$ends_scheduled==6) {k_val=25}
  if (gm$ends_scheduled==10) {k_val=35}
  
  new_elo=data.table(elo.calc(wins.A=gm$result,
                   elo.A=tm1_elo,
                   elo.B = tm2_elo,
                   k=k_val))
  
  tm1_new_elo=new_elo[1,elo.A]
  tm2_new_elo=new_elo[1,elo.B]
  
  tms=tms %>% mutate(elo=if_else(team==gm$Team1, tm1_new_elo,
                                 if_else(team==gm$Team2, tm2_new_elo, elo)))
  #tms=data.table(tms)
  #print(paste('non nas in elo =',count(tms[is.na(elo)])))
  games[i,':='(elo1=tm1_new_elo,elo2=tm1_new_elo)]
  

}
tms1=data.table(tms)
all_teams=rbindlist(list(games.orig[,.(team=team1,fn=first_name1,ln=last_name1,country=country1,Date)],
                         games.orig[,.(team2,first_name2,last_name2,country=country2,Date)]),use.names = F)
all_teams=all_teams[order(-Date)]
all_teams[!is.na(country) & !country %in% c('Minnesota','North Carolina'),n:=1:.N,'team']
all_teams=all_teams[n==1]
all_teams=all_teams[,.(team,fn,ln,country)]

tms1=merge(tms1,all_teams,'team')

#first...figure out current ratings based on the current season - 20 games minimum by end of season
wins=merge(
  games[Season==2020,.(sum(result),.N-sum(result)),'Team1'],
  games[Season==2020,.(.N-sum(result),sum(result)),'Team2'],
  by.x='Team1',by.y='Team2')
wins=wins[,.(team=Team1,W=V1.x+V1.y,L=V2.x+V2.y)]
out=merge(tms1,wins,'team',all.x=T)
out[,rk:=NA_integer_]
out[W+L>=20,rk:=frank(-elo,ties.method = 'min')]
out[W+L<20,rk:=999]
out[order(rk)] %>% head(50)

#now figure out top25/top50 wins/SOS
results = merge(gms,out[,.(Team1=team,elo1=elo,rk1=rk,fn1=fn,ln1=ln)],'Team1')
results = merge(results,out[,.(Team2=team,elo2=elo,rk2=rk,fn2=fn,ln2=ln)],'Team2')

wins2=merge(
  results[Season==2020,.(T10W=sum(result==1 & rk2<=10,na.rm=T),T10L=sum(result==0 & rk2<=10,na.rm=T),
                       T25W=sum(result==1 & rk2>10 & rk2<=25,na.rm=T),T25L=sum(result==0 & rk2>10 & rk2<=25,na.rm=T),
                       totSOS=sum(elo2),G=.N),'Team1'],
  results[Season==2020,.(T10W=sum(result==0 & rk1<=10,na.rm=T),T10L=sum(result==1 & rk1<=10,na.rm=T),
                       T25W=sum(result==0 & rk1>10 & rk1<=25,na.rm=T),T25L=sum(result==1 & rk1>10 & rk1<=25,na.rm=T),
                       totSOS=sum(elo1),G=.N),'Team2'],
  by.x='Team1',by.y='Team2',all.x=T)
wins2[is.na(wins2)]=0
wins2=wins2[,.(team=Team1,T10W=T10W.x+T10W.y,T10L=T10L.x+T10L.y,T25W=T25W.x+T25W.y,T25L=T25L.x+T25L.y,
               SOS=(totSOS.x+totSOS.y)/(G.x+G.y))]
out=merge(out,wins2,'team',all.x=T)
out[W+L>=20,rkSOS:=frank(-SOS)]

#get last year's elo
last_yr=rbindlist(list(games[,.(Season,Date,Game,team=Team1,old_elo=elo1)],games[,.(Season,Date,Game,team=Team2,old_elo=elo2)]))
last_yr=last_yr[order(-Date,-Game)]
last_yr=last_yr[Season==2019]
last_yr[,n:=1:.N,team]
last_yr=last_yr[n==1]
out=merge(out,last_yr[,.(team,old_elo)],'team',all.x=T)
out[,yr_chg:=elo-old_elo]

out=out[,.(rk,fn,ln,country,elo=round(elo),W=round(W),L=round(L),T25W,T25L,T50W,T50L,SOS=round(SOS),rkSOS)]
out[order(rk)] %>% head(50)

