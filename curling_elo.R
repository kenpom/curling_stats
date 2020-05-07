library(elo)
library(data.table)
library(RMySQL)
library(dplyr)

q=("select e.Season,g.event_id,e.provider_id,g.team_id,g.Draw,g.Game,
g.Date,t.Fourth Team1,g.Total Score1,t2.Fourth Team2,g.Total_opp Score2 from curling_games g 
join curling_teams t on g.team_id=t.id
join curling_teams t2 on g.Opp_team_id=t2.id 
join curling_events e on e.id=g.event_id where gender in ('Women')")
con_local=try(dbConnect(RMySQL::MySQL(),dbname='kenpom_1',username="root",password='',host='localhost'))
games.orig=data.table(dbGetQuery(con_local,q))
gms=games.orig
gms[,Date:=as.Date(Date)]
gms=gms[order(Date)]

gms[,n:=1:.N,.(event_id,Draw,Game)]
gms=gms[n==1]
gms[,c('n','Draw','Game','event_id'):=NULL]

tms=data.table(team=unique(c(gms$Team1,gms$Team2)))
tms[,elo:=1500]

gms[,result:=1*(Score1>Score2)]
gms[Score1==Score2,result:=0.5]

games=gms[!is.na(result),.(Season,Date,Team1,Team2,result)]
i=1
for (i in seq_len(nrow(games))) {
  print(paste0(i,'/',nrow(games)))
  gm=games[i]
  tm1_elo=subset(tms,team==gm$Team1)$elo
  tm2_elo=subset(tms,team==gm$Team2)$elo
  
  new_elo=data.table(elo.calc(wins.A=gm$result,
                   elo.A=tm1_elo,
                   elo.B = tm2_elo,
                   k=30))
  
  tm1_new_elo=new_elo[1,elo.A]
  tm2_new_elo=new_elo[1,elo.B]
  
  tms=tms %>% mutate(elo=if_else(team==gm$Team1, tm1_new_elo,
                                 if_else(team==gm$Team2, tm2_new_elo, elo)))

}
tms=data.table(tms)
tms[order(-elo)] %>% head(10)
dbDisconnect(con_local)

wins=merge(
  games[Season==2020,.(sum(result),.N-sum(result)),'Team1'],
  games[Season==2020,.(.N-sum(result),sum(result)),'Team2'],
  by.x='Team1',by.y='Team2')
wins=wins[,.(team=Team1,W=V1.x+V1.y,L=V2.x+V2.y)]
out=merge(tms,wins,'team')
out[,rk:=frank(-elo)]
out[order(-elo)] %>% head(50)

games.orig[Season==2020 & Team1=='Mackenzie Zacharias'][order(Date)]
games.orig[Season==2020 & Team1=='New Brunswick'][order(Date)]

out[team=='Mary Mattatall'] 

games.orig[team_id==1943]

