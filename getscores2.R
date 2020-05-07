library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(RMySQL)
season=2016
rewrite=0
s=season
gender='Women'
#event_code=6037

#get existing events from db
q=paste0("select event_id from events")
con_local=try(dbConnect(RMySQL::MySQL(),dbname='curling',username="root",password='',host='localhost'))
existing_ids=data.table(dbGetQuery(con_local,q))
existing_ids=unique(existing_ids$event_id)

source('get_events.R')
source('get_teams.R')
source('get_games.R')

events=get_events(season,gender)
needed_ids=events[!(event_id %in% existing_ids),event_id]

if (rewrite==1) {needed_ids=new_ids}
for (id in needed_ids) {
  #id=6376
  event_code=id
  print(paste0('getting data for event #',event_code," (",events[event_id==event_code,event_name],")"))

  #get teams
  teams=get_teams(id,gender)
  if (teams=='error') {next}
  
  games=get_games(id)
  if (games=='error') {next}
  event=events[event_code==event_id]
  q=paste0("replace into events (season,event_id,event,start_date,end_date,gender) 
           values (",event$season,",",event$event_id,",'",sub("'","''",event$event_name),"','",event$start_date,
           "','",event$end_date,"','",event$gender,"')")
  dbSendStatement(con_local,q)

  if (is.null(games)) {next}

  games=games[!is.na(E1)]
  if(nrow(games)==0) {
    warning('no games found')
    next
  }
  
  #add teams to db
  outfile='teams.csv'
  if (nrow(teams)>0) {
    write.csv(teams,outfile,row.names = F,na = 'NULL')
    
    q=paste0("LOAD DATA LOCAL INFILE '",outfile,"' REPLACE INTO TABLE teams
             FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"'
             LINES TERMINATED BY '\n' IGNORE 1 LINES")
    dbSendStatement(con_local,q)
  }
  cat (paste0("****(Teams in event: ",nrow(teams),")\n"))
  
  #TODO: delete existing games for this event
  q=paste0('delete from games where event_id=',event_code)
  dbSendStatement(con_local,q)
  
  outfile='games.csv'
  
  write.csv(games,outfile,row.names = F,na = 'NULL')
  q=paste0("LOAD DATA LOCAL INFILE '",outfile,"' REPLACE INTO TABLE games
             FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"'
           LINES TERMINATED BY '\n' IGNORE 1 LINES")
  dbSendStatement(con_local,q)
  
  cat (paste0("****added ",nrow(games)," new games.\n"))
}
e=dbDisconnect(con_local)

all_cons <- dbListConnections(MySQL())

for(con in all_cons)
  +  dbDisconnect(con)
