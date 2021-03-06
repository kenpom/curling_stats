library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(RMySQL)
season=2020
rewrite=0
s=season
gender='Men'
#event_code=6037
months=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

#get existing events from db
q=paste0("select provider_id from curling_events where gender='",gender,"'")
con_local=try(dbConnect(RMySQL::MySQL(),dbname='kenpom_1',username="root",password='',host='localhost'))
existing_ids=data.table(dbGetQuery(con_local,q))
existing_ids=unique(existing_ids$provider_id)

source('get_events.R')
source('get_teams.R')
new_ids=get_events(season,gender)
needed_ids=new_ids[!(new_ids %in% existing_ids)]
if (rewrite==1) {needed_ids=new_ids}
id=5874
for (id in needed_ids) {
# for (j in 1:nrow(dt)) {
  event_code=id
  print(paste0('getting data for event #',event_code))

  #get teams
  teams=get_teams(id,gender)
  
  url_base=paste0("https://www.curlingzone.com/event.php?eventid=",event_code,"&view=Scores&showdrawid=")
  results.all=NULL

  for (i in 1:22) {
    print(i)
    url=paste0(url_base,i)
    xmlFile=getURL(url)
    xmlFile=gsub('&nbsp;','',xmlFile)
    xmlFile=gsub("<img src=https://www.curlingzone.com/forums/images/hammer.gif style='height: 15px; width: 15px;'>",'H',xmlFile)
    event=gsub('.+<title>(.+)</title>.+','\\1',xmlFile)
    event=sub("'","",event)
    event_month=gsub('.+<br>[A-Z][a-z][a-z], ([A-Z][a-z][a-z]) [0-9]+.+','\\1',xmlFile)
    event_month=which(months==event_month)
    event_day=gsub('.+<br>[A-Z][a-z][a-z], [A-Z][a-z][a-z] ([0-9]+).+','\\1',xmlFile)
    xmlFile=gsub('<br>','|',xmlFile)
    xmlTables=readHTMLTable(xmlFile)
    for (j in 1:length(xmlTables)) {
      if ('HMR' %in% xmlTables[j]$`NULL`$V2) {break}
    }
  
    results=data.table(xmlTables[j]$`NULL`)
    if (length(results$V2)==0) {next}
    if (length(results$V12)==0) {next}
    results=results[!is.na(V2)]
    results=results[!is.na(V4)]
    ee_cols=unique(names(results)[which(results == 'EE', arr.ind=T)[, "col"]])
    if (length(ee_cols)>1) {
      results[,ee_cols[2:length(ee_cols)]:=NULL]
    }
    colnames(results)=as.character(paste0("V",seq(1,length(colnames(results)))))
    results[,V2:=as.character(V2)]
    results[,c('V1','V2'):=tstrsplit(V2,'|',fixed=T)]
    setnames(results,c('V1','V2'),c('Team','Location'))
    if (nrow(results[V3=='H'])==0) {results[,V3:=NULL]}
    if (results[Team=='HMR' & V10==8,.N]==0) {
      ends=6
    } else if (results[Team=='HMR' & V12==10,.N]==0) {
      ends=8
    } else {
      ends=10
    }

    #TODO:need to account for extra end
    if (ends==6) {
      if (length(results$V12)==0) {results$V14=NA_integer_}
      if (length(results$V11)==0) {next}
      #if ('V16' %in% names(results) & !'V14' %in% names(results))
      setnames(results,c('V4','V5','V6','V7','V8','V9','V10','V11','V12'),
               c('Hammer','E1','E2','E3','E4','E5','E6','EE','Total'))
      SDcolsH=c('E1','E2','E3','E4','E5','E6','EE','Total','Hammer')
      SDcols=c('E1','E2','E3','E4','E5','E6','EE','Total')
    } else if (ends==8) {
      if (length(results$V14)==0) {results$V14=NA_integer_}
      if (length(results$V13)==0) {next}
      if (length(results$V15)>0) {
        results[V14=='',V14:=V15][,V15:=NULL]
      }
      setnames(results,c('V4','V5','V6','V7','V8','V9','V10','V11','V12','V13','V14'),
               c('Hammer','E1','E2','E3','E4','E5','E6','E7','E8','EE','Total'))
      SDcolsH=c('E1','E2','E3','E4','E5','E6','E7','E8','EE','Total','Hammer')
      SDcols=c('E1','E2','E3','E4','E5','E6','E7','E8','EE','Total')
    } else {
      if (length(results$V16)==0) {results$V16=NA_integer_}
      if (length(results$V15)==0) {next}
      setnames(results,c('V4','V5','V6','V7','V8','V9','V10','V11','V12','V13','V14','V15','V16'),
               c('Hammer','E1','E2','E3','E4','E5','E6','E7','E8','E9','E10','EE','Total'))
      SDcolsH=c('E1','E2','E3','E4','E5','E6','E7','E8','E9','E10','EE','Total','Hammer')
      SDcols=c('E1','E2','E3','E4','E5','E6','E7','E8','E9','E10','EE','Total')
    }
    results=results[!is.na(Location)]
    results[,Draw:=i]
    results[,Game:=ceiling((1:.N)/2)]
    results[,(SDcolsH):=lapply(.SD,as.character),.SDcols=SDcolsH]
    results[,(SDcols):=lapply(.SD,function(x) {ifelse(x %in% c('X',''),NA_integer_,x)}),.SDcols=SDcols]
    results[,(SDcols):=lapply(.SD,as.numeric),.SDcols=SDcols]
    results[,':='(Total=ifelse(is.na(Total),EE,Total),EE=ifelse(is.na(Total),NA,EE))]
    if (event_month>5) {season_year=season-1} else {season_year=season}
    results[,Date:=paste0(season_year,'-',sprintf('%02d',as.numeric(event_month)),'-',sprintf('%02d',as.numeric(event_day)))]
    if (ends==6) {results[,c('E7','E8','E9','E10'):=NA_integer_]}
    if (ends==8) {results[,c('E9','E10'):=NA_integer_]}
    results[,ends_scheduled:=ends]  
    if (is.null(results.all)) {
      results.all=results
    } else {
        results.all=rbindlist(list(results.all,results),use.names=T,fill = T)
    }
    if (length(results.all$V17)>0) {stop('error')}
    #print (names(results.all))
  }
  cat (paste0('Event=',event,'\n'))
  #get event id

  q=paste0("select * from curling_events")
  events=data.table(dbGetQuery(con_local,q))
  g=gender
  if (nrow(events[event_name==event & season==s & gender==g])>0) {
    kp_id=events[event_name==event & season==s & gender==g,id]
  } else {
    kp_id=max(0,max(events$id))+1
  }

  q=paste0("replace into curling_events (id,provider_id,season,event_name,tour,ends_scheduled,gender) 
           values (",kp_id,",",event_code,",",s,",'",event,"',NULL,",ends,",'",gender,"')")
  dbSendStatement(con_local,q)

  if (is.null(results.all)) {next}
  results.all[,maxP:=max(Total),c('Draw','Game')][,W:=ifelse(maxP==Total,'W','L')][,maxP:=NULL]

  results.all=results.all[!is.na(E1)]
  if(nrow(results.all)==0) {
    warning('no games found')
    next
  }
  
  #check for missing hammers
  #results.all[Hammer=='H' & !is.na(Total),sum(W=='W')/.N]
  #results.all[,sum(W=='W')/.N]
  results.all=merge(
    results.all,
    results.all[,.(Hammers=sum(Hammer=='H')),c('Draw','Game')][Hammers==0],c('Game','Draw'),all.x=T)
  results.all[Hammers==0,Hammer:='M']
  results.all[,Hammers:=NULL]
  
  #check if team is in db - get id or add it
  q='select * from curling_teams'
  tms=data.table(dbGetQuery(con_local,q))
  start_id=max(1,max(tms$id)+1)
  event_tms=unique(results.all[,.(Fourth=Team,Location)])
  event_tms[,c('City','Country'):=tstrsplit(Location,',',keep = c(1,2))]
  
  new_teams=merge(tms,event_tms,by=c('Fourth','City','Country'),all.y=T)[is.na(id)]
  new_teams[,id:=start_id:(start_id+.N-1)]
  outfile='results.csv'
  if (nrow(new_teams)>0) {
    write.csv(new_teams[,.(id,Lead,Second,Third,Fourth,City,Country)],outfile,row.names = F,na = 'NULL')
    
    q=paste0("LOAD DATA LOCAL INFILE '",outfile,"' REPLACE INTO TABLE curling_teams
             FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"'
             LINES TERMINATED BY '\n' IGNORE 1 LINES")
    dbSendStatement(con_local,q)
  }
  cat (paste0("****added ",nrow(new_teams)," new teams. (Teams in event: ",nrow(event_tms),")\n"))
  
  tms=rbindlist(list(tms,new_teams[,.(id,Lead,Second,Third,Fourth,City,Country)]))
  
  results.all=merge(results.all,tms[,.(team_id=id,Team=Fourth,Location=paste(City,Country,sep = ','))],
                    by=c('Team','Location'))
  results.all[,c('Team','Location'):=NULL]
  
  setcolorder(results.all,c("Game","Draw","team_id","W","Hammer","E1","E2","E3","E4","E5",
  "E6","E7","E8","E9","E10","EE","Total","Date",'ends_scheduled'))
  
  results.all=rbindlist(list(
    merge(results.all[Hammer=='H'],results.all[Hammer!='H'],c('Draw','Game'),suffixes = c('','.opp')),
    merge(results.all[Hammer!='H'],results.all[Hammer=='H'],c('Draw','Game'),suffixes = c('','.opp'))
  ))
  
  #figure out hammer for each end
  results.all[,H1:=ifelse(Hammer=='H',1,0)]
  results.all[,H2:=ifelse(E1==E1.opp,H1,ifelse(E1>0,0,1))]
  results.all[,H3:=ifelse(E2==E2.opp,H2,ifelse(E2>0,0,1))]
  results.all[,H4:=ifelse(E3==E3.opp,H3,ifelse(E3>0,0,1))]
  results.all[,H5:=ifelse(E4==E4.opp,H4,ifelse(E4>0,0,1))]
  results.all[,H6:=ifelse(E5==E5.opp,H5,ifelse(E5>0,0,1))]
  results.all[,H7:=ifelse(E6==E6.opp,H6,ifelse(E6>0,0,1))]
  results.all[,H8:=ifelse(E7==E7.opp,H7,ifelse(E7>0,0,1))]
  results.all[,H9:=ifelse(E8==E8.opp,H8,ifelse(E9>0,0,1))]
  results.all[,H10:=ifelse(E9==E9.opp,H9,ifelse(E10>0,0,1))]
  results.all[ends_scheduled==6,HEE:=ifelse(E6==E6.opp,H8,ifelse(E6>0,0,1))]
  results.all[ends_scheduled==8,HEE:=ifelse(E8==E8.opp,H8,ifelse(E8>0,0,1))]
  results.all[ends_scheduled==10,HEE:=ifelse(E10==E10.opp,H10,ifelse(E10>0,0,1))]
  results.all[,HEE:=as.numeric(HEE)]
  results.all[,c('Date.opp','Hammer.opp','Hammer','W.opp'):=NULL]
  
  #TODO: need to check if last end is scoreless and make it null if so.
  
  new_rows=c('event_id','Date')
  results.all[,event_id:=kp_id]
  setcolorder(results.all,c(new_rows,names(results.all)[!names(results.all) %in% new_rows]))
  setcolorder(results.all,c(names(results.all)[!names(results.all) %in% 'ends_scheduled'],'ends_scheduled'))
  results.all[,ends_scheduled.opp:=NULL]
  
  write.csv(results.all,outfile,row.names = F,na = 'NULL')
  cat (paste0("****added ",nrow(results.all)/2," new games.\n"))
  #check if event is in db - get id or add it
  
  #TODO: delete existing games for this event
  q=paste0('delete from curling_games where event_id=',kp_id)
  dbSendStatement(con_local,q)
  
  q=paste0("LOAD DATA LOCAL INFILE '",outfile,"' REPLACE INTO TABLE curling_games
           FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"'
           LINES TERMINATED BY '\n' IGNORE 1 LINES")
  dbSendStatement(con_local,q)
  
}
e=dbDisconnect(con_local)

all_cons <- dbListConnections(MySQL())

for(con in all_cons)
  +  dbDisconnect(con)
