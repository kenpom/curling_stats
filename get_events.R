#get event numbers from curlingzone.com
library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(RMySQL)

get_events = function (season,gender) {
  months=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  if (gender=='Men') {code=81} else if (gender=='Women') {code=82} else if (gender=='Junior Men') {code=83} else {code=84}
  url=paste0('https://www.curlingzone.com/schedule.php?eventyear=',season,'&et=',code)
  xmlFile=getURL(url)
  ids=str_match_all(xmlFile,"event.php\\?task=Event&view=Main&eventid=([0-9][0-9][0-9][0-9])")[[1]][,2]
  ids=as.numeric(ids)
  dates=str_match_all(xmlFile,'<td class=scheduledate .+?>(.+?)</td>')[[1]][,2]
  names=str_match_all(xmlFile,'event.php\\?task=Event&view=Main&eventid=[0-9][0-9][0-9][0-9]>(.+?)</a>')[[1]][,2]
  names=sub('(.+) *-* [Pp]resented by .+','\\1',names)
  events=data.table(ids,names,dates)
  
  #parse begin date and end date
  events[,c('start','end'):=tstrsplit(dates,'-')]
  events[,start:=trimws(start)]
  events[,end:=trimws(end)]
  events[,c('start_month','start_day'):=tstrsplit(start,' ')]
  events[,c('end_month','end_day'):=tstrsplit(end,' ')]
  events[is.na(end_day),':='(end_day=end_month,end_month=start_month)]
  events[,start_month:=match(start_month,months)]
  events[,end_month:=match(end_month,months)]
  events[,start_year:=ifelse(start_month<=5,season,season-1)]
  events[,end_year:=ifelse(end_month<=5,season,season-1)]
  events[,start_date:=sprintf("%4d-%02d-%02d",start_year,as.numeric(start_month),as.numeric(start_day))]
  events[,end_date:=sprintf("%4d-%02d-%02d",end_year,as.numeric(end_month),as.numeric(end_day))]
  events=events[ids!=6100]
  return(events[,.(season,event_id=ids,event_name=names,start_date,end_date,gender,tour=NA_character_)])
}
