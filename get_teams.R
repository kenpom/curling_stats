#get teams - function to get individual players from cz
get_teams = function (event_id,gender) {
  #event_id=id
  #gender='Men'
  if (gender=='Men') {code=81} else if (gender=='Women') {code=82} else {code=83}
  url=paste0('https://www.curlingzone.com/event.php?eventid=',event_id,'&eventtypeid=',code,'&view=Teams')
  xmlFile=getURL(url)
  
  x=gsub('.+<div class="table-responsive">(.+?)</div>.+','\\1',xmlFile)

  teams=str_match_all(x,"<a class='wctlight_team_text'.+?teamid=(\\d+).+?>(.*?)<p>")
  teams_id=teams[[1]][,2]
  teams=teams[[1]][,3]
  
  if (length(teams)==0) {return("error")}
  
  locations=str_match_all(x,"<a class='wctlight_team_text'.*?>.+gif><br>(.+)</a>")
  locations=locations[[1]][,2]
  locations=gsub("<br>","|",locations)
  locations=gsub("â\u0080\u0099","'",locations)
  locations=gsub(" \\|","|",locations)
  locations=gsub(" / ","/",locations)
  
  positions=str_match_all(x,"<font class=wctlight_player_title>(.+)</font>")
  positions=positions[[1]][,2]
  
  players=str_match_all(x,"<a class='wctlight_player_text' href=player.php\\?playerid=(\\d+)>(.+)</a>")
  ids=players[[1]][,2]
  players=players[[1]][,3]
  players=gsub("<br>","|",players)
  fourths=players[seq(1, length(players), 4)]
  thirds=players[seq(2, length(players), 4)]
  seconds=players[seq(3, length(players), 4)]
  leads=players[seq(4, length(players), 4)]
  
  fourths_id=ids[seq(1, length(players), 4)]
  thirds_id=ids[seq(2, length(players), 4)]
  seconds_id=ids[seq(3, length(players), 4)]
  leads_id=ids[seq(4, length(players), 4)]
  
  teamdata=data.table(tmid=teams_id,tm=trimws(teams),loc=locations,skip=4-(which(positions=='Skip')-1) %% 4,id4=fourths_id,
                      fourth=fourths,id3=thirds_id,third=thirds,id2=seconds_id,
                      second=seconds,id1=leads_id,lead=leads)
  
  teamdata[,c('city','country'):=tstrsplit(loc,"\\|")]
  teamdata[,c('fn1','ln1'):=tstrsplit(lead,"\\|")]
  teamdata[,c('fn2','ln2'):=tstrsplit(second,"\\|")]
  teamdata[,c('fn3','ln3'):=tstrsplit(third,"\\|")]
  teamdata[,c('fn4','ln4'):=tstrsplit(fourth,"\\|")]
  
  SDcols=c('id1','id2','id3','id4')
  teamdata[, (SDcols) := lapply(.SD, function(x) x=ifelse(x==0,NA,x)),.SDcols=SDcols]
  SDcols=c('fn1','fn2','fn3','fn4')
  teamdata[, (SDcols) := lapply(.SD, function(x) x=ifelse(x=='',NA,x)),.SDcols=SDcols]
  teamdata[,city:=ifelse(city=='character(0)',NA,city)]
  teamdata[,country:=ifelse(country=='character(0)',NA,country)]
  #print(teamdata)
  
  return(teamdata[,.(tmid,city,country,skip,id4,fn4,ln4,id3,fn3,ln3,id2,fn2,ln2,id1,fn1,ln1)])
}
