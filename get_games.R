get_games = function (event_id) {
  #event_id=id
  url=paste0('https://www.curlingzone.com/event.php?eventid=',event_id,'&view=Scores&showall=1')
  xmlFile=getURL(url)
  x=gsub('.+<div class="table-responsive">(.+)','\\1',xmlFile)
  hmr=str_match_all(x,"\\s+<td align=center class=linescorehammer>(.+?)</td>")
  hmr=hmr[[1]][,2]
  hmr[grepl('hammer',hmr)]='H'
  hmr=sub('&nbsp;','',hmr)
  
  tms=str_match_all(x,paste0('<a class="linescoreteamlink" href="event.php\\?view=Team&eventid=\\d+&teamid=(\\d+)'))
  tms=tms[[1]][,2]
  
  if (identical(tms,character(0))) {return("error")}
  
  scores=str_match_all(x,'<td align=center class=linescoreend>(.+)</td>')
  scores=scores[[1]][,2]
  scores=str_match_all(scores,'&nbsp;(.?)&nbsp;')
  
  finals=str_match_all(x,'<td .+ class=linescorefinal>&nbsp;<b>(\\d+)</b>&nbsp;')
  finals=finals[[1]][,2]
  
  if (length(finals)==0 | length(finals)<length(scores)) {
    print ("fewer finals than scores" )
    return("error")}

  ends=str_match_all(x,'<td width=25 align=center class=linescorehead>(.+)</td>')
  ends=ends[[1]][,2]
  ends=str_match_all(ends,'<b>(.[\\dE]*)</b>')
  
  tot=length(tms)
  lines=data.table(game_num=rep(1:ceiling(tot/2),each=2),tms,hmr,E1=rep('X',each=tot),E2=rep('X',each=tot),E3=rep('X',each=tot),E4=rep('X',each=tot),
                   E5=rep('X',each=tot),E6=rep('X',each=tot),E7=rep('X',each=tot),E8=rep('X',each=tot),
                   E9=rep('X',each=tot),E10=rep('X',each=tot),EE=rep('X',each=tot),score=finals)
  for (i in 1:length(scores)) {
    j=ceiling(i/2)
    ee=which(ends[[j]][,2]=='EE')
    e=max(as.numeric(ends[[j]][,2][ends[[j]][,2]!='EE']))
    line=transpose(as.list(scores[[i]][,2]))[[1]]
    if (e>=1) {lines[i,E1:=line[1]]}
    if (e>=2) {lines[i,E2:=line[2]]}
    if (e>=3) {lines[i,E3:=line[3]]}
    if (e>=4) {lines[i,E4:=line[4]]}
    if (e>=5) {lines[i,E5:=line[5]]}
    if (e>=6) {lines[i,E6:=line[6]]}
    if (e>=7) {lines[i,E7:=line[7]]}
    if (e>=8) {lines[i,E8:=line[8]]}
    if (e>=9) {lines[i,E9:=line[9]]}
    if (e>=10) {lines[i,E10:=line[10]]}
    if (length(ee)>0) {lines[i,EE:=sum(as.numeric(line[ee]))]}
    lines[i,ends:=as.numeric(e)]
  }
  lines[is.na(lines)]='X'
  SDcols=names(lines)[grepl('^E',names(lines))]
  lines[, (SDcols) := lapply(.SD, function(x) sub("^$", NA, x)),.SDcols=SDcols]
  out=merge(lines,lines,'game_num',suffixes = c('','_opp'))
  out=out[tms!=tms_opp]
  out[,index:=pmin(tms,tms_opp)]
  out[,n:=1:.N,c('game_num','index')]
  out=out[n==1]
  out[,c('ends','n','index'):=NULL]
  setnames(out,'ends_opp','ends')

  #set hammers
  #figure out hammer for each end
  out[,H1:=ifelse(hmr=='H',1,0)]
  out[,H2:=ifelse(E1=='X','X',ifelse(E1==E1_opp,H1,ifelse(E1>0,0,1)))]
  out[,H3:=ifelse(E2=='X','X',ifelse(E2==E2_opp,H1,ifelse(E2>0,0,1)))]
  out[,H4:=ifelse(E3=='X','X',ifelse(E3==E3_opp,H1,ifelse(E3>0,0,1)))]
  out[,H5:=ifelse(E4=='X','X',ifelse(E4==E4_opp,H1,ifelse(E4>0,0,1)))]
  out[,H6:=ifelse(E5=='X','X',ifelse(E5==E5_opp,H1,ifelse(E5>0,0,1)))]
  out[,H7:=ifelse(E6=='X','X',ifelse(E6==E6_opp,H1,ifelse(E6>0,0,1)))]
  out[,H8:=ifelse(E7=='X','X',ifelse(E7==E7_opp,H1,ifelse(E7>0,0,1)))]
  out[,H9:=ifelse(E8=='X','X',ifelse(E8==E8_opp,H1,ifelse(E8>0,0,1)))]
  out[,H10:=ifelse(E9=='X','X',ifelse(E9==E9_opp,H1,ifelse(E9>0,0,1)))]
  
  out[,HEE:=NA_character_]
  out[ends==6 & EE!='X',HEE:=ifelse(E6==E6_opp,H6,ifelse(E6>0,0,1))]
  out[ends==8 & EE!='X',HEE:=ifelse(E8==E8_opp,H8,ifelse(E8>0,0,1))]
  out[ends==10 & EE!='X',HEE:=ifelse(E10==E10_opp,H10,ifelse(E10>0,0,1))]
  out[is.na(HEE),HEE:='X']
  out[,event_id:=event_id]
  setcolorder(out,c('event_id',names(out)[names(out)!='event_id']))
  out[,c('hmr','hmr_opp'):=NULL]
  out[, names(out) := lapply(.SD, function(x) gsub("X", NA, x))]
  return(out)
}

