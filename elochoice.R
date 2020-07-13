library(EloChoice)
g=games[Season>=2019]
x=elochoice(g$Team1,g$Team2,kval=30,startvalue = 1500,runs=50)

y=ratings(x,drawplot = F,show='mean')
teams=names(y)
y=data.table(y)
y[,tms:=teams]
tm_names=rbindlist(list(games.orig[,.(team1,first_name1,last_name1)],
                        games.orig[,.(team2,first_name2,last_name2)]),use.names = F)
tm_names=unique(tm_names)
y[,tms:=as.integer(tms)]
y=merge(y,tm_names,by.x='tms',by.y='team1')
dec=x$ov
tms=as.numeric(rownames(dec))
dec=data.table(dec)
dec[,tms:=tms]
y=merge(y,dec,'tms')
y=y[order(-y)]
View(y)
