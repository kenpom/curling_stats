#biggest upsets - run curling_elo.R first

p1=merge(gms,tms[,.(Team2=team,elo2=elo)],'Team2')
p2=merge(p1,tms[,.(Team1=team,elo1=elo)],'Team1')
p2[,p:=elo.prob(elo1,elo2)]
p2[result==1 & Season==2020][order(p)]
p2[result==0 & Season==2020][order(-p)]

#field strength

