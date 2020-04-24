library(data.table)
library(RMySQL)
con_local=try(dbConnect(RMySQL::MySQL(),dbname='kenpom_1',username="root",password='',host='localhost'))
q='select j.* from curling_games j join curling_events e on j.event_id=e.id where e.ends_scheduled=8'
dt=data.table(dbGetQuery(con_local,q))
e=dbDisconnect(con_local)
dt=dt[!is.na(W)]
#some stats
dt[!is.na(E1),.(sum(E1>0)/.N,.N),H1][order(H1)]
dt[!is.na(E2),.(sum(E2>0)/.N,.N),H2][order(H2)]
dt[!is.na(E3),.(sum(E3>0)/.N,.N),H3][order(H3)]
dt[!is.na(E4),.(sum(E4>0)/.N,.N),H4][order(H4)]
dt[!is.na(E5),.(sum(E5>0)/.N,.N),H5][order(H5)]
dt[!is.na(E6),.(sum(E6>0)/.N,.N),H6][order(H6)]
dt[!is.na(E7),.(sum(E7>0)/.N,.N),H7][order(H7)]
dt[!is.na(E8),.(sum(E8>0)/.N,.N),H8][order(H8)]
dt[!is.na(EE),.(sum(EE>0)/.N,.N),HEE][order(HEE)]

#some stats
#dt[!is.na(E1) & is.na(E1_opp)]
dt[!is.na(E1+E1_opp),.(sum((E1+E1_opp)>0,na.rm=T)/.N,.N)]
dt[!is.na(E2),.(sum((E2+E2_opp)>0)/.N,.N)]
dt[!is.na(E3),.(sum((E3+E3_opp)>0)/.N,.N)]
dt[!is.na(E4),.(sum((E4+E4_opp)>0)/.N,.N)]
dt[!is.na(E5),.(sum((E5+E5_opp)>0,na.rm=T)/.N,.N)]
dt[!is.na(E6),.(sum((E6+E6_opp)>0,na.rm=T)/.N,.N)]
dt[!is.na(E7),.(sum((E7+E7_opp)>0,na.rm=T)/.N,.N)]
dt[!is.na(E8),.(sum((E8+E8_opp)>0,na.rm=T)/.N,.N)]

dt[,E1_diff:=E1-E1_opp]
dt[,E2_diff:=E2-E2_opp]
dt[,E3_diff:=E3-E3_opp]
dt[,E4_diff:=E4-E4_opp]
dt[,E5_diff:=E5-E5_opp]
dt[,E6_diff:=E6-E6_opp]
dt[,E7_diff:=E7-E7_opp]
dt[,E8_diff:=E8-E8_opp]
dt[,EE_diff:=EE-EE_opp]

dt[,E2_mar:=E1_diff]
dt[,E3_mar:=E2_mar+E2_diff]
dt[,E4_mar:=E3_mar+E3_diff]
dt[,E5_mar:=E4_mar+E4_diff]
dt[,E6_mar:=E5_mar+E5_diff]
dt[,E7_mar:=E6_mar+E6_diff]
dt[,E8_mar:=E7_mar+E7_diff]

dt[H1==1,.(sum(W=='W')/.N,.N),.(H1,E1_diff)][order(H1,E1_diff)] #win prob based on 1st end score
dt[H2==1 & E2_mar==-2,.(sum(W=='W')/.N,.N),.(H2,E2_diff)][order(H2,E2_diff)] #win prob based on 1st end score
dt[H3==1 & E3_mar==1,.(sum(W=='W')/.N,.N),.(H3,E3_diff)][order(H3,E3_diff)] #win prob based on 1st end score
dt[H4==1 & E4_mar==1,.(sum(W=='W')/.N,.N),.(H4,E4_diff)][order(H4,E4_diff)] #win prob based on 1st end score
dt[H5==1 & E5_mar==0,.(sum(W=='W')/.N,.N),.(H5,E5_diff)][order(H5,E5_diff)] #win prob based on 1st end score
dt[H6==1 & E6_mar==0,.(sum(W=='W')/.N,.N),.(E6_diff)][order(E6_diff)] #win prob based on 1st end score
dt[H7==1 & E7_mar==0,.(sum(W=='W')/.N,.N),.(E7_diff)][order(E7_diff)] #win prob based on 1st end score
dt[H8==1 & E8_mar==-1,.(sum(W=='W')/.N,.N),.(E8_diff)][order(E8_diff)] #win prob based on 1st end score
dt[H2==1,.(sum(W=='W')/.N,.N),E2_mar][order(E2_mar)] #win prob based on 1st end score

dt[H7==1,.(sum(W=='W')/.N,.N),E3_mar][order(E3_mar)]
dt[H1==1 & E1_diff==1,.N,E2_mar]
dt[H1==0 & E1_diff==1,.N,E2_mar]

#hammer value, end 1
dt[H1==1,.(non_hmr_score_pct=sum(E1_opp>0,na.rm=T)/.N,
           non_hmr_score_avg=sum(E1_opp,na.rm = T)/.N,
           hmr_score_pct=sum(E1>0,na.rm=T)/.N,
           hmr_score_avg=sum(E1,na.rm = T)/.N,.N)]

dt[H6==1 & E6_mar==1,.(sum(W=='W',na.rm=T)/.N,.N)] #first end hammer win pct

stop()

dt[,.(sum(W=='W')/.N,.N),.(H3,E3_diff)][order(H3,E3_diff)]

dt[,.(sum(W=='W')/.N,.N),.(H4,E4_diff)][order(H4,E4_diff)]

dt[H5==1,.(sum(W=='W')/.N,.N),.(H5,E5_diff)][order(H5,E5_diff)]

dt[H6==1,.(sum(W=='W')/.N,.N),.(H6,E6_diff)][order(H6,E6_diff)]

dt[H7==1,.(sum(W=='W')/.N,.N),.(H7,E7_diff)][order(H7,E7_diff)]

dt[H8==1,.(sum(W=='W')/.N,.N),.(H8,E8_diff)][order(H8,E8_diff)]

dt[HEE==1 & !is.na(EE_diff),.(sum(W=='W')/.N,.N)]
