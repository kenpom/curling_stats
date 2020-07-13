#update skips table
q=("REPLACE INTO skips (end_date,team_id,country,skip_id,first_name,last_name)
SELECT DISTINCT
e.end_date end_date, t.team_id team_id,
(CASE WHEN
(t.country IN ('Minnesota' , 'North Carolina', 'Washington', 'Wisconsin', 'Pennsylvania',
'New York', 'Massachusetts', 'Alaska','Michigan', 'North Dakota','Colorado','Ohio'))
THEN 'United States'
WHEN (t.country = 'Korea') THEN 'South Korea'
WHEN (t.country = 'Newfoundland/Labrador') THEN 'Newfoundland'
ELSE t.country
END) country,
(CASE t.skip
WHEN 4 THEN t.id4 WHEN 3 THEN t.id3 WHEN 2 THEN t.id2 WHEN 1 THEN t.id1
ELSE t.id4
END) AS skip_id,
(CASE t.skip
WHEN 4 THEN t.first_name4 WHEN 3 THEN t.first_name3 WHEN 2 THEN t.first_name2 WHEN 1 THEN t.first_name1 ELSE t.first_name4
END) AS first_name,
(CASE t.skip
WHEN 4 THEN t.last_name4 WHEN 3 THEN t.last_name3 WHEN 2 THEN t.last_name2 WHEN 1 THEN t.last_name1 ELSE t.last_name4
END) AS last_name
FROM
((teams t
JOIN games g ON ((t.team_id = g.team_id)))
JOIN events e ON ((g.event_id = e.event_id)))")

library(RMySQL)
con=try(dbConnect(RMySQL::MySQL(),dbname='curling',username="dbmasteruser",password='j)n$#P)xg:5kV|A1r[40r9+?:h`BVBTL',host='ls-22fe63cc95c378303961f16ce9e5e616e5d48bdd.cwyvyahhslya.ca-central-1.rds.amazonaws.com'))
e=dbSendStatement(con,q)

