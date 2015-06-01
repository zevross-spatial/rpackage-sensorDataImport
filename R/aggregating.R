library(devtools)
library(dplyr)
load_all()
get_connection("columbiaBike","spatial",  host="localhost",
   port=5433, user="postgres")


afunction(thetable, aggregation_unit=c("min", "hour", "day"))
thetable<-tbl(.connection, "hxi")


aggregation_unit<-"day"
agg_form<-switch(aggregation_unit,
       day  = 'YYYY-MM-DD',
       hour = 'YYYY-MM-DD HH24',
       min  = 'YYYY-MM-DD HH24:MI')

q_1<-paste0("group_by(thetable, subjectid, sessionid, to_char(datetime,'", agg_form, "'))")
q_2<-"summarize(breathing_avg = mean(breathing_rate),breathing_cnt = n())"

q_all<-paste(grp, s, sep="%>%")

x<-eval(parse(text=q_all))










# junk
bymin<-
  byhour<-'YYYY-MM-DD HH24'
byday<-
  
  x<-group_by(thetable, subjectid, sessionid, to_char(datetime,bymin)) %>%
  summarize(breathing_avg = mean(breathing_rate)),breathing_cnt = n())


x<-group_by(thetable, subjectid, sessionid, DATE_PART("year", datetime)) %>%
  summarize(breathing_avg = mean(breathing_rate),
            breathing_cnt = n())

EXTRACT(MONTH FROM datetime)

eval(parse(text="mean(1:10)"))
