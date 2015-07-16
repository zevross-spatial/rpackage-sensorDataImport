get_connection("columbiaBike","spatial",  host="localhost",
               port=5433, user="postgres")


# get the hex data at one minute
# this might take some time
hex <- get_sensor_data("hxi",
                            do_aggregate     = TRUE,
                            aggregation_unit = "1 min",
                            xtravars         = NULL,
                            summarize_vars   = c("breathing_rate", "heart_rate", "minute_ventilation", "cadence"),
                            grouping_vars    = c("subjectid", "sessionid"))




# careful, no compliance or HEPA correction applied
micropem <- get_sensor_data("mpm",
                            do_aggregate     = TRUE,
                            aggregation_unit = "1 min",
                            xtravars         = NULL,
                            summarize_vars   = c("temperature", "flow", "neph_rhcorrect"),
                            grouping_vars    = c("subjectid", "sessionid"))



gps<- get_sensor_data("gps",
                      do_aggregate     = TRUE,
                      aggregation_unit = "1 min",
                      xtravars         = NULL,
                      summarize_vars   = c("latitude", "longitude"),
                      grouping_vars    = c("subjectid", "sessionid"))


# for the pre-pilot data the dates/times do not seem to correspond
# so let's add date to make it easier to see where they match

library(dplyr)
hex <- mutate(hex, date = format(begin, "%Y-%m-%d"))
micropem <- mutate(micropem, date = format(begin, "%Y-%m-%d"))
gps <- mutate(gps, date = format(begin, "%Y-%m-%d"))


# take a look at unique combos

select(hex, date, subjectid, sessionid) %>% distinct
select(micropem, date, subjectid, sessionid) %>% distinct
select(gps, date, subjectid, sessionid) %>% distinct


# in theory this would work -- but in the pre-pilot testing data 
# there is not a lot of matching data


hex_pem <- inner_join(hex, micropem, by=c("subjectid", "sessionid",  "begin", "end"))
hex_gps <- inner_join(hex, gps, by=c("subjectid","sessionid",  "begin", "end"))
pem_gps <- inner_join(micropem, gps, by=c("subjectid", "sessionid",  "begin", "end"))

pem_gps <- mutate(pem_gps, date = date.y) %>% 
  select(-c(interval.x, interval.y, date.x))
  
library(ggplot2)
library(ggmap)
qmap("George Washington Bridge, NYC", zoom = 11, color = "bw", legend = "topleft")+
  geom_point(data=pem_gps, aes(longitude_avg, latitude_avg, color=temperature_avg))+
  scale_color_gradient(low="darkkhaki", high="darkgreen")






