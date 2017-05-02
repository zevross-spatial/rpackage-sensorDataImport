# ****************************************************
# MAE
# ****************************************************

mae<-get_sensor_data("mae", clean_first=FALSE) 

# Plot 1: bc also include run time

mae1 <- filter_add_runtime(mae, "bc")
sensor_ggplot(mae1, "bc", "Microaeth BC")


# Plot 2


mae1 <- filter_add_runtime(mae, "atn")
sensor_ggplot(mae1, "atn", "Microaeth ATN")


# ****************************************************
# MPM
# ****************************************************

mpm <- get_sensor_data("mpm", clean_first = FALSE)

# Plot 1 - (RH-Corrected Nephelometer HR)

mpm1 <- filter_add_runtime(mpm, "neph_rhcorrect_hr")
sensor_ggplot(mpm1, "neph_rhcorrect_hr", "RH-Corrected Nephelometer HR")

# Plot 2 - 

mpm1 <- filter_add_runtime(mpm, "vectorsumcomp")
sensor_ggplot(mpm1, "vectorsumcomp", "Vector Sum Composite (activity)")

# ****************************************************
# hxi
# ****************************************************

hxiA <- get_sensor_data("hxi", clean_first = FALSE, do_aggregate = TRUE,
                       aggregation_unit = "1 min", xtravars = NULL,
                       summarize_vars = c("activity", "minute_ventilation"))

hxiB <- get_sensor_data("hxi", clean_first = FALSE)
# Plot 1 - (RH-Corrected Nephelometer HR)

hxi1 <- filter_add_runtime(hxiA, "activity_avg")
sensor_ggplot(hxi1, "activity_avg", "Activity (1 minute average)")

# Plot 2 - 
# CAREFUL THAT THIS IS CORRECT THEY ASK FOR MINUTE VENTILATION ADJUSTED
# THIS MUST BE IN THE BINARY FILES
hxi1 <- filter_add_runtime(hxiA, "minute_ventilation_avg")
sensor_ggplot(hxi1, "minute_ventilation_avg", "Minute ventilation (1 minute average)")


# Plot 3: 

hxi1 <- filter_add_runtime(hxiB, "heart_rate")
sensor_ggplot(hxi1, "heart_rate", "Heart rate")

# ****************************************************
# GPS
# ****************************************************


gpsDat <- get_sensor_data("gps", clean_first = FALSE)
# Plot 1 - (RH-Corrected Nephelometer HR)

gps1 <- filter_add_runtime(gpsDat, "longitude")
sensor_ggplot(gps1, "longitude", "Longitude (as a surrogate for non-missing data)")



# get_sensor_data <- function(tablename, 
#                             do_aggregate = FALSE,
#                             clean_first = FALSE,
#                             aggregation_unit="15 min",
#                             xtravars = "all",
#                             summarize_vars = NULL, 
#                             grouping_vars = c("subjectid", "sessionid")

forplot_filter_add_runtime <- function(dat, thevar){
  # dat <- hxiA
  # thevar <- "activity_avg"
  if(!any(names(dat)=="datetime") && any(names(dat)=="begin")) 
    dat$datetime <- dat$begin
  dat <- dat[!is.na(dat[[thevar]]),]
  dat <- group_by(dat, sessionid) %>% 
    mutate(runtime = paste("(runtime of", 
                           round(max(datetime)-min(datetime),1), "hours)"),
           firstdate = as.Date(min(datetime)))
  dat$sessionid <- paste0("Session ", trimws(dat$sessionid), ", ", dat$firstdate,
                         " ", dat$runtime)
  dat
}

sensor_ggplot <- function(dat, var, title){
  if(!any(names(dat)=="datetime") && any(names(dat)=="begin")) 
    dat$datetime <- dat$begin
  ggplot(dat, aes_string("datetime", var)) + geom_point() + 
    ggtitle(title) +
    facet_wrap(~sessionid, scales = "free", ncol = 1)
}



ggsave(filename = "/Users/zevross/git-repos/sensorDataImport.wiki/images/mae3.png",
       dpi = 72, width = 6, height = 4)
