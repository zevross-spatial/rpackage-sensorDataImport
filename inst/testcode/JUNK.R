get_connection("columbiaBike","spatial",  host="localhost",
               port=5433, user="postgres")


x<-process_microaeth("X:/projects/columbia_bike/notes/issues_w_app/PrePilot_01/MicroAeth Data/Ltest1 - 636/", 
"BIKE0001_MAE36_S01_150616.csv", c("abc", "def"))



process_microaeth<-function(filepath, filename, fileinfo,metainfilename){
  
  #filepath<-"X:/projects/columbia_bike/notes/issues_w_app/PrePilot_01/MicroAeth Data/Ltest1 - 636/BIKE0001_MAE36_S01_150616.csv"
  #filename<-"BIKE0001_MAE36_S01_150616.csv"
  #fileinfo<-c("abc", "def")
  headinfo<-read.csv(filepath,as.is=T, nrow=5, header=FALSE)
  manufacturer<-headinfo[1,1]
  deviceid<-headinfo[2,1]
  softwareV<-headinfo[3,1]
  flowrate<-headinfo[4,1]
  freq<-headinfo[5,1]
  
  colNames<-names(read.csv(filepath, as.is=T, nrow=1, skip=7))
  #BC should be last column
  colNames<-tolower(colNames[1:which(colNames=="BC")])
  data<-read.csv(filepath, as.is=T, skip=9, header=FALSE)
  
  data%<>%dplyr::select(1:length(colNames)) # don't comment
  
  names(data)<-colNames
  
  
  
  
  data%<>%dplyr::mutate(time=addZero(time,8), date.yyyy.mm.dd.=paste(as.Date(date.yyyy.mm.dd.,"%m/%d/%y"), time))%>%
    dplyr::select(-time)%>%
    dplyr::rename(datetime=date.yyyy.mm.dd.)
  
  metadata<-generate_metadata(fileinfo, nrow(data), filename, metainfilename)
  data<-cbind(data, metadata)
  
  return(data)
  
  
}



##############################################

x<-aggregate_data(dat, 
                  aggregation_unit="2 hours",
                  grouping_vars = c("subjectid", "sessionid"),
                  summarize_vars=c("cadence", "breathing_rate"))


get_sensor_data <- 

do_aggregate = FALSE,
clean_first = TRUE,
aggregation_unit="15 min",
vars = "datetime",
summarize_vars = NULL, 
grouping_vars = c("subjectid", "sessionid")
                            
                            
                            
res<-get_sensor_data("hxi", clean_first=FALSE, xtravars="all") 
res<-get_sensor_data("hxi", xtravars=c("datetime", "subjectid"))  

res<-get_sensor_data("gps")
res<-get_sensor_data("hxi", 
                   do_aggregate=TRUE, 
                   xtravars= NULL,
                   aggregation_unit="10 min",
                   grouping_vars = c( "subjectid", "sessionid"),
                   summarize_vars=c("cadence", "breathing_rate"))

# this will give you average lat, long (centroid) of each subject/session
res<-get_sensor_data("gps",
                     do_aggregate=TRUE, 
                     xtravars= NULL,
                     aggregation_unit="complete",
                     grouping_vars = c( "subjectid", "sessionid"),
                     summarize_vars=c("latitude", "longitude"))




# gives error, check xtravars
res<-get_sensor_data("hxi", xtravars=c("datetime", "asdfjasf")) #gives error

# gives error check grouping vars
res<-get_sensor_data("hxi", 
                     do_aggregate=TRUE, 
                     xtravars= NULL,
                     aggregation_unit="2 hours",
                     grouping_vars = c("datetime", "subjectid", "asdlfjsdlfj"),
                     summarize_vars=c("cadence", "breathing_rate"))




## filenaming

filenames<-c("BIKE0001_GPS01_S01_150306.gpx", "BIKE0001_ABP01_S01_150306.gpx")
substring(sapply(stringr::str_split(filenames, "_"), "[[",2),1,3)

filenames_split<-strsplit(filenames, "_")

l<-sapply(filenames_split, length)
prefix<-substring(sapply(filenames_split, "[[",2),1,3)


#
curpath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0001_GPS01_S01_150306.gpx"
curfilename<- "BIKE0001_GPS01_S01_150306.gpx"
projectid<-"columbiaBike"
metainfilename<-FALSE


initiate_processing(filepath  = curpath, 
                    filename  = curfilename,
                    projectid = projectid,
                    metainfilename = metainfilename)

                            