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



# *****************************************************************************
#  process NPM
# *****************************************************************************

filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0002_MPM01_S99_BK0001_150306.csv"
filename<-"BIKE0002_MPM01_S99_BK0001_150306.csv"
projectid<-"columbiaBike"

filepath<-"X:/projects/columbia_bike/data/client_data/20150710_prepilotdata/PrePilot_01/MicroPEM/BIKE0001_MPM01_S01_150626.csv"
filename<-"BIKE0001_MPM01_S01_150626.csv"
fileinfo<-unlist(stringr::str_split(filename, "_"))
fileinfo<-fileinfo[-length(fileinfo)] # we don't need date
fileinfo<-c(fileinfo, projectid)
metainfilename<-TRUE


process_micropem<-function(filepath, filename, fileinfo,metainfilename){
  
  #filepath<-"X:/projects/columbia_bike/data/client_data/20150205_microPem/UGF320417N_KHC0226.csv"
  # Grab the header information -- n=50 to make sure I have all lines
  # but this includes extra lines
  nonParsed<-readLines(file(filepath, "r"), warn=FALSE, n=50)
  
  
  blanks<-!grepl("[a-z0-9A-Z]", nonParsed)# which are blank lines (those starting with two commas)
  numBlanks<-sum(blanks) # number of blank lines
  nonParsed<-nonParsed[!blanks] # remove blanks
  
  
  
  startRowSensor<-grep("Sensor", nonParsed)
  ventilationRow<-grep("Ventilation", nonParsed)
  endHeader<-max(grep("Date", nonParsed))
  
  
  headinfo1<-sapply(splitHeader(nonParsed, 1,startRowSensor-1), collapseHeader)
  
  # this is the table in the header
  headinfo2<-sapply(splitHeader(nonParsed,startRowSensor+1, ventilationRow), collapseHeader, width=6)
  
  headerinfo<-c(headinfo1, headinfo2)
  
  
  headers1<-c("hdr_filename", "hdr_downloaddate", "hdr_downloadtime",  "hdr_deviceserial", "hdr_datetimehard", "hdr_datetimesoft",
              "hdr_participantid", "hdr_filterid", "hdr_participantwt", "hdr_inletsize","hdr_delay_samp_off", "hdr_systimes")
  
  headers2<-c("hdr_nephelometer", "hdr_temperature", "hdr_humidity", 
              "hdr_inlpressure", "hdr_oripressure", "hdr_flow", "hdr_accelerometer", "hdr_battery", "hdr_ventilation")
  
  
  names(headerinfo)<-c(headers1, headers2)
  
  
  data<-read.csv(filepath, as.is=T, skip=endHeader+numBlanks, header=FALSE)
  names(data)<-c("date", "time", "neph_rhcorrect", "temperature", "rh", "battery", "inlpressure", "oripressure", "flow",
                 "xaxis", "yaxis", "zaxis", "vectorsumcomp", "action")
  
  
  #I'm seeing that they might have a line called "Errored Line"
  data %<>% dplyr::filter(!grepl("Errored Line", date))
  
  # from Ashlinn's code -- if the RH is negative we should not have
  # a valid value for nephelometer. File BIKE0002_MPM02_S99_BK0001_150306
  # has examples for testing
  data$neph_rhcorrect[data$rh < 0 & !is.na(data$rh)]<-NA
  data$rh[data$rh<0]<-NA
  
  # need to add the header info, each as it's own record
  # this is bad form making data global
  #     invisible(mapply(function(x,i){
  #       data[[i]]<<-x
  #     }, headerinfo, names(headerinfo)))
  
  h<-data.frame(matrix(headerinfo, nrow=nrow(data), ncol=length(headerinfo), byrow=TRUE))
  names(h)<-names(headerinfo)
  
  data<-cbind(data, h)
  
  # There are two date types possible 2/2/14 or 2-Feb-14. So here I'm testing
  # if the second "piece" of the date is a number and then proceed accordingly
  # Note 5/28/2015 turns out there are sometimes 2/2/14 and sometimes 2/2/2014
  # need to deal with this.
  
  splitdate<-unlist(strsplit(data$date[1], "-|/"))
  isSecondAnumber<-!is.na(suppressWarnings(as.numeric(splitdate[2])))
  isYear2digits<-nchar(splitdate)[3]==2
  
  if(isSecondAnumber){
    
    ifelse(isYear2digits, dateform<-"%m/%d/%y", dateform<-"%m/%d/%Y")
    data%<>%dplyr::mutate(time=addZero(time,8), date=paste(as.Date(date,dateform), time))%>%
      dplyr::select(-time)%>%
      dplyr::rename(datetime=date)
  }else{
    ifelse(isYear2digits, dateform<-"%d-%b-%y", dateform<-"%d-%b-%Y")
    data%<>%dplyr::mutate(time=addZero(time,8), date=paste(as.Date(date,dateform), time))%>%
      dplyr::select(-time)%>%
      dplyr::rename(datetime=date)
  }
  
  

  
  metadata<-generate_metadata(fileinfo, nrow(data), filename, metainfilename)
  proper_session     <- find_gaps_assign_session(data$datetime)
  metadata$sessionid <- proper_session 
  data<-cbind(data, metadata)
  
  return(data)
}


library(ggplot2)
ggplot(data, aes(datetime, 1))+geom_point()




session<-rep(1:nrow(xxx), times=xxx$length)

data$session<-session

data$gap<-c(x, NA)
data[anaction,c("datetime", "flow", "gap", "session")]

data[1:23,c("datetime","action", "flow", "gap", "session")]


dat


data[1:10,]

# *****************************************************************************
#  process NPM
# *****************************************************************************

filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0001_ABP01_S01_150306.abp"
filename<-"BIKE0001_ABP01_S01_150306.abp"

filepath<-"X:/projects/columbia_bike/data/client_data/20150710_prepilotdata/PrePilot_01/ABPM data/BIKE0001_ABP01_S01_150627.abp"
filename<-"BIKE0001_ABP01_S01_150627.abp"

fileinfo<-unlist(stringr::str_split(filename, "_"))
fileinfo<-fileinfo[-length(fileinfo)] # we don't need date
fileinfo<-c(fileinfo, "columbiaBike")
metainfilename<-TRUE

process_abp<-function(filepath, filename, fileinfo, metainfilename){
  #filepath<-X:/projects/columbia_bike/data/client_data/20150710_prepilotdata/PrePilot_01/ABPM data/BIKE0001_ABP01_S01_150627.abp"
  #filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0001_ABP01_S01_150306.abp"
  rl<-readLines(file(filepath,encoding="UTF-16LE"), warn=FALSE)
  endOfIntro<-max(grep("Unknown line", rl))+1
  endOfFile<-grep("XML", rl)-1
  
  
  ID <- read.csv(filepath, 
                 fileEncoding="UTF-16LE", 
                 header=F, 
                 nrows=endOfIntro, 
                 sep=" ", 
                 stringsAsFactors=F,
                 blank.lines.skip = FALSE) # I kept blank lines so that we have consistency with file
  #visit <- ID[1,1]      # assumes that visit number is entered for first name
  studyid <- ID[1,2]    # assumes that study id is entered for last name
  #session <- ID[2,1]    # assumes that session id is entered for patient id 
  
  cntobs <- as.numeric(ID[endOfIntro,1]) # this is not always in the same place, sometimes it's row 8 and sometimes row 9
  data <- read.csv(filepath, 
                   fileEncoding="UTF-16LE", 
                   header=F, 
                   skip=endOfIntro, 
                   nrows=cntobs,
                   blank.lines.skip = FALSE) # load BP data 
  
  data <- data[,1:7]
  names(data) <- c("hour", "minute", "systolic_bp", "mean_arterial_p", "diastolic_bp", "heart_rate", "event_code")
  
  timestart <-endOfIntro+cntobs
  
  DS <- read.csv(filepath, 
                 fileEncoding="UTF-16LE", 
                 header=F, 
                 skip=timestart, 
                 nrows= cntobs,
                 blank.lines.skip = FALSE)
  
  names(DS) <- c( "month", "day","year", "code")
  data$studyid <- studyid
  
  data <- cbind(data, DS)
  
  
  # addZero is function to pad in helper functions
  data[,c("hour", "minute", "month", "day")]<-
    lapply(data[,c("hour", "minute", "month", "day")],addZero)
  
  
  data$event_code<-as.character(data$event_code)
  data$event_code[data$event_code==""]<-NA
  
  data$datetime <- paste("20", data$year, "-", data$month,"-", data$day," ", data$hour,":", data$minute,":00", sep="")
  #BP$datetime <- ymd_hm(BP$datetime), uses lubridate
  #BP$file <- basename(file)
  data%<>% dplyr::select(datetime, which(!names(data)%in%c("datetime","day", "month", "year", "hour", "minute")))
  
  metadata<-generate_metadata(fileinfo, nrow(data), filename, metainfilename)
  proper_session     <- find_gaps_assign_session(data$datetime)
  metadata$sessionid <- proper_session 
  
  data<-cbind(data, metadata)
  
  return(data)
  
}

"X:/projects/columbia_bike/data/client_data/20150710_prepilotdata/PrePilot_01/ABPM data/BIKE0001_ABP_S01_150626.csv"
                            