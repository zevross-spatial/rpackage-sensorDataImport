#' testdplyr
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export

testdplyr<-function(){
  
  a<-select(mtcars, contains("g"))
  
  a%>%filter(gear==4)
}

#' XXX
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
processGPS<-function(filepath, filename, fileinfo){
  #filepath<-"X:/projects/columbia_bike/bikeStats/shiny/test_input_data/BIKE0001_GPS01_S01_150306.gpx"
  #fileinfo
  writeLines("This is a GPS file, I'm processing...")
  
  prePGtry<-try({
    
    
    data<-plotKML::readGPX(filepath)
    data <- as.data.frame(data$tracks)  #  extract the relevant info from the list
    names(data)<-c("longitude", "latitude", "elevation", "datetime")
    
    data%<>%select(datetime, which(!names(data)%in%"datetime"))%>%
      mutate(datetime = gsub("T|Z", " ", datetime))
    
    
    metadata<-repeatFileInfo(fileinfo, nrow(data))
    data<-cbind(data, metadata)
    
  }, silent=TRUE)
  
  if(is.error(prePGtry)) return(list(prePGtry, "prePGerror"))
  
  writeLines(paste("Processing successful now uploading", nrow(data), "rows to database"))
  PGtry<-try(postgresqlWriteTableAlt(.connection$con, "gps", data, append=TRUE, row.names=FALSE), silent=TRUE)
  
  
  if(is.error(PGtry)){
    return(list(PGtry, "PGerror"))
  }else{
    writeLines(paste("Processing and upload complete\n"))
    return(list("Fine", "Fine"))
  }
  
  
}


#' XXX
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
processABP<-function(filepath, filename, fileinfo){
  
  writeLines("This is a ABP file, I'm processing and uploading to database...")
  
  # process file
  #filepath<-"X:/projects/columbia_bike/bikeStats/shiny/test_input_data/abp_spacelabs.abp"
  
  # darby hard-coded the last line in the header. This is probably fine but for
  # now it seems that the last occurrence of "Unknown line" is the end of the
  # begining of file. Be careful here since readLine could take awhile with big files
  prePGtry<-try({
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
    
    metadata<-repeatFileInfo(fileinfo, nrow(data))
    
    data<-cbind(data, metadata)
    
  }, silent=TRUE)
  
  if(is.error(prePGtry)) return(list(prePGtry, "prePGerror"))
  
  # export to PostgreSQL
  #write.csv(BP, "X:/projects/columbia_bike/data/processed_data/sample_tables/ambulatory_blood_pressure.csv", row.names=FALSE)
  writeLines(paste("Processing successful now uploading", nrow(data), "rows to database"))
  PGtry<-try(postgresqlWriteTableAlt(.connection$con, "abp", data, append=TRUE, row.names=FALSE), silent=TRUE)
  
  if(is.error(PGtry)){
    return(list(PGtry, "PGerror"))
  }else{
    writeLines(paste("Processing and upload complete\n"))
    return(list("Fine", "Fine"))
  }
  
}




#' XXX
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export

processMicroPEM<-function(filepath, filename, fileinfo){
  #filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0003_MPM01_S99_BK0001_150306.csv"
  #filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0002_MPM02_S99_BK0001_150306.csv"
  #fileinfo<-unlist(str_split("BIKE0003_MPM01_S99_BK0001_150306.csv", "_"))
  writeLines("This is a microPEM file, I'm processing and uploading to database...")
  
  prePGtry<-try({
    
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
    
    # need to add the header info, each as it's own record
    invisible(mapply(function(x,i){
      data[[i]]<<-x
    }, headerinfo, names(headerinfo)))
    
    # There are two date types possible 2/2/14 or 2-Feb-14. So here I'm testing
    # if the second "piece" of the date is a number and then proceed accordingly
    isSecondAnumber<-!is.na(suppressWarnings(as.numeric(unlist(strsplit(data$date[1], "-|/"))[2])))
    
    if(isSecondAnumber){
      data%<>%dplyr::mutate(time=addZero(time,8), date=paste(as.Date(date,"%m/%d/%y"), time))%>%
        dplyr::select(-time)%>%
        dplyr::rename(datetime=date)
    }else{
      data%<>%dplyr::mutate(time=addZero(time,8), date=paste(as.Date(date,"%d-%b-%y"), time))%>%
        dplyr::select(-time)%>%
        dplyr::rename(datetime=date)
    }
    
    metadata<-repeatFileInfo(fileinfo, nrow(data))
    data<-cbind(data, metadata)
    
  }, silent=TRUE)
  
  if(is.error(prePGtry)) return(list(prePGtry, "prePGerror"))
  
  #write.csv(data[1:1000,], "X:/projects/columbia_bike/data/processed_data/sample_tables/micropem.csv", row.names=FALSE)
  writeLines(paste("Processing successful now uploading", nrow(data), "rows to database"))
  PGtry<-try(postgresqlWriteTableAlt(.connection$con, "mpm", data, append=TRUE, row.names=FALSE), silent=TRUE)
  
  if(is.error(PGtry)){
    return(list(PGtry, "PGerror"))
  }else{
    writeLines(paste("Processing and upload complete\n"))
    return(list("Fine", "Fine"))
  }
}


#' XXX
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export

processMicroAeth<-function(filepath, filename, fileinfo){
  #library(magrittr)
  #library(dplyr)
  #filepath<-"X:/projects/columbia_bike/bikeStats/shiny/test_input_data/uAe1_PROBLEM.csv"
  #filepath<-"X:/projects/columbia_bike/bikeStats/shiny/test_input_data/uAe1.csv"
  #filepath<-"X:/projects/columbia_bike/data/client_data/20141001_sample_data_files/655_20140710_MicroAeth_Data.csv"
  writeLines("This is a microAeth file, I'm processing and uploading to database...")
  
  prePGtry<-try({
    
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
    
    metadata<-repeatFileInfo(fileinfo, nrow(data))
    data<-cbind(data, metadata)
    
  }, silent=TRUE)
  
  if(is.error(prePGtry)) return(list(prePGtry, "prePGerror"))
  # export to PostgreSQL
  #write.csv(data, "X:/projects/columbia_bike/data/processed_data/sample_tables/microaeth.csv", row.names=FALSE)
  writeLines(paste("Processing successful now uploading", nrow(data), "rows to database"))
  PGtry<-try(postgresqlWriteTableAlt(.connection$con, "mae", data, append=TRUE, row.names=FALSE), silent=TRUE)
  
  if(is.error(PGtry)){
    return(list(PGtry, "PGerror"))
  }else{
    writeLines(paste("Processing and upload complete\n"))
    return(list("Fine", "Fine"))
  }
  
}


# -----------------------------------------------------------------------------
# Process hexoskin file
# -----------------------------------------------------------------------------

#' XXX
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
processHexoskin<-function(filepath, filename, fileinfo){
  
  writeLines("This is a hexoskin file, I'm processing...")
  
  prePGtry<-try({
    #filepath<-"X:/projects/columbia_bike/data/client_data/20141001_sample_data_files/hexoskin/record-55556.csv"
    data<-read.csv(filepath,as.is=T, check.names=FALSE)
    
    # names are there but followed by a space and junk
    m<-regexpr("[^ ]*", names(data), perl=TRUE)
    names(data)<-regmatches(names(data), m)
    
    data$timestamp<-format(as.POSIXct((data$timestamp)/256, origin = "1970-01-01"), usetz=FALSE)
    names(data)[names(data)=="timestamp"]<-"datetime"
    
    metadata<-repeatFileInfo(fileinfo, nrow(data))
    data<-cbind(data, metadata)
    
  }, silent=TRUE)
  
  if(is.error(prePGtry)) return(list(prePGtry, "prePGerror"))
  
  writeLines(paste("Processing successful now uploading", nrow(data), "rows to database"))
  PGtry<-try(postgresqlWriteTableAlt(.connection$con, "hxi", data, append=TRUE, row.names=FALSE), silent=TRUE)
  
  
  if(is.error(PGtry)){
    return(list(PGtry, "PGerror"))
  }else{
    writeLines(paste("Processing and upload complete\n"))
    return(list("Fine", "Fine"))
  }
  
  
}
