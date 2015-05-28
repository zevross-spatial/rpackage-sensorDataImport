#' test dplyr to make sure that dplyr functions and magrittr pipes are included
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


# *****************************************************************************
# Process GPS ---------------------------
# *****************************************************************************

#' xxy
#' 
#' @param filepath is the full filepath with file name
#' @param filename is just the filename (this can't be derived from filepathdue to browser restrictions)
#' @param fileinfo is a vector of values that are treated as 
#' columns and repeated through the whole file - most likely the split filename but can be anything
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
process_gps<-function(filepath, filename, fileinfo){

    print("In GPS processing function")
    data<-plotKML::readGPX(filepath)
    data <- as.data.frame(data$tracks)  #  extract the relevant info from the list
    names(data)<-c("longitude", "latitude", "elevation", "datetime")
    
    data%<>%select(datetime, which(!names(data)%in%"datetime"))%>%
      mutate(datetime = gsub("T|Z", " ", datetime))
    
    
    metadata<-repeatFileInfo(fileinfo, nrow(data), filename)
    data<-cbind(data, metadata)
    
    print("Finished with GPS processing")
    return(data)

  
}


# *****************************************************************************
# Process ABP ---------------------------
# *****************************************************************************

#' xxy
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
process_abp<-function(filepath, filename, fileinfo){
  

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
    
    metadata<-repeatFileInfo(fileinfo, nrow(data), filename)
    
    data<-cbind(data, metadata)
    
    return(data)
  
}


# *****************************************************************************
# Process microPEM ---------------------------
# *****************************************************************************

#' xxy
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export

process_micropem<-function(filepath, filename, fileinfo){

    
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
    
    # need to add the header info, each as it's own record
    invisible(mapply(function(x,i){
      data[[i]]<<-x
    }, headerinfo, names(headerinfo)))
    
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
    
    metadata<-repeatFileInfo(fileinfo, nrow(data), filename)
    data<-cbind(data, metadata)
    
    return(data)
}


# *****************************************************************************
# Process microAeth ---------------------------
# *****************************************************************************

#' xxy
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export

process_microaeth<-function(filepath, filename, fileinfo){

    
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
    
    metadata<-repeatFileInfo(fileinfo, nrow(data), filename)
    data<-cbind(data, metadata)
  
    return(data)

  
}


# -----------------------------------------------------------------------------
# Process hexoskin file
# -----------------------------------------------------------------------------

#' Process hexoskin
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
process_hexoskin<-function(filepath, filename, fileinfo){
  

    #filepath<-"X:/projects/columbia_bike/data/client_data/20141001_sample_data_files/hexoskin/record-55556.csv"
    data<-read.csv(filepath,as.is=T, check.names=FALSE)
    
    # names are there but followed by a space and junk
    m<-regexpr("[^ ]*", names(data), perl=TRUE)
    names(data)<-regmatches(names(data), m)
    
    data$timestamp<-format(as.POSIXct((data$timestamp)/256, origin = "1970-01-01"), usetz=FALSE)
    names(data)[names(data)=="timestamp"]<-"datetime"
    
    metadata<-repeatFileInfo(fileinfo, nrow(data), filename)
    data<-cbind(data, metadata)
    
    return(data)
  
}
