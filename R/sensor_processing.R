
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
process_gps<-function(filepath, filename, fileinfo,metainfilename){

    print("In GPS processing function")
    
    #x<-data$tracks[[1]]
    #names(x) # "Morning Ride"
  
    data<-readGPX(filepath)
    data <- as.data.frame(data$tracks)  #  extract the relevant info from the list
    names(data)<-c("longitude", "latitude", "elevation", "datetime")
    
    data%<>%select(datetime, which(!names(data)%in%"datetime"))%>%
      mutate(datetime = gsub("T|Z", " ", datetime))
    
    data <- data %>% mutate(datetime = format(as.POSIXct(datetime, tz = "UTC"), tz = "America/New_York"))
    metadata<-generate_metadata(fileinfo, nrow(data), filename, metainfilename)

    
    #metadata<-repeatFileInfo(fileinfo, nrow(data), filename)
    data<-cbind(data, metadata)
    
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
  
  # the generate metadata function returns the session ID from the filename
  # which is fine for most files. But for micropem and abp we need to determine
  # session from gaps in the time series.
  
  proper_session     <- find_gaps_assign_session(data$datetime)
  metadata$sessionid <- proper_session 
  
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

process_micropem<-function(filepath, filename, fileinfo,metainfilename){
    #filepath<-"X:/projects/columbia_bike/data/client_data/20150205_microPem/UGF320417N_KHC0226.csv"
    
    # ---- Extract header-related information
  
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
    
    # ---- Get the data without the header
    
    data<-read.csv(filepath, as.is=T, skip=endHeader+numBlanks, header=FALSE)
    names(data)<-c("date", "time", "neph_rhcorrect", "neph_rhcorrect_hr", "temperature", "rh", "battery", "inlpressure", "oripressure", "flow",
                   "xaxis", "yaxis", "zaxis", "vectorsumcomp", "shut_down_reason",
                   "wearing_compliance", "validity_wearing_compliance")
    
    
    #I'm seeing that they might have a line called "Errored Line"
    data %<>% dplyr::filter(!grepl("Errored Line", date))

    # from Ashlinn's code -- if the RH is negative we should not have
    # a valid value for nephelometer. File BIKE0002_MPM02_S99_BK0001_150306
    # has examples for testing
    data$neph_rhcorrect[data$rh < 0 & !is.na(data$rh)]<-NA
    data$rh[data$rh<0]<-NA
    
    # ---- make header info into data.frame
    
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
    
    # the generate metadata function returns the session ID from the filename
    # which is fine for most files. But for micropem and abp we need to determine
    # session from gaps in the time series.
    
    proper_session     <- find_gaps_assign_session(data$datetime)
    metadata$sessionid <- proper_session 
    
    
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

process_microaeth<-function(filepath, filename, fileinfo,metainfilename){
  
  
  headinfo<-read.csv(filepath,as.is=T, nrow=30, header=FALSE)
  
  # this is the old format
  if(headinfo$V1[1] == "AethLabs"){
    
    manufacturer<-headinfo[1,1]
    
    vals<-sapply(trim(strsplit(headinfo[2:5,1], "=")), "[[", 2)
    
    deviceid<-vals[1]
    softwareV<-vals[2]
    flowrate<-vals[3]
    freq<-vals[4]
    startdate    <- NA
    starttime    <- NA
    origdtform   <- NA
    origtmform   <- NA
    flowunit     <- NA
    pcbtempunit  <- NA
    batteryunit  <- NA
    bcunit       <- NA
    
    dateformat<- "%m/%d/%y"
    toskip <- 7
  }
  
  if(headinfo$V1[1] == "Sep = "){
    
    manufacturer <- headinfo[3,1]
    
    vals<-sapply(trim(strsplit(headinfo[4:15,1], "=")), "[[", 2)
    
    deviceid     <- vals[1]
    softwareV    <- vals[2]
    flowrate     <- vals[3]
    freq         <- vals[4]
    startdate    <- vals[5]
    starttime    <- vals[6]
    origdtform   <- vals[7]
    origtmform   <- vals[8]
    flowunit     <- vals[9]
    pcbtempunit  <- vals[10]
    batteryunit  <- vals[11]
    bcunit       <- vals[12]
    
    
    dateformat<-"%Y/%m/%d"
    toskip <- 16
    
  }
  
  
  
  colNames<-names(read.csv(filepath, as.is=T, nrow=1, skip=toskip))
  #BC should be last column
  colNames<-tolower(colNames[1:which(colNames=="BC")])
  data<-read.csv(filepath, as.is=T, skip=toskip+2, header=FALSE)
  
  data%<>%dplyr::select(1:length(colNames)) # don't comment
  
  names(data)<-colNames
  
  names(data)[grep("date.yyyy.mm.dd.", names(data))]<-"date"
  names(data)[names(data)=="temp"]<-"pcbtemp"
  names(data)[names(data)=="pcb.temp"]<-"pcbtemp"
  
  data%<>%dplyr::mutate(time=addZero(time,8), date=paste(as.Date(date,dateformat), time))%>%
    dplyr::select(-time)%>%
    dplyr::rename(datetime=date)
  
  
  data$hdr_deviceid <-deviceid
  data$hdr_appv     <- softwareV
  data$hdr_flowrate <- flowrate
  data$hdr_timebase <- freq
  data$hdr_startdate<-startdate
  data$hdr_starttime <- starttime
  data$hdr_origdateform<- origdtform
  data$hdr_origtimeform<-origtmform
  data$hdr_flowunit<-flowunit
  data$hdr_pcbtempunit<-pcbtempunit
  data$hdr_batteryunit <- batteryunit
  data$hdr_bcunit<-bcunit
  
  
  metadata<-generate_metadata(fileinfo, nrow(data), filename, metainfilename)
  data<-cbind(data, metadata)
  
    return(data)

  
}


# -----------------------------------------------------------------------------
# Process hexoskin file
# -----------------------------------------------------------------------------

#' This was the original function for processing the hexoskin and it is replaced
#' by a function that uses the binary file
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
process_hexoskin_old<-function(filepath, filename, fileinfo,metainfilename){
  

  data<-read.csv(filepath,as.is=T, check.names=FALSE)
  
  # names are there but followed by a space and junk
  m<-regexpr("[^ ]*", names(data), perl=TRUE)
  names(data)<-regmatches(names(data), m)
  
  data$timestamp<-format(as.POSIXct((data$timestamp)/256, origin = "1970-01-01"), usetz=FALSE)
  names(data)[names(data)=="timestamp"]<-"datetime"
  
  if(!"sleep_position"%in%names(data)) data$sleep_position <- NA
  
  # put them in order
  data <- select(data, datetime, breathing_rate, heart_rate,   minute_ventilation,
                 cadence, sleep_position, activity)
  
  
  metadata<-generate_metadata(fileinfo, nrow(data), filename, metainfilename)
  data<-cbind(data, metadata)
    
    return(data)
  
}


#' Process hexoskin data
#' 
#' This function uses the utilities provided by the makers of hexoskin to convert
#' the binary files in the ZIP to CSVs. Then we combine the individual pieces into
#' one file
#' 
#' @family postgresql functions
#' @param dbname the database.
#' @param host database host, usually 'localhost'
#' @return Nothing
#' @examples
#' xyz
#' @export
#' 
process_hexoskin <- function(filepath, filename, fileinfo,metainfilename){

  os <- .Platform$OS.type
  origdir <- tempdir()
  origdir <- gsub("//", "/", origdir)
  tmpdir <- origdir
  preUnzip <- list.dirs(tmpdir, recursive = FALSE)
  unzip(zipfile = filepath, exdir = tmpdir)
  dirs <- list.dirs(tmpdir, recursive = FALSE)
  dirs <- dirs[!dirs%in%preUnzip]
  tmpdir <- dirs[!grepl("__", dirs)]
  
  if(length(tmpdir ) != 1) stop("there is an issue with the path to the hex data")
  
  
  print(paste("Unzipped to", tmpdir))
  
  info <- jsonlite::fromJSON(paste0(tmpdir, "/info.json")) 
  #info$decoder_version <- "0.5.3"
  #cat(jsonlite::toJSON(info, auto_unbox = TRUE), file = paste0(tmpdir, "/info.json"))
  
  if(os == 'unix'){
    #system(paste0("inst/hexapps/mac/HxConvertSourceFile.app/Contents/MacOS/HxConvertSourceFile ", "/Users/zevross/junk/hex/record_108680"))
    system(paste0("../../hexapps/mac/HxConvertSourceFile.app/Contents/MacOS/HxConvertSourceFile ", tmpdir))
  }
  

  if(os == 'windows'){
    system(paste0("../../hexapps/windows/HxConvertSourceFile.exe ", tmpdir))
  }
  
  # rjsonlite

  startdate <- as.POSIXct(gsub('T' , '', info$start_date), origin = "1970-01-01", tz = "GMT")
  startdate <- as.POSIXct(format(startdate, tz = "America/New_York"))
  
  acceleration_x <- read.csv(paste0(tmpdir, "/acceleration_X.csv"), as.is = TRUE, col.names = c("second", "acceleration_x")) 
  acceleration_y <- read.csv(paste0(tmpdir, "/acceleration_Y.csv"), as.is = TRUE, col.names = c("second", "acceleration_y"))
  acceleration_z <- read.csv(paste0(tmpdir, "/acceleration_Z.csv"), as.is = TRUE, col.names = c("second", "acceleration_z")) 
  
  acceleration_x <- hex_average_to_second(acceleration_x)
  acceleration_y <- hex_average_to_second(acceleration_y)
  acceleration_z <- hex_average_to_second(acceleration_z)
  
  activity <- read.csv(paste0(tmpdir, "/activity.csv"), as.is = TRUE, col.names = c("second", "activity")) 
  breathing_rate <- read.csv(paste0(tmpdir, "/breathing_rate.csv"), as.is = TRUE, col.names = c("second", "breathing_rate")) 
  cadence <- read.csv(paste0(tmpdir, "/cadence.csv"), as.is = TRUE, col.names = c("second", "cadence"))
  device_position <- read.csv(paste0(tmpdir, "/device_position.csv"), as.is = TRUE, col.names = c("second", "device_position")) 
  device_position <- device_position[device_position$second!=0,]
  
  heart_rate <- read.csv(paste0(tmpdir, "/heart_rate.csv"), as.is = TRUE, col.names = c("second", "heart_rate")) 
  minute_ventilation_adjusted <- read.csv(paste0(tmpdir, "/minute_ventilation_adjusted.csv"), as.is = TRUE, col.names = c("second", "minute_ventilation_adjusted")) 
  minute_ventilation_raw<- read.csv(paste0(tmpdir, "/minute_ventilation_raw.csv"), as.is = TRUE, col.names = c("second", "minute_ventilation_raw")) 
  tidal_volume_adjusted <- read.csv(paste0(tmpdir, "/tidal_volume_adjusted.csv"), as.is = TRUE, col.names = c("second", "tidal_volume_adjusted")) 
  tidal_volume_raw <- read.csv(paste0(tmpdir, "/tidal_volume_raw.csv"), as.is = TRUE, col.names = c("second", "tidal_volume_raw")) 
  
  res <- full_join(acceleration_x, acceleration_y) %>% 
    full_join(., acceleration_z) %>% 
    full_join(., activity) %>% 
    full_join(., breathing_rate) %>% 
    full_join(., cadence) %>% 
    full_join(., device_position) %>% 
    full_join(., heart_rate) %>% 
    full_join(., minute_ventilation_adjusted) %>% 
    full_join(., minute_ventilation_raw) %>% 
    full_join(., tidal_volume_adjusted) %>% 
    full_join(., tidal_volume_raw) 
  
  
  res$datetime <- startdate + res$second
  
  data <- dplyr::select(res, datetime, acceleration_x, acceleration_y, acceleration_z,
                 activity, breathing_rate, cadence, device_position,
                 heart_rate, minute_ventilation_adjusted, minute_ventilation_raw,
                 tidal_volume_adjusted, tidal_volume_raw)
  
  metadata<-generate_metadata(fileinfo, nrow(data), filename, metainfilename)
  data<-cbind(data, metadata)
  
  #unlink(origdir, recursive = TRUE, force = TRUE)
  return(data)
  
}




# *****************************************************************************
# Process pdr ---------------------------
# *****************************************************************************

#' Script for processing the PDR data
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
#' 
process_pdr<-function(filepath, filename, fileinfo,metainfilename){
  
  #filepath<-"X:/projects/columbia_bike/data/client_data/20150610_pdr_data/1192_P6.CSV"
  
  nonParsed <- readLines(file(filepath, "r"), warn=FALSE, n=30)
  endHeader <- grep("Logged Data:", nonParsed)
  begData   <- grep("Point", nonParsed)
  
  headerinfo  <- nonParsed[1:(endHeader-1)]
  headerinfo  <- matrix(unlist(strsplit(headerinfo, ": ")), ncol=2, byrow=T) # note colon and space not just colon
  
  headers<-c("hdr_serial", "hdr_userid","hdr_tagnum",
             "hdr_numlogged", "hdr_start", 
             "hdr_elapsed", "hdr_logperiod", "hdr_calibration",
             "hdr_maxdispconc", "hdr_timemax", "hdr_maxstelconc",
             "hdr_timemaxstel", "hdr_avgconc")
  
  headerinfo<-gsub("^\\s+|\\s+$", "", headerinfo[,2])
  names(headerinfo)<-headers
  
  data<-read.csv(filepath, skip=begData, header=FALSE)
  names(data) <- c("point", "date", "time", "avg_mg3" )
  
  # need to figure out year since it's not included
  #head(strptime(paste(data$date, "2001", data$time), "%e %b %Y %T"))
  
  
  
  
  h<-data.frame(matrix(headerinfo, nrow=nrow(data), ncol=length(headerinfo), byrow=TRUE))
  names(h)<-headers
  
  data <- cbind(data, h)
  
  #metadata<-generate_metadata(fileinfo, nrow(data), filename, metainfilename)
  #data<-cbind(data, metadata)
  
  
}

