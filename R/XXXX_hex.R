#fold <- "/Users/zevross/Gdrive/Projects/columbia_bike/data/client_data/20170402_WAV_folder/record_108680"
#file1 <- "/Users/zevross/Gdrive/Projects/columbia_bike/data/client_data/20170402_WAV_folder/record_108680.zip"
#/Users/zevross/git-repos/sensorDataImport/inst/hexapps/mac/HxConvertSourceFile.app/Contents/MacOS/HxConvertSourceFile
#/Users/zevross/junk/hex/record_108680


#file1 <- "/Users/zevross/git-repos/sensorDataImport/inst/example_data/BIKE0002_HXI01_S01A_150306.zip"

hex_extract_convert <- function(thefile){
  
  # thefile <- file1
  # tmpdir <- "/Users/zevross/junk/hex/record_108680"
  #"/var/folders/67/5936qfdd7fb2rtrxbm19bmnw0000gn/T/RtmpjAEAGw"
  
  os <- .Platform$OS.type
  bn <- basename(thefile)
  bn_folder <- gsub(".zip", "", bn)
  tmpdir <- tempdir()
  tmpdir <- gsub("//", "/", tmpdir)
  unzip(zipfile = thefile, exdir = tmpdir)
  dirs <- list.dirs(tmpdir, recursive = FALSE)
  tmpdir <- dirs[grep("record_", dirs)]

  
  print(paste("Unzipped to", tmpdir))
  
  if(os == 'unix'){
    #system(paste0("inst/hexapps/mac/HxConvertSourceFile.app/Contents/MacOS/HxConvertSourceFile ", "/Users/zevross/junk/hex/record_108680"))
    system(paste0("inst/hexapps/mac/HxConvertSourceFile.app/Contents/MacOS/HxConvertSourceFile ", tmpdir))
  }
  
  if(os == 'windows'){
    system(paste0("inst/hexapps/windows/HxConvertSourceFile.exe ", tmpdir))
  }
  
  # rjsonlite
  info <- jsonlite::fromJSON(paste0(tmpdir, "/info.json")) 
  # info$start_date: "2016-09-15T17:24:47.003906"
  # format(as.POSIXct((info$timestamp), origin = "1970-01-01"), usetz=FALSE)
  startdate <- as.POSIXct(gsub('T' , '', info$start_date), origin = "1970-01-01", tz = "GMT")
  startdate <- as.POSIXct(format(startdate, tz = "America/New_York"))
  
  acceleration_X <- read.csv(paste0(tmpdir, "/acceleration_X.csv"), as.is = TRUE, col.names = c("second", "acceleration_x")) 
  acceleration_Y <- read.csv(paste0(tmpdir, "/acceleration_Y.csv"), as.is = TRUE, col.names = c("second", "acceleration_y"))
  acceleration_Z <- read.csv(paste0(tmpdir, "/acceleration_Z.csv"), as.is = TRUE, col.names = c("second", "acceleration_z")) 
  
  acceleration_X <- hex_average_to_second(acceleration_X)
  acceleration_Y <- hex_average_to_second(acceleration_Y)
  acceleration_Z <- hex_average_to_second(acceleration_Z)
  
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
 
  res <- full_join(acceleration_X, acceleration_Y) %>% 
    full_join(., acceleration_Z) %>% 
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
  
  res
    
}








