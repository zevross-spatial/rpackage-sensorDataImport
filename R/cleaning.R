# *****************************************************************************
# Get database connection ---------------------------
# *****************************************************************************

#' A function that lists the variables that will be needed in the cleaning step
#' @family xxx
#' @param tablename the table to be cleaned
#' @return variables needed for cleaning
#' @examples
#' cleaning_vars("hxi")
#' @export

cleaning_vars<-function(tablename){
  
switch(tablename,
  "abp" = c("sessionid"),
  "gps" = c("sessionid"),
  "hxi" = c("sessionid"),
  "mae" = c("sessionid"),
  "mpm" = c("sessionid"),
  "pdf" = c("sessionid")
  
)
  
}




# *****************************************************************************
# Get database connection ---------------------------
# *****************************************************************************

#' A function that lists the variables that will be needed in the cleaning step
#' @family xxx
#' @param tablename the table to be cleaned
#' @return variables needed for cleaning
#' @examples
#' cleaning_vars("hxi")
#' @export

clean_data<-function(tablename, dat){
  
  dat$sessionid <- trim(dat$sessionid)
  dat <- filter(dat, sessionid != "Non session")

  
  switch(tablename,
         "abp" = clean_abp(tablename, dat),
         "gps" = clean_gps(tablename, dat),
         "hxi" = clean_hxi(tablename, dat),
         "mae" = clean_mae(tablename, dat),
         "mpm" = clean_mpm(tablename, dat),
         "pdr" = clean_pdr(tablename, dat)
         
  )
  
}


# *****************************************************************************
# Clean abp data---------------------------
# *****************************************************************************

#' A function that cleans HXI data
#' @family xxx
#' @param tablename the table to be cleaned
#' @param data to be cleaned
#' @return cleaned data
#' @examples
#' cleaning_abp("abp", dat)
#' @export

clean_abp<-function(tablename, dat){
  

  print("I'm cleaning ABP data")
  
  return(dat)
  
}



# *****************************************************************************
# Clean gps data---------------------------
# *****************************************************************************

#' A function that cleans HXI data
#' @family xxx
#' @param tablename the table to be cleaned
#' @param data to be cleaned
#' @return cleaned data
#' @examples
#' cleaning_gps("gps", dat)
#' @export

clean_gps<-function(tablename, dat){
  
  
  print("I'm cleaning GPS data")
  
  return(dat)
  
}



# *****************************************************************************
# Clean hxi data---------------------------
# *****************************************************************************

#' A function that cleans HXI data
#' @family xxx
#' @param tablename the table to be cleaned
#' @param data to be cleaned
#' @return cleaned data
#' @examples
#' cleaning_hxi("hxi", dat)
#' @export

clean_hxi<-function(tablename, dat){
  
  
  print("I'm cleaning HXI data")
  
  return(dat)
  
}



# *****************************************************************************
# Clean mae data---------------------------
# *****************************************************************************

#' A function that cleans HXI data
#' @family xxx
#' @param tablename the table to be cleaned
#' @param data to be cleaned
#' @return cleaned data
#' @examples
#' cleaning_mae("mae", dat)
#' @export

clean_mae<-function(tablename, dat){
  
  
  print("I'm cleaning MAE data")
  
  return(dat)
  
}



# *****************************************************************************
# Clean mae data---------------------------
# *****************************************************************************

#' A function that cleans MPM data
#' @family xxx
#' @param tablename the table to be cleaned
#' @param data to be cleaned
#' @return cleaned data
#' @examples
#' cleaning_mae("mae", dat)
#' @export

clean_mpm<-function(tablename, dat){
  
#   # TODO: I believe we will need to allow the user to set interactively
#   compliance_threshold <- 0.02
#   
#   # This is from Ashlinn's code
#   #### ADDING COMPLIANCE CRITERIA ###
#   
#   active.minute.average$sd_composite_above_threshold <- 
#     ifelse(active.minute.average$Vector.Sum.Composite_SD > compliance_threshold, 1, 0)
#   
#   active.minute.average$sd_x_above_threshold <- 
#     ifelse(active.minute.average$X.axis_SD > compliance_threshold, 1, 0)	
#   
#   active.minute.average$sd_y_above_threshold <- 
#     ifelse(active.minute.average$Y.axis_SD > compliance_threshold, 1, 0) 
#   
#   active.minute.average$sd_z_above_threshold <- 
#     ifelse(active.minute.average$Z.axis_SD > compliance_threshold, 1, 0) 
#   
#   ## **** NOTE **** To change the width of the rolling mean window 
#   # for compliance, change the parameter for "width" w. 
#   
#   active.minute.average$sd_composite_rollmean <- 
#     as.numeric(rollapply(active.minute.average$sd_composite_above_threshold, 
#                          width=window_width,  FUN = mean, align = "center", 
#                          na.rm = TRUE, fill = NA))  
#   
#   active.minute.average$compliance_rollmean <- 
#     ifelse(active.minute.average$sd_composite_rollmean > 0, 1, 0)
#   
#   if (sum(!is.na(active.minute.average$compliance_rollmean)) > 0) {
#     
#     active.minute.average.complete <-  
#       active.minute.average[complete.cases(active.minute.average),] 
#     
#   } else {
#     
#     active.minute.average.complete <- 
#       active.minute.average
#     
#   }
#   
  
  
  print("I'm cleaning MPM data")
  
  
  
  
  return(dat)
  
}



# *****************************************************************************
# Clean pdr data---------------------------
# *****************************************************************************

#' A function that cleans HXI data
#' @family xxx
#' @param tablename the table to be cleaned
#' @param data to be cleaned
#' @return cleaned data
#' @examples
#' cleaning_pdr("pdr", dat)
#' @export

clean_pdr<-function(tablename, dat){
  
  
  print("I'm cleaning PDR data")
  
  return(dat)
  
}



