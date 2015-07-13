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
  "abp" = c(),
  "gps" = c(),
  "hxi" = c("instrumentid"),
  "mae" = c(),
  "mpm" = c("rh"),
  "pdf" = c()
  
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



