
# *****************************************************************************
# Utility function for plotting ---------------------------
# *****************************************************************************

#' Filters out NA based on a variable and adds runtime and date to the 
#' session ID so that these occur in the plot strip
#' @family xxx
#' @param dat input data from the database
#' @param thevar the variable that will be used for NA filtering
#' @return new dataset with no NA for the var and new sessionid
#' @examples
#' xxxx

forplot_filter_add_runtime <- function(dat, thevar){

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


# *****************************************************************************
# Utility function for plotting ---------------------------
# *****************************************************************************

#' Create a sensor ggplot with a panel for each session
#' @family xxx
#' @param dat input data from the database
#' @param var the variable that will be used for NA filtering
#' @param title the plot title
#' @return a ggplot
#' @examples
#' xxxx

sensor_ggplot <- function(dat, var, title){
  if(!any(names(dat)=="datetime") && any(names(dat)=="begin")) 
    dat$datetime <- dat$begin
  ggplot(dat, aes_string("datetime", var)) + geom_point() + 
    ggtitle(title) +
    facet_wrap(~sessionid, scales = "free", ncol = 1)
}




# *****************************************************************************
# QA/QC, select plotting function ---------------------------
# *****************************************************************************

#' A function that lists the variables that will be needed in the plotting step
#' @family xxx
#' @param tablename the table to be ploted
#' @return variables needed for plotting
#' @examples
#' plotting_vars("hxi")
#' @export

plot_qaqc<-function(tablename, dat, save = TRUE, savepath = ""){

  dat$datetime <- as.POSIXct(dat$datetime)
  p <- switch(tablename,
         "abp" = plot_abp(tablename, dat),
         "gps" = plot_gps(tablename, dat),
         "hxi" = plot_hxi(tablename, dat),
         "mae" = plot_mae(tablename, dat),
         "mpm" = plot_mpm(tablename, dat),
         "pdr" = plot_pdr(tablename, dat)
         
  )
  
  if(save){
    # do save
  }
  
  p
  
}


# *****************************************************************************
# Plot abp data---------------------------
# *****************************************************************************

#' A function that plots HXI data
#' @family xxx
#' @param tablename the table to be ploted
#' @param data to be ploted
#' @return ploted data
#' @examples
#' plotting_abp("abp", dat)
#' @export

plot_abp<-function(tablename, dat){
  
  dat <- data.frame(a = 1:10, b = 1:10)
  p <- ggplot(dat, aes(a, b)) + geom_point() + ggtitle(tablename)
  print("I'm plotting ABP data")
  
  return(p)
  
}



# *****************************************************************************
# Plot gps data---------------------------
# *****************************************************************************

#' A function that plots HXI data
#' @family xxx
#' @param tablename the table to be ploted
#' @param data to be ploted
#' @return ploted data
#' @examples
#' plotting_gps("gps", dat)
#' @export

plot_gps<-function(tablename, dat){
  gpsDat <- get_sensor_data("gps", clean_first = FALSE)
  # Plot 1 - (RH-Corrected Nephelometer HR)
  
  gps1 <- filter_add_runtime(gpsDat, "longitude")
  p <-sensor_ggplot(gps1, "longitude", "Longitude (as a surrogate for non-missing data)")
  
  print("I'm plotting GPS data")
  
  return(p)
  
}



# *****************************************************************************
# Plot hxi data---------------------------
# *****************************************************************************

#' A function that plots HXI data
#' @family xxx
#' @param tablename the table to be ploted
#' @param data to be ploted
#' @return ploted data
#' @examples
#' plotting_hxi("hxi", dat)
#' @export

plot_hxi<-function(tablename, dat){
  
  
  # res1 <- filter(res, !is.na(activity))
  # res1$sessionid <- paste("Session", trimws(res1$sessionid))
  # ggplot(res1, aes(datetime, activity)) + geom_point() + 
  #   ggtitle("HXI activity") +
  #   facet_wrap(~sessionid, scales = "free", ncol = 1)
  
  # res1 <- filter(res, !is.na(minute_ventilation))
  # res1$sessionid <- paste("Session", trimws(res1$sessionid))
  # ggplot(res1, aes(datetime, minute_ventilation)) + geom_point() + 
  #   ggtitle("HXI minute_ventilation") +
  #   facet_wrap(~sessionid, scales = "free", ncol = 1)
  
  dat <- data.frame(a = 1:10, b = 1:10)
  p <- ggplot(dat, aes(a, b)) + geom_point() + ggtitle(tablename)
  print("I'm plotting HXI data")
  
  return(p)
  
}



# *****************************************************************************
# Plot mae data---------------------------
# *****************************************************************************

#' A function that plots HXI data
#' @family xxx
#' @param tablename the table to be ploted
#' @param data to be ploted
#' @return ploted data
#' @examples
#' plotting_mae("mae", dat)
#' @export

plot_mae<-function(tablename, dat){
  
  # res1 <- filter(res, !is.na(bc))
  # ggplot(res1, aes(datetime, bc)) + geom_point() + 
  #   ggtitle("MPM RH-Corrected Nephelometer HR") +
  #   facet_wrap(~sessionid, scales = "free", ncol = 1)
  
  dat <- data.frame(a = 1:10, b = 1:10)
  p <- ggplot(dat, aes(a, b)) + geom_point() + ggtitle(tablename)
  print("I'm plotting MAE data")
  
  return(p)
  
}



# *****************************************************************************
# Plot mae data---------------------------
# *****************************************************************************

#' A function that plots MPM data
#' @family xxx
#' @param tablename the table to be ploted
#' @param data to be ploted
#' @return ploted data
#' @examples
#' plotting_mae("mae", dat)
#' @export

plot_mpm<-function(tablename, dat){


  print("I'm plotting MPM data")
  
  mpm <- get_sensor_data("mpm", clean_first = FALSE)
  
  # Plot 1 - (RH-Corrected Nephelometer HR)
  
  mpm1 <- filter_add_runtime(mpm, "neph_rhcorrect_hr")
  p1 <- sensor_ggplot(mpm1, "neph_rhcorrect_hr", "RH-Corrected Nephelometer HR")
  
  # Plot 2 - 
  
  mpm1 <- filter_add_runtime(mpm, "vectorsumcomp")
  p2 <- sensor_ggplot(mpm1, "vectorsumcomp", "Vector Sum Composite (activity)")
  
  # res1 <- filter(dat, !is.na(neph_rhcorrect_hr))
  # p1 <- ggplot(res1, aes(datetime, neph_rhcorrect_hr)) + geom_point() +
  #   ggtitle("MPM RH-Corrected Nephelometer HR")
  # 
  # res2 <- filter(dat, !is.na(vectorsumcomp))
  # p2 <- ggplot(res2, aes(datetime, vectorsumcomp)) + geom_point() + 
  #   ggtitle("MPM vector sum comp")
  print("I'm done plotting MPM data")



  return(grid.arrange(p1, p2 ,ncol = 1))
  
}



# *****************************************************************************
# Plot pdr data---------------------------
# *****************************************************************************

#' A function that plots HXI data
#' @family xxx
#' @param tablename the table to be ploted
#' @param data to be ploted
#' @return ploted data
#' @examples
#' plotting_pdr("pdr", dat)
#' @export

plot_pdr<-function(tablename, dat){
  
  dat <- data.frame(a = 1:10, b = 1:10)
  p <- ggplot(dat, aes(a, b)) + geom_point() + ggtitle(tablename)
  print("I'm plotting PDR data")
  
  return(p)
  
}



