
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
#' plotting_hxi("hxi", dat)
#' @export

forplot_filter_add_runtime <- function(dat, thevar){
  #dat <- hxiA
  if(!any(names(dat)=="datetime") && any(names(dat)=="begin")) dat$datetime <- dat$begin
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
#' plotting_hxi("hxi", dat)
#' @export

sensor_ggplot <- function(dat, var, title){
  # dat <- hxiA
  # var <- "activity_avg"
  # title <- "a"
  if(!any(names(dat)=="datetime") && any(names(dat)=="begin")){dat$datetime <- dat$begin}
  
  ggplot(dat, aes_string("datetime", var)) + geom_point(color = "blue", alpha = 0.75) + 
    ggtitle(title) +
    facet_wrap(~sessionid, scales = "free", ncol = 1) +
    theme(
      strip.text = element_text(size = rel(1.3), face = "bold")
    )
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

    if(class(p)[1] == "list"){
      for(i in 1:length(p)){
        
        ggsave(paste0(savepath, "/plot_", tools::file_path_sans_ext(dat$filename[1]), "_", i, ".png"), 
               p[[i]], 
               width = 6, height = 4, units = "in")
      }
    }else{
      ggsave(paste0(savepath, "/plot_", tools::file_path_sans_ext(dat$filename[1]), "_", ".png"), 
             p, 
             width = 6, height = 4, units = "in")
    }
    
    

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
  
  gps1 <- forplot_filter_add_runtime(gpsDat, "longitude")
  p <-sensor_ggplot(gps1, "longitude", "GPS: Longitude (as a surrogate for non-missing data)")
  
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
  
  print("I'm plotting HXI data")

  hxiA<- aggregate_data(dat,
                        aggregation_unit = "1 min",
                        summarize_vars = c("activity", "minute_ventilation_adjusted"),
                        grouping_vars = c("subjectid", "sessionid"))
  
  hxiB <- dat
  # Plot 1 - (RH-Corrected Nephelometer HR)

  hxi1 <- forplot_filter_add_runtime(hxiA, "activity_avg")
  p1 <- sensor_ggplot(hxi1, "activity_avg", "Hexoskin: Activity (1 minute average)")
  
  # Plot 2 - 
  # CAREFUL THAT THIS IS CORRECT THEY ASK FOR MINUTE VENTILATION ADJUSTED
  # THIS MUST BE IN THE BINARY FILES
  hxi1 <- forplot_filter_add_runtime(hxiA, "minute_ventilation_adjusted_avg")
  p2 <- sensor_ggplot(hxi1, "minute_ventilation_adjusted_avg", "Hexoskin: Minute ventilation adjusted (1 minute average)")
  
  
  # Plot 3: 
  
  hxi1 <- forplot_filter_add_runtime(hxiB, "heart_rate")
  p3 <- sensor_ggplot(hxi1, "heart_rate", "Hexoskin: Heart rate")
  
  
  return(list(p1, p2, p3))
  #return(grid.arrange(p1, p2, p3, ncol = 1))
  
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
  
  
  
  # Plot 1: bc also include run time
  
  mae1 <- forplot_filter_add_runtime(dat, "bc")
  p1 <- sensor_ggplot(mae1, "bc", "Microaeth: BC")
  
  
  # Plot 2
  
  
  mae1 <- forplot_filter_add_runtime(dat, "atn")
  p2 <- sensor_ggplot(mae1, "atn", "Microaeth: ATN")
  
  return(list(p1, p2))
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
  
  # Plot 1 - (RH-Corrected Nephelometer HR)
  
  mpm1 <- forplot_filter_add_runtime(dat, "neph_rhcorrect_hr")
  p1 <- sensor_ggplot(mpm1, "neph_rhcorrect_hr", "MicroPEM: RH-Corrected Nephelometer HR")
  
  # Plot 2 - 
  
  mpm1 <- forplot_filter_add_runtime(dat, "vectorsumcomp")
  p2 <- sensor_ggplot(mpm1, "vectorsumcomp", "MicroPEM: Vector Sum Composite (activity)")
  
  print("I'm done plotting MPM data")
  
  
  return(list(p1, p2))
  #return(grid.arrange(p1, p2 ,ncol = 1))
  
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



