
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
  
  dat <- data.frame(a = 1:10, b = 1:10)
  p <- ggplot(dat, aes(a, b)) + geom_point() + ggtitle(tablename)
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
  res1 <- filter(dat, !is.na(neph_rhcorrect_hr))
  p1 <- ggplot(res1, aes(datetime, neph_rhcorrect_hr)) + geom_point() +
    ggtitle("MPM RH-Corrected Nephelometer HR")

  res2 <- filter(dat, !is.na(vectorsumcomp))
  p2 <- ggplot(res2, aes(datetime, vectorsumcomp)) + geom_point() + 
    ggtitle("MPM vector sum comp")
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



