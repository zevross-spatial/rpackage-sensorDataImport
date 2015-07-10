#' @import dplyr
#' @import XML

#' @importFrom magrittr %<>%
#' @export
NULL

#' @importFrom data.table foverlaps
#' @export
NULL


#' @importFrom data.table setDT
#' @export
NULL


#' @importFrom data.table :=
#' @export
NULL


# *****************************************************************************
# Test file prefix -----
# *****************************************************************************

#' Make sure all the user-selected files have the right prefix.
#' 
#' @param prefixes are the prefixes of files
#' @param filenames the full list of filenames the user uploaded
#' @param stage is the stage of processing ("processing", "uploading",
#' "pre-screening" right now.)
#' @return user.
#' @examples
#' prefixes_ok(c("GPS", "AB"))
#' @export

filename_ok<-function(filenames, projectid){


  
  bad_filenames <- switch(projectid,
         "columbiaBike" = filename_ok_biking(filenames))
  
  # return T/F for if there is a problem, then list of problem
  # filenames
  return(list(is.null(bad_filenames), bad_filenames))
  
}



# *****************************************************************************
# Test file prefix -----
# *****************************************************************************

#' Make sure all the user-selected files have the right prefix.
#' 
#' @param prefixes are the prefixes of files
#' @param filenames the full list of filenames the user uploaded
#' @param stage is the stage of processing ("processing", "uploading",
#' "pre-screening" right now.)
#' @return user.
#' @examples
#' prefixes_ok(c("GPS", "AB"))
#' @export

filename_ok_biking<-function(filenames){
  
  #filenames<-c("BIKE0001_GPS01_S01_150306.gpx", "BIKE0001_ABP01_sdf_150306.gpx", "BIKE0001_ABP01_S01_150306.gpx")
  
  req_length <- 4
  req_prefix <- c("GPS", "ABP", "MAE", "MPM", "HXI")
  
  filenames_split<-strsplit(filenames, "_")
  
  # test consistent file name length
  l <- sapply(filenames_split, length)
  bad_length <- which(l!=req_length)
  
  
  # test prefix
  prefix<-substring(sapply(filenames_split, "[[",2),1,3)
  bad_prefix <- which(!prefix%in%req_prefix)


  bad_tot <- unique(c(bad_length, bad_prefix))
  if(length(bad_tot)==0) return(NULL)
  

  return(filenames[bad_tot])
  
  
}





# *****************************************************************************
# Test file prefix -----
# *****************************************************************************

#' Make sure all the user-selected files have the right prefix.
#' 
#' @param prefixes are the prefixes of files
#' @param filenames the full list of filenames the user uploaded
#' @param stage is the stage of processing ("processing", "uploading",
#' "pre-screening" right now.)
#' @return user.
#' @examples
#' prefixes_ok(c("GPS", "AB"))
#' @export

prefixes_ok<-function(prefixes, allowed=c("GPS", "ABP", "MAE", "MPM", "HXI")){

  
  tst<-prefixes%in%allowed
  return(tst)
  
  
  
  
}


# *****************************************************************************
# Data processing or upload error report for Shiny
# *****************************************************************************

#' Report assemble the error report about which files were processed, uploaded
#' or screened
#' 
#' @param currentfile_num is the number in sequence of the file where
#' an error occurred
#' @param filenames the full list of filenames the user uploaded
#' @param stage is the stage of processing ("processing", "uploading",
#' "pre-screening" right now.)
#' @return user.
#' @examples
#' error_report(1, c("file1_abc", "file2_abc"), stage="uploading")
#' @export

error_report<-function(currentfile_num, filenames, stage){
  i<-currentfile_num
  cur<-filenames[i]
  
  if(currentfile_num==1){
    completed<-" No files uploaded successfully."
    notcompleted<-""
    
  }else{

    complete<-filenames[1:(i-1)]
    notcomplete<-filenames[i:length(filenames)]
    completed<-paste(" Files ", 
                     paste(complete, collapse=", "), 
                     " uploaded successfully.", sep="")
    
    notcompleted<-paste(" Files ", 
                        paste(notcomplete, collapse=", "), 
                        " NOT UPLOADED.", sep="")

  }
  
  
  msg<-paste("There is a problem with file ", 
             cur, 
             ".", 
             " Error occurred in the ", stage, " step.",
             completed,
             notcompleted, sep="")

  
  if(stage=="filename screening") msg<-paste(msg, "The file", cur, "seems to have been uploaded previously.")
  
  return(msg)
}




# *****************************************************************************
# parseFileName
# *****************************************************************************

#' xxt
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
#' 
initiate_processing<-function(filepath, filename, projectid, metainfilename){
  
  # this is metadata and each element of fileinfo will become a column
  # where each row has the same value.
  
  
  if(projectid=="columbiaBike") fileinfo<-unlist(stringr::str_split(filename, "_"))
  if(projectid!="columbiaBike") fileinfo<-c("abc", "def", "ghi", "jkl")
  fileinfo<-fileinfo[-length(fileinfo)] # we don't need date
  fileinfo<-c(fileinfo, projectid)
  
  filetype<-substring(fileinfo[2],1,3)

  
  
  process_result<-switch(filetype,
                         GPS = process_gps(filepath, filename, fileinfo, metainfilename),
                         ABP = process_abp(filepath, filename, fileinfo, metainfilename),
                         MAE = process_microaeth(filepath, filename, fileinfo, metainfilename),
                         MPM = process_micropem(filepath, filename, fileinfo, metainfilename),
                         HXI = process_hexoskin(filepath, filename, fileinfo, metainfilename))
  

      return(process_result)
  
}


# *****************************************************************************
# Allow autoincrement field and date added
# *****************************************************************************

# in order to write the tables to the postgresql database the dbWriteTable works
# fine but when I want an autoincrement serial field and a auto date/time field
# for when data was added I needed a hack like this. The dbWriteTable uses the
# function postgresqlWriteTable so I'm creating a new functino called 
# postgresqlWriteTableAlt that takes care of it. Copied from stackoverflow

#http://bit.ly/1E0Vsf6

#' xxt
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
body_lines <- deparse(body(RPostgreSQL::postgresqlWriteTable))
new_body_lines <- sub(
  'postgresqlTableRef(name), "FROM STDIN")', 
  'postgresqlTableRef(name), "(", paste(shQuote(names(value)), collapse = ","), ") FROM STDIN")', 
  body_lines,
  fixed = TRUE
)
postgresqlWriteTableAlt <- RPostgreSQL::postgresqlWriteTable
body(postgresqlWriteTableAlt) <- parse(text = new_body_lines)

# *****************************************************************************
# parseFileName
# *****************************************************************************

#' xxt
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
addZero<-function(dat,width=2){
  stringr::str_pad(dat, width=width, side="left", pad="0")
}



# *****************************************************************************
# is error
# *****************************************************************************

#http://adv-r.had.co.nz/Exceptions-Debugging.html

#' xxt
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
is.error <- function(x) inherits(x, "try-error")


# *****************************************************************************
# repeatFileInfo
# *****************************************************************************

# we are adding columns to the tables in the database with the metadata so
# we need to metadata repeated for each record in the table

#' A function to create a matrix of the metadata extracted from the file
#' names for adding to a sensor table.
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
generate_metadata<-function(fileinfo, n, filename, metainfilename){
  
  writeLines(paste("Metadata in filename: ", metainfilename))
  filetype<-substring(fileinfo[2],1,3)
  
  # the sessionid should not include the "S"
  fileinfo[3]<-gsub("S", "", fileinfo[3])
  
  l<-length(fileinfo)
  fin<-data.frame(matrix(fileinfo, nrow=n, ncol=l, byrow=TRUE))
  if(l==4) names(fin)<-c("subjectid", "instrumentid", "sessionid", "projectid")
  if(l==5) names(fin)<-c("subjectid", "instrumentid", "sessionid", "filterid", "projectid")
  
  fin$filename<-filename
  
  
  return(fin)
  
}


# *****************************************************************************
# splitHeader and collapseHeader -- for dealing with MicroPEM headers
# *****************************************************************************

#' xxt
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
splitHeader<-function(dat, beg, end){
  strsplit(dat[beg:end], ",")  
}


#' Collapses header info into one string separated by pipes (|)
#' this is done because we don't want each piece of the header
#' to be its own variable.
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
collapseHeader<-function(dat, width=NA){
  #dat<-strsplit(nonParsed[[14]], ",")[[1]]
  #dat<-tmp[[6]]
  #width<-6
  #width<-NA
  if(is.na(width)){
    dat<-dat[dat!=""][-1]
    if(all(dat=="")) dat<-"NA"
  }else{
    dat[dat==""]<-"NA"
    dat<-dat[2:(width+1)]
  }
  dat<-paste(dat, collapse="|")
  
  dat
}



# *****************************************************************************
# GPS
# *****************************************************************************

#' Read GPX files [http://www.topografix.com/gpx.asp] and convert the data to tables;
#' this function was created by Tomislav Hengl with contributions from Dylan Beaudette
#' and Pierre Roudier
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export


readGPX <- function (gpx.file, metadata = TRUE, bounds = TRUE, waypoints = TRUE, 
          tracks = TRUE, routes = TRUE) 
{
  options(warn = -1)
  if (metadata == TRUE) {
    metadata <- readGPXelement(gpx.file, "name")
  }
  if (bounds == TRUE) {
    bounds <- readGPXelement(gpx.file, "bounds")
  }
  if (waypoints == TRUE) {
    waypoints <- readGPXelement(gpx.file, "wpt")
  }
  if (tracks == TRUE) {
    tracks <- readGPXelement(gpx.file, "trk")
  }
  if (routes == TRUE) {
    routes <- readGPXelement(gpx.file, "rte")
  }
  gpx <- list(metadata = metadata, bounds = bounds, waypoints = waypoints, 
              tracks = tracks, routes = routes)
  return(gpx)
}




# *****************************************************************************
# GPS
# *****************************************************************************

#' Read GPX files [http://www.topografix.com/gpx.asp] and convert the data to tables;
#' this function was created by Tomislav Hengl with contributions from Dylan Beaudette
#' and Pierre Roudier
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export



readGPXelement<-function (gpx.file, element) 
{
  ret <- xmlTreeParse(gpx.file, useInternalNodes = TRUE)
  top <- xmlRoot(ret)
  if (any(grep(element, names(top)))) {
    if (element == "trk") {
      ret <- NULL
      nu <- which(names(top) %in% element)
      for (c in seq_along(nu)) {
        lst <- which(names(top[[nu[c]]]) %in% "trkseg")
        nm <- names(top[[nu[c]]][[lst[1]]][[1]])
        ret[[c]] <- list(NULL)
        for (i in seq_along(lst)) {
          trkpt <- top[[nu[c]]][[lst[i]]]
          ret[[c]][[i]] <- data.frame(NULL)
          lon <- as.numeric(xmlSApply(trkpt, xmlGetAttr, 
                                      "lon"))
          lat <- as.numeric(xmlSApply(trkpt, xmlGetAttr, 
                                      "lat"))
          ret[[c]][[i]][1:length(lon), "lon"] <- lon
          ret[[c]][[i]][1:length(lat), "lat"] <- lat
          if (!nm[[1]] == "NULL") {
            for (j in 1:length(nm)) {
              xm <- as.character(sapply(sapply(xmlChildren(trkpt), 
                                               function(x) x[[nm[[j]]]]), xmlValue))
              ret[[c]][[i]][1:length(xm), nm[[j]]] <- xm
            }
          }
        }
        names(ret[[c]]) <- xmlValue(top[[nu[c]]][["name"]])
      }
    }
    if (element == "wpt") {
      ret <- data.frame(NULL)
      nu <- which(names(top) %in% element)
      nm <- names(top[[nu[1]]])
      for (i in seq_along(nu)) {
        ret[i, "lon"] <- as.numeric(xmlGetAttr(top[[nu[i]]], 
                                               "lon"))
        ret[i, "lat"] <- as.numeric(xmlGetAttr(top[[nu[i]]], 
                                               "lat"))
        if (!nm[[1]] == "NULL") {
          for (j in 1:length(nm)) {
            ret[i, nm[[j]]] <- xmlValue(xmlChildren(top[[nu[i]]])[[nm[[j]]]])
          }
        }
      }
    }
    if (element == "rte") {
      ret <- NULL
      nu <- which(names(top) %in% element)
      for (c in seq_along(nu)) {
        ret[[c]] <- data.frame(NULL)
        lst <- which(names(top[[nu[c]]]) %in% "rtept")
        nm <- names(top[[nu[c]]][[lst[1]]])
        for (i in seq_along(lst)) {
          rtept <- top[[nu[c]]][[lst[i]]]
          ret[[c]][i, "lon"] <- as.numeric(xmlGetAttr(rtept, 
                                                      "lon"))
          ret[[c]][i, "lat"] <- as.numeric(xmlGetAttr(rtept, 
                                                      "lat"))
          if (!nm[[1]] == "NULL") {
            for (j in c("name", "cmt", "desc", "sym", 
                        "type")) {
              try(ret[[c]][i, j] <- xmlValue(rtept[[j]]), 
                  silent = TRUE)
            }
          }
        }
        names(ret)[c] <- xmlValue(top[[nu[c]]][["name"]])
      }
    }
    if (element == "bounds") {
      nu <- which(names(top) %in% element)
      ret <- matrix(rep(NA, 4), nrow = 2, dimnames = list(c("lat", 
                                                            "lon"), c("min", "max")))
      ret[1, 1] <- as.numeric(xmlGetAttr(top[[nu[1]]], 
                                         "minlon"))
      ret[1, 2] <- as.numeric(xmlGetAttr(top[[nu[1]]], 
                                         "maxlon"))
      ret[2, 1] <- as.numeric(xmlGetAttr(top[[nu[1]]], 
                                         "minlat"))
      ret[2, 2] <- as.numeric(xmlGetAttr(top[[nu[1]]], 
                                         "maxlat"))
    }
    if (element == "name") {
      lst <- c("name", "desc", "author", "email", "url", 
               "urlname", "time")
      nu <- which(names(top) %in% lst)
      if (!nu[[1]] == "NULL") {
        ret <- data.frame(NULL)
        for (i in seq_along(lst)) {
          try(ret[1, lst[i]] <- xmlValue(top[[nu[[i]]]]), 
              silent = TRUE)
        }
      }
    }
  }
  else {
    ret <- NULL
  }
  return(ret)
}



