#' @import dplyr



#' @importFrom magrittr %<>%
#' @name %<>%
#' @export
#' @usage xxx
NULL

# -----------------------------------------------------------------------------
# getConnection: connect to database
# -----------------------------------------------------------------------------
#' xxx
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
getConnection<-function(){
  # note the double arrow to make global
  .connection<<-try(dplyr::src_postgres(dbname="columbiaBike", host="localhost",
                                        password="spatial", port=5433, user="postgres"),
                    silent=TRUE)
}

#getConnection()

# -----------------------------------------------------------------------------
# parseFileName
# -----------------------------------------------------------------------------
# #' xxx
# #' 
# #' @param sdf
# #' @return user.
# #' @examples
# #' add(1, 1)
# #' add(10, 1)
# #' @export
# identify_filetype<-function(filename){
# 
#   fileinfo<-unlist(stringr::str_split(filename, "_"))
#   filetype <- substring(fileinfo[2], 1,3)
#   return(filetype)
#   
# }


# -----------------------------------------------------------------------------
# Data processing or upload error report for Shiny
# -----------------------------------------------------------------------------
#' xxx
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
error_report<-function(currentfile_num, currentfile_name, completedfile_names, stage){
  
  if(currentfile_num==1){
    completed<-" No files uploaded successfully."
  }else{
    
    completed<-paste(" Files ", 
                     paste(completedfile_names, collapse=", "), 
                     " uploaded successfully.", sep="")
  }
  
  
  
  msg<-paste("There is a problem with file ", 
             currentfile_name, 
             ".", 
             " Error occurred in the ", stage, " step.",
             completed, sep="")
  
  return(msg)
}




# -----------------------------------------------------------------------------
# parseFileName
# -----------------------------------------------------------------------------
#' xxx
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
process_data<-function(filepath, filename, filetype){
  
  # this is metadata
  fileinfo<-unlist(stringr::str_split(filename, "_"))
  
#   process_result<-switch(filetype,
#                      GPS = try({processGPS(filepath, filename, fileinfo)}, silent=TRUE),
#                      ABP = try({processABP(filepath, filename, fileinfo)}, silent=TRUE),
#                      MAE = try({processMicroAeth(filepath, filename, fileinfo)}, silent=TRUE),
#                      MPM = try({processMicroPEM(filepath, filename, fileinfo)}, silent=TRUE),
#                      HXI = try({processHexoskin(filepath, filename, fileinfo)}, silent=TRUE))
 
  
  
  process_result<-switch(filetype,
                         GPS = processGPS(filepath, filename, fileinfo),
                         ABP = processABP(filepath, filename, fileinfo),
                         MAE = processMicroAeth(filepath, filename, fileinfo),
                         MPM = processMicroPEM(filepath, filename, fileinfo),
                         HXI = processHexoskin(filepath, filename, fileinfo))
  

      return(process_result)
  
}


# -----------------------------------------------------------------------------
# Allow autoincrement field and date added
# -----------------------------------------------------------------------------

# in order to write the tables to the postgresql database the dbWriteTable works
# fine but when I want an autoincrement serial field and a auto date/time field
# for when data was added I needed a hack like this. The dbWriteTable uses the
# function postgresqlWriteTable so I'm creating a new functino called 
# postgresqlWriteTableAlt that takes care of it. Copied from stackoverflow

#http://bit.ly/1E0Vsf6
#' xxx
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

# -----------------------------------------------------------------------------
# parseFileName
# -----------------------------------------------------------------------------
#' xxx
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



# -----------------------------------------------------------------------------
# is error
# -----------------------------------------------------------------------------

#http://adv-r.had.co.nz/Exceptions-Debugging.html
#' xxx
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
is.error <- function(x) inherits(x, "try-error")


# -----------------------------------------------------------------------------
# repeatFileInfo
# -----------------------------------------------------------------------------

# we are adding columns to the tables in the database with the metadata so
# we need to metadata repeated for each record in the table

#' xxx
#' 
#' @param sdf
#' @return user.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
repeatFileInfo<-function(fileinfo,n){
  
  #fileinfo<-"BIKE0001_ABP01_S01_150306.abp"
  #n<-30
  
  varnames<-c("subjectID", "instrumentID", "sessionID", "filterID")
  l<-length(fileinfo)-1
  fin<-data.frame(matrix(fileinfo[1:l], nrow=n, ncol=l, byrow=TRUE))
  
  names(fin)<-tolower(varnames[1:l])
  return(fin)
  
}


# -----------------------------------------------------------------------------
# splitHeader and collapseHeader -- for dealing with MicroPEM headers
# -----------------------------------------------------------------------------
#' xxx
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
#' xxx
#' 
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
