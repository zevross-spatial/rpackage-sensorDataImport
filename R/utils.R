#' @import dplyr
#' @import plotKML



#' @importFrom magrittr %<>%
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

prefixes_ok<-function(prefixes, allowed=c("GPS", "ABP", "MAE", "MPM", "HXI")){
  #prefixes<-c("GPS", "BB")
  tst<-prefixes%in%allowed
  return(all(tst))
  
  
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
  
  l<-length(fileinfo)
  fin<-data.frame(matrix(fileinfo, nrow=n, ncol=l, byrow=TRUE))
  print(head(fin))
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




