# *****************************************************************************
# C ---------------------------
# *****************************************************************************

#' This function creates a new postgreSQL database .
#' 
#' \code{createDatabase} will create a new postgresql database.
#' @family postgresql functions
#' @param dbname Give the database a name.
#' @param port. You likely don't need to change this.
#' @return user.
#' @examples
#' get_sensor_data(tablename="hxi", 
#'                aggregate = TRUE,
#'                clean_first = TRUE,
#'                vars = NULL,
#'                aggregation_unit="hour",
#'                grouping_vars = c("subjectid", "sessionid"),
#'                summarize_vars=c("cadence", "breathing_rate"))
#' 
#' 

agg_unit_ok <- function(aggregation_unit){
  
  if(tolower(aggregation_unit) == "complete") return(TRUE)
  
  #aggregation_unit<-"5  min"
  vals <- strsplit(aggregation_unit, "\\W+")[[1]]
  
  #if there is a trailing s remove it so we have consistency
  #in output tables
  
  
  if(length(vals)!=2) return(FALSE)
  
  vals[2] <- gsub("s$", "", vals[2])
  
  val1<- suppressWarnings(as.numeric(vals[1]))
  anumber  <- !is.na(val1) && is.numeric(val1) 
  goodunit <- vals[2]%in%c("min", "hour", "day") 

  return(anumber & goodunit)
  
}




# *****************************************************************************
# C ---------------------------
# *****************************************************************************

#' This function creates a new postgreSQL database .
#' 
#' \code{createDatabase} will create a new postgresql database.
#' @family postgresql functions
#' @param dbname Give the database a name.
#' @param port. You likely don't need to change this.
#' @return user.
#' @examples
#' get_sensor_data(tablename="hxi", 
#'                aggregate = TRUE,
#'                clean_first = TRUE,
#'                vars = NULL,
#'                aggregation_unit="hour",
#'                grouping_vars = c("subjectid", "sessionid"),
#'                summarize_vars=c("cadence", "breathing_rate"))
#' 
#' 

get_sensor_data <- function(tablename, 
                            do_aggregate = FALSE,
                            clean_first = TRUE,
                            aggregation_unit="15 min",
                            vars = "datetime",
                            summarize_vars = NULL, 
                            grouping_vars = c("subjectid", "sessionid")
                            ){
  
  
  valcon<-valid_connection()
  tableexists<-table_exists(tablename)
  con<-.connection$con
  
  if(!valcon || !tableexists) stop(paste("Either you don't have a valid database connection or the table does not exist"))

  
  
  if(do_aggregate && !"datetime"%in%names(dat)) stop("You need the datetime field to aggregate")
  if(!agg_unit_ok(aggregation_unit)) stop("Your aggregation unit is invalid")

  
  
  thetable<-tbl(.connection, tablename)
  
  # variables we need
  vars_to_get <- unique(c(vars, grouping_vars, summarize_vars))
  
  # if you're going to clean you'll probably need some extra variables
  if(clean_first){
    vars_to_get <- unique(c(vars_to_get, cleaning_vars(tablename)))
  }
  
  dat <- collect(select_(thetable, .dots=vars_to_get))
  
  
  # do the cleaning if desired
  if(clean_first){
    
    dat <- clean_data(tablename, dat)
  }
 
  
  if(do_aggregate){
    
    dat <- aggregate_data(dat,
                   aggregation_unit = aggregation_unit,
                   summarize_vars = summarize_vars,
                   grouping_vars = grouping_vars)
  }
  
  
  
   
}

# *****************************************************************************
# aggregat_data
# *****************************************************************************

#' This function aggregates data
#' 
#' \code{createDatabase} will create a new postgresql database.
#' @family postgresql functions
#' @param dbname Give the database a name.
#' @param aggregation unit. examples include "5 min", "2 hour", 
#' @return user.
#' @examples
#' aggregate_data(tablename="hxi", 
#'                aggregation_unit="1 hour",
#'                grouping_vars = c("subjectid", "sessionid"),
#'                summarize_vars=c("cadence", "breathing_rate"))
#' 



aggregate_data <- function(dat, 
                          aggregation_unit = aggreation_unit,
                          summarize_vars   = summarize_vars, 
                          grouping_vars    = grouping_vars){
  
  
  
  vartypes<-sapply(summarize_vars, function(x) class(dat[[x]]))
  
  # make sure all the summarize vars are numeric or integer
  if(!all(vartypes%in%c("integer", "numeric"))){
    
    reform<-sapply(seq_along(vartypes), 
                   function(x) paste(names(vartypes)[x], vartypes[x], sep=":"))
    
    stop("Not all the variables are numeric - ", paste(reform, collapse=" | "))  
    
  }
  
  
  
    dat <- add_intervals(dat, aggregation_unit)
  
    
    grouping_vars<-c("interval", "begin", "end", grouping_vars)
    
    # only keep summarize/group vars (drop cleaning vars)
    
    
    dat <- select_(dat, .dots = c(grouping_vars, summarize_vars ))
    
    # assemble dplyr code for the grouping
    grpvars<-paste(grouping_vars, collapse=", ")
    grp<-paste0("group_by(dat, ", grpvars, ")")
    
    # this will be output format
    template<-"XX_avg = mean(XX), XX_sd = sd(XX)"
    
    # here we're assembling the summarize statement for dplyr
    summarizevars<-sapply(summarize_vars, function(x) gsub("XX", x, template))
    summarizevars<-paste(summarizevars, collapse=", ")
    summarizevars<-paste0("summarize(", summarizevars, ", ", "interval_cnt = n())")
    
    # combine the dplyr group and summarize code
    for_dplyr<-paste(grp, summarizevars, sep="%>%")
  
    # run the dplyr code
    res<-eval(parse(text=for_dplyr))
  
  
    names(res)[grepl("to_char.datetime", names(res))]<-"datepart"
  
    return(res)
    

}



# *****************************************************************************
# get_intervals
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
add_intervals<-function(dat, aggregation_unit){
  
  if(tolower(aggregation_unit)=="complete") aggregation_unit <- "50000 days"
  
  # this seems to be necessary since originally was tbl_df
  dat<-data.frame(dat)
  setDT(dat)
  
  
  beg <- as.POSIXct("2013-01-01 00:00:00")
  end <- as.POSIXct(Sys.time())
  
  # allow for more than one space in agg unit
  
  aggregation_unit<-gsub("\\W+", " ", aggregation_unit)
  aggregation_unit<-tolower(aggregation_unit)
  
  timeseq<-seq(beg, end, by=aggregation_unit)
  
  if(length(timeseq)==1){
    ref<- data.table(interval="Complete", begin=beg, end=end)
  }else{
    # this is the reference dataset with the intervals
    ref<-data.table(interval=gsub("s$", "", aggregation_unit),
                    begin=timeseq[-length(timeseq)], 
                    end=timeseq[-1])
    
  }
  

  

  dat<-dat[, dummy := datetime]
  dat <- dat[order(datetime)]
  setkey(ref, begin, end) 
  setkey(dat, datetime, dummy)
  
  dat <- foverlaps(dat, ref, nomatch=0L, mult="first")[, dummy := NULL]
  dat
  data.frame(dat)
  
}




