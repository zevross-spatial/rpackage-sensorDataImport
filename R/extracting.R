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
  
  #aggregation_unit<-"2 hours"
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

#' This function extracts data from the database.
#' 
#' @family postgresql functions
#' @param tablename What is the name of the table (as a string).
#' @param do_aggregate should the data be aggregated (TRUE/FALSE).
#' @param clean_first should the data go through the table-specific cleaning procedure first.
#' @param aggregation_unit what time unit should be used for aggregation. This should be a string with a number followed by min, hour, day. Plural is OK (e.g., 'days')
#' @param xtravars what extra variables should be extracted when grabbing raw data. Should be NULL if aggregating! Note that this should be a vector of variable names or the word "all" to get all variables.
#' @param summarize_vars What variables will be summarized. This should be a vector of variable names -- variables should be numeric.
#' @param grouping_vars What variables will be used for aggregation (these are the keys)
#' @return user.
#' @export
#' @examples 
#' res<-get_sensor_data("hxi", clean_first=FALSE, xtravars="datetime") 
#' res<-get_sensor_data("hxi", xtravars=c("datetime", "subjectid"))  
#' 
#' res<-get_sensor_data("gps")
#' res<-get_sensor_data(tablename= "hxi", 
#'                      do_aggregate=TRUE, 
#'                      xtravars= NULL,
#'                       aggregation_unit="2 hours",
#'                      grouping_vars = c("subjectid", "sessionid"),
#'                      summarize_vars=c("cadence", "breathing_rate"))
#' 
#' # this will give you average lat, long (centroid) of each subject/session
#' res<-get_sensor_data("gps",
#'                      do_aggregate=TRUE, 
#'                      xtravars= NULL,
#'                      aggregation_unit="complete",
#'                      grouping_vars = c( "subjectid", "sessionid"),
#'                      summarize_vars=c("latitude", "longitude"))

#' # gives error, check xtravars
#' res<-get_sensor_data("hxi", xtravars=c("datetime", "asdfjasf")) #gives error
#' 
#' # gives error check grouping vars
#' res<-get_sensor_data("hxi", 
#'                      do_aggregate=TRUE, 
#'                      xtravars= NULL,
#'                      aggregation_unit="2 hours",
#'                      grouping_vars = c("datetime", "subjectid", "asdlfjsdlfj"),
#'                      summarize_vars=c("cadence", "breathing_rate"))
#'                      
#'res<-get_sensor_data("mpm",
#'    clean_first = TRUE,
#'    do_aggregate=TRUE,
#'    xtravars  = NULL,
#'    aggregation_unit="1 hour",
#'    grouping_vars = c( "subjectid", "sessionid"),
#'    summarize_vars = c("temperature", "rh", "flow"))
#'    
#'    
                     
get_sensor_data <- function(tablename, 
                            do_aggregate = FALSE,
                            clean_first = FALSE,
                            aggregation_unit="15 min",
                            xtravars = "all",
                            summarize_vars = NULL, 
                            grouping_vars = c("subjectid", "sessionid")
                            ){
  
  #tablename<-"mpm"
  vars_to_get<-NULL
  clean_vars<-NULL #variables needed to do cleaning
  valcon<-valid_connection()
  tableexists<-table_exists(tablename)
  con<-.connection$con
  possiblevars<-get_column_names(tablename)$column_name
  
  if(!valcon || !tableexists) stop(paste("Either you don't have a valid database connection or the table does not exist"), call.=FALSE)
  #if(do_aggregate && !"datetime"%in%grouping_vars) stop("You need the datetime field to aggregate")
  if(!agg_unit_ok(aggregation_unit)) stop("Your aggregation unit is invalid", call.=FALSE)
  if(!is.null(xtravars) & do_aggregate) stop("It looks like you want to aggregate. You should have xtravars=NULL", call.=FALSE)
  if(!is.null(xtravars) && all(tolower(xtravars)!="all") && !all(xtravars%in%possiblevars)){
    stop(paste("One of your extra variables is not in the table. Possible vars are", paste(possiblevars, collapse=",")))
  }
  
  if(do_aggregate && (is.null(grouping_vars) | !all(grouping_vars%in%possiblevars))){
    stop(paste("You have a problem with your grouping variables. Possible vars are", paste(possiblevars, collapse=",")), call.=FALSE)
  }
  
  if(do_aggregate && (is.null(summarize_vars) | !all(summarize_vars%in%possiblevars))){
    stop(paste("You have a problem with your summarize variables. Possible vars are", paste(possiblevars, collapse=",")), call.=FALSE)
  }
  
  
  
  
  thetable<-tbl(.connection, tablename)
  
  # if user has "all" for vars and is not aggregating get all
  # variables
  
  if(!do_aggregate && tolower(xtravars) == "all" ){
    
    dat<-collect(thetable, n = Inf)
    vars_to_get <- names(dat)
    
    # otherwise start with the user selected variables
    # if they're aggregating then include the grouping and summarizing vars
    # if they're cleaning then add the vars needed for clearning
  }else{
    vars_to_get <- xtravars #should be NULL if aggregating
    
    # note that I'm manually adding "sessionid" because, even if it's
    # not a grouping variable, we will still need to filter out the
    # non sessions in the cleaning phase
    if(do_aggregate) vars_to_get <- unique(c("datetime", vars_to_get, grouping_vars, summarize_vars))
    
    clean_vars<-NULL
    if(clean_first){
      clean_vars <- cleaning_vars(tablename)
    }
    
    
    dat <- collect(select_(thetable, .dots=unique(c(vars_to_get, clean_vars))))
  }
  
  
  # if we need to clean, we will clean and then drop the variables
  # that are required for cleaning
  
  if(clean_first){
    
    dat <- clean_data(tablename, dat)
    
    # This will get rid of the cleaning variables
    dat <- select_(dat, .dots = vars_to_get)


  }
 
  
  if(do_aggregate){

    dat <- aggregate_data(dat,
                   aggregation_unit = aggregation_unit,
                   summarize_vars = summarize_vars,
                   grouping_vars = grouping_vars)
  }
  
  # if the aggregation unit is complete then we're averaging with
  # no regard to time and then the output should not include time
  # info.
  dat <- ungroup(dat)
  
  if(aggregation_unit == "complete"){
    dat<-dat %>% select(-c(interval, begin, end))
  }
  
  # since users are more familiar with base R let's return
  # data frame instead of tbl_df
  
  data.frame(dat)
  
   
}

# *****************************************************************************
# aggregate_data
# *****************************************************************************

#' This function aggregates data
#' 
#' \code{createDatabase} will create a new postgresql database.
#' @family postgresql functions
#' @param dbname Give the database a name.
#' @param aggregation unit. examples include "5 min", "2 hour", 
#' @return user.
#' @export
#' @examples xxx




aggregate_data <- function(dat, 
                          aggregation_unit = aggreation_unit,
                          summarize_vars   = summarize_vars, 
                          grouping_vars    = grouping_vars){
  
  
  # dat
  vartypes<-sapply(summarize_vars, function(x) class(dat[[x]]))

  
  # make sure all the summarize vars are numeric or integer
  if(!all(vartypes%in%c("integer", "numeric"))){
    
    reform<-sapply(seq_along(vartypes), 
                   function(x) paste(names(vartypes)[x], vartypes[x], sep=":"))
    
    stop("Not all the variables are numeric - ", paste(reform, collapse=" | "),  call.=FALSE)  
    
  }
  
  
  
    dat <- add_intervals(dat, aggregation_unit)
  
    
    grouping_vars<-c("interval", "begin", "end", grouping_vars)
    
    # only keep summarize/group vars (drop cleaning vars)
    
    
    dat <- select_(dat, .dots = c(grouping_vars, summarize_vars ))
    
    # assemble dplyr code for the grouping
    grpvars<-paste(grouping_vars, collapse=", ")
    grp<-paste0("group_by(dat, ", grpvars, ")")
    
    # this will be output format
    template<-"XX_avg = mean(XX, na.rm=T), XX_sd = sd(XX, na.rm=T), XX_cnt = sum(!is.na(XX))"
    
    # here we're assembling the summarize statement for dplyr
    summarizevars<-sapply(summarize_vars, function(x) gsub("XX", x, template))
    summarizevars<-paste(summarizevars, collapse=", ")
    summarizevars<-paste0("summarize(", summarizevars, ", tot_cnt = n())")
    
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
  
  
  beg <- as.POSIXct("2007-01-01 00:00:00")
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




