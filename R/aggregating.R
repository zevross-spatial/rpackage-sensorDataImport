








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
#' aggregate_vars(tablename="hxi", 
#'                aggregation_unit="hour",
#'                grouping_vars = c("subjectid", "sessionid"),
#'                summarize_vars=c("cadence", "breathing_rate"))
#' 



aggregate_vars <- function(tablename, 
                          aggregation_unit=c("min", "hour", "day"),
                          summarize_vars, 
                          grouping_vars = c("subjectid", "sessionid")){
  

  valcon<-valid_connection(con)
  tableexists<-table_exists(tablename)
  
  if(!valcon || !tableexists){
    stop(paste("Either you don't have a valid database connection or the table does not exist"))
  }
  
  vartypes<-column_types(tablename, summarize_vars)
  
  if(!all(vartypes=="numeric")){
    
    reform<-sapply(seq_along(vartypes), 
                   function(x) paste(names(vartypes)[x], vartypes[x], sep=":"))
    
    stop("Not all the variables are numeric - ", paste(reform, collapse=" | "))  
    
  }
  
  
    agg_form<-switch(aggregation_unit,
           day  = 'YYYY-MM-DD',
           hour = 'YYYY-MM-DD HH24',
           min  = 'YYYY-MM-DD HH24:MI')
  
    thetable<-tbl(eval(as.name(con)), "hxi")
    
    # assemble the dplyr code
    grpvars<-paste(grouping_vars, collapse=", ")
    grp<-paste0("group_by(thetable, ", grpvars, ", to_char(datetime,'", agg_form, "'))")

    template<-"XX_avg = mean(XX), XX_sd = sd(XX)"
    summarizevars<-sapply(summarize_vars, function(x) gsub("XX", x, template))
    summarizevars<-paste(summarizevars, collapse=", ")
    summarizevars<-paste0("summarize(", summarizevars, ", ", aggregation_unit, "_cnt = n())")
    
    for_dplyr<-paste(grp, summarizevars, sep="%>%")
  
    res<-eval(parse(text=for_dplyr))
  
    res<-collect(res)
  
    names(res)[grepl("to_char.datetime", names(res))]<-"datepart"
  
    return(res)
    

}








