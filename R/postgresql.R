


# *****************************************************************************
# Create PostgreSQL Database ---------------------------
# *****************************************************************************

#' This function creates a new postgreSQL database .
#' 
#' \code{createDatabase} will create a new postgresql database.
#' @family postgresql functions
#' @param dbname Give the database a name.
#' @param port. You likely don't need to change this.
#' @return user.
#' @examples
#' create_database("columbiaBike", port=5432)
#' 
create_database<-function(dbname, port=5432, user="postgres"){  
  
  #TODO, might want a test to see if DB exists
  #val<-"psql -U postgres -c \"select count(*) from pg_catalog.pg_database where datname = 'cehtp_pesticide'\""
  #system(val)
  
  system(paste("createdb -h localhost -p", port, "-U", user, dbname))
  
}


# *****************************************************************************
# Add empty tables ---------------------------
# *****************************************************************************

#' This function adds tables to a postgreSQL database.
#' @family postgresql functions
#' @param x A number.
#' @export
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add_tables_db("columbiaBike", port=5432)
#' 

add_tables_db<-function(dbname, port=5432, user="postgres"){

  sqlfile<-system.file("sql", "create_tables.sql", package = "sensorDataImport")
  runsql<-paste("psql -p", port, "-U", user,"-d", dbname,"-a -f", sqlfile)
  system(runsql)
  
}


# *****************************************************************************
# Get database connection ---------------------------
# *****************************************************************************

#' This function creates the connection to a database
#' @family postgresql functions
#' @param dbname the database.
#' @param host database host, usually 'localhost'
#' @return .connection -- which is a global variable
#' @examples
#' get_connection(dbname="columbiaBike", host="localhost",
#' password="spatial", port=5433, user="postgres")
#' @export

get_connection<-function(dbname,
                         password,  
                         host="localhost",
                         port=5432, 
                         user="postgres"){
  
  #print("Connecting to database")
  # note the double arrow to make global
  .connection<<-try({dplyr::src_postgres(dbname=dbname, 
                                         host=host,
                                         password=password, 
                                         port=port, 
                                         user=user)}, silent=TRUE)  
}



# *****************************************************************************
# Test connection ---------------------------
# *****************************************************************************

#' Test if there is a valid connection.
#' 
#' 
#' \code{createDatabase} will create a new postgresql database.
#' @family postgresql functions
#' @param dbname Give the database a name.
#' @param port. You likely don't need to change this.
#' @return user.
#' @examples
#' create_database("columbiaBike", port=5432)
#' 
valid_connection<-function(con = ".connection"){  
  
  if(!exists(con) || is.error(eval(as.name(con)))){
    #message(paste(con, "is NOT valid database connection"))
    return(FALSE)
  }
  
  if(exists(con) & !is.error(eval(as.name(con)))){
    #message(paste(con, "is a valid database connection"))
    return(TRUE)
  }
  
}



# *****************************************************************************
# Test table existence ---------------------------
# *****************************************************************************

#' Test if table exists in DB.
#' 
#' 
#' \code{createDatabase} will create a new postgresql database.
#' @family postgresql functions
#' @param dbname Give the database a name.
#' @param port. You likely don't need to change this.
#' @return user.
#' @examples
#' create_database("columbiaBike", port=5432)
#' 
table_exists<-function(tablename, con = ".connection"){  

  if(!valid_connection(con)){
    stop(paste(con, "is NOT valid database connection"))
  }else{
    #tablename<-"gps"
    tst<-any(grepl(tablename, list_tables(con = con)))
    return(tst)
    
  }

  
}


# *****************************************************************************
# List tables ---------------------------
# *****************************************************************************

#' List tables in db.
#' 
#' 
#' \code{createDatabase} will create a new postgresql database.
#' @family postgresql functions
#' @param dbname Give the database a name.
#' @param port. You likely don't need to change this.
#' @return user.
#' @examples
#' create_database("columbiaBike", port=5432)
#' 
list_tables<-function(con = ".connection"){  
  
  if(!valid_connection(con)){
    stop(paste(con, "is NOT valid database connection"))
  }else{
    
    con<-eval(as.name(con))
    tbls<-RPostgreSQL::dbListTables(con[["con"]])
    db<-con$info$dbname
    #message(paste("The tables in the", db, "database are:", paste(tbls, collapse=", ")))
    
    return(tbls)
  }
  
  
}


# *****************************************************************************
# Upload table ---------------------------
# *****************************************************************************

#' This function is for uploading data to a postgres table
#' 
#' @family postgresql functions
#' @param tablename the table name.
#' @param data the data to upload
#' @return 
#' @examples
#' 
#' @export

upload_postgres<-function(tablename, data){
  rows<-nrow(data)
  print(paste("About to upload", rows, "rows to" , tablename))
  postgresqlWriteTableAlt(.connection$con, tablename, data, append=TRUE, row.names=FALSE)
  print(paste("Completed upload of", rows, "rows to" , tablename))
}


# *****************************************************************************
# Delete data ---------------------------
# *****************************************************************************

#' A function to delete data based on a filename
#' 
#' @family postgresql functions
#' @param tablename
#' @param filename
#' @return 
#' @examples
#' #"BIKE0001_GPS01_S01_150306.gpx"
#' @export

delete_postgres_data<-function(tablename, filename){
    
  
    test_filename<-readline("Please re-type the filename to\nconfirm deletion of all data with this filename:")
    
    if(identical(test_filename, filename)){
      q<-paste0("DELETE FROM ", tablename, " WHERE filename = '", filename, "'")
      message(paste0("Proceeding with delete using the query:\n", q))
      
      res<-RPostgreSQL::dbGetQuery(.connection$con, q)
      
      
    }else{
      warning(paste0("The file names you provided are not the same.\nThe file name given was ", filename, 
                     " and the file name typed was ", test_filename))
    }
  
}




# *****************************************************************************
# Test for previous file upload ---------------------------
# *****************************************************************************

#' Tests if data has already been uploaded
#' 
#' This function tests whether the filename exists in the given table
#' there is no check to see if the date or data are the same -- based only on 
#' filename
#' 
#' @family postgresql functions
#' @param dbname the database.
#' @param host database host, usually 'localhost'
#' @return .connection -- which is a global variable
#' @examples
#' try(already_uploaded("gps", "BIKE0001_GPS01_S01_150306.gpx"), silent=TRUE)
#' @export

already_uploaded<-function(tablename, filename){
  
  # tablename<-"gps"
  # filename<-"BIKE0001_GPS01_S01_150306.gpx"
  #thetable <- dplyr::tbl(.connection, tablename)
  #res<-nrow(dplyr::filter(thetable, filename==filename))==0
  q<-paste0("SELECT exists (SELECT 1 FROM ", tablename, " WHERE filename = '", filename, "' LIMIT 1);")
  res<-RPostgreSQL::dbGetQuery(.connection$con, q)
  
  if(res==FALSE){
    return()
  }else{
    stop()
  }

}





# *****************************************************************************
# Load data ---------------------------
# *****************************************************************************

my_fun <- function(a, b) {
  if (!requireNamespace("pkg", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)
  }
}


