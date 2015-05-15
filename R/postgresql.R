


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
#' add(1, 1)
#' add(10, 1)
createDatabase<-function(dbname, port=5432, user="postgres"){  
  
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
#' addTables("mytable")
#' add(10, 1)

addTables<-function(dbname, port=5432, user="postgres"){

  sqlfile<-system.file("sql", "create_tables.sql", package = "bikeData")
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
#' .connection<<-try(dplyr::src_postgres(dbname="columbiaBike", host="localhost",
#'password="spatial", port=5433, user="postgres"),silent=TRUE)
#' @export

getConnection<-function(dbname="columbiaBike", host="localhost", password="spatial",port=5432, user="postgres"){
  # note the double arrow to make global
  .connection<<-try(dplyr::src_postgres(dbname=dbname, host=host,
                                        password=password, port=port, user=user),
                    silent=TRUE)
}




# *****************************************************************************
# Upload table ---------------------------
# *****************************************************************************

#' This function is for uploading data to a postgres table
#' @family postgresql functions
#' @param dbname the database.
#' @param host database host, usually 'localhost'
#' @return .connection -- which is a global variable
#' @examples
#' .connection<<-try(dplyr::src_postgres(dbname="columbiaBike", host="localhost",
#'password="spatial", port=5433, user="postgres"),silent=TRUE)
#' @export

uploadPostgres<-function(tablename){
  
  PGtry<-try(postgresqlWriteTableAlt(.connection$con, tablename, data, append=TRUE, row.names=FALSE), silent=TRUE)
  
  if(is.error(PGtry)){
    return(list(PGtry, "PGerror"))
  }else{
    writeLines(paste("Upload complete\n"))
    return(list("Fine", "Fine"))
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


