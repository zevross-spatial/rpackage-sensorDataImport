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
# Load data ---------------------------
# *****************************************************************************
#' This function adds tables to a postgreSQL database.
#' @family postgresql functions
#' @param x A number.
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
# Load data ---------------------------
# *****************************************************************************

my_fun <- function(a, b) {
  if (!requireNamespace("pkg", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)
  }
}


