# Sensor Data Import

Package with functions for processing environmental sensor data, create and load a PostgreSQL database and run Shiny apps specific to the project needs.


**Soon you can import with the following:**

devtools::install_github("zross/sensorDataImport")

This may work now but it has not been tested in Macs or other operating systems.

There are a few sample files in the package path extdata. To find where the package is installed use `.libPaths()` and browse to the first path.


### General structure

* Functions related to creating or manipulating the PostgreSQL database are in `postgresql.R`
* The shiny apps themselves live in `inst/shiny-apps`


### In terms of the flow:

* Create a new postgreSQL database with the `create_database()` function
* Add tables with the `add_tables_db()` function (refers to an existing SQL file right now)
* runShiny("nyc") will launch the Shiny app (inst/shiny-apps/nyc)
* The server.R file sets the working directory to the shiny apps working directory
* The server.R file tries to connect to the DB using the function `get_connection()`. It reports an error if there was a problem with connection. This has been updated so that the Shiny app has parameters related to the connection that are used by default in the get_connection() and the error is reported directly in the shiny app.
* The server then waits for upload.
* On upload the server tests for names that match a specific pattern. In this instance it looks for the first three letters to be ABP, GPS etc. If not it reports an error.
* The server then uses the function `process_data()` to try and process. This function is part of utils.R and it actually doesn't process the data it determines which of the processing scripts to use and then runs that script. The processing scripts are in the `sensor_processing.R` script and they each return a processed datafile.
* Note that each of the processing scripts makes use of a function called `repeatFileInfo()`. What this function does is takes the split file name ("fileinfo") as input and simply repeats this over and over. So if you have a file called GPS_123 then it creates a column where "GPS" is repeated over and over and 123 is repeated over and over. This way the table has the metadata extracted from the file name.
* The server then tries to upload the data with the function `upload_postgres()` which is a very simple script with one function for uploading. Note that this APPENDS data -- the table needs to exist.



### To Do (including tasks we assembled from in-person meeting)

The to-do list is no set up as GitHub issues.


### If you're getting errors

* Sometimes deleting NAMESPACE and re-running document(), load_all() helped
* The rows might be getting uploaded to PostgreSQL but pgAdmin may not be updating so you may need to run `count` rather than `refresh`
* How should we handle time zone -- do we need to have the user input?
* roxygen2::document() a problem with files on network drive. Issue with `file.access()` within the `digest` package. Right now I'm using a local drive.


### Additional notes

The microPEM has several different date formats that the app should be dealing with correctly but if you need specific files with specific formats see below.

```r
# Format: 1/1/15
filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0003_MPM01_S99_BK0001_150306.csv"
filename<-"BIKE0003_MPM01_S99_BK0001_150306.csv"

# Format: 08-Sep-14
filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0002_MPM02_S99_BK0001_150306.csv"
filename<-"BIKE0002_MPM02_S99_BK0001_150306.csv"


#Format: 1/26/2015
filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0001_MPM01_S99_BK0001_150306.csv"
filename<-"BIKE0001_MPM01_S99_BK0001_150306"
```



**Package development steps**

* devtools::load_all()
* devtools::document()
* devtools::test()
* runShiny("nyc")

library(devtools)
library(dplyr)
load_all()
get_connection("columbiaBike","spatial",  host="localhost",
    port=5433, user="postgres")
