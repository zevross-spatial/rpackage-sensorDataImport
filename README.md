# Sensor Data Import

Package with functions for processing environmental sensor data, create and load a PostgreSQL database and run Shiny apps specific to the project needs.


**Soon you can import with the following:**
devtools::install_github("zross/sensorDataImport")

This may work now but it has not been tested in Macs or other operating systems.


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

*  We will need to develop and approach to assigning "session" to all the files. This could potentially be in the SQL database or maybe when the data is pulled.

* We need some kind of "events" field or check. For example, if humidity data is bad, but other data is good for a line how do we handle.

* We need to add data quality checks on file upload.

* Test PostgreSQL functions on a Mac.

* Make changes to backup function so that it will work on a Mac.

* There are few items in Ashlin's Shiny app that we need to make sure to decide about including Study ID, TimeZone, HEPA, minutes for rolling mean.

* Need to address time zones and review what happens with daylight savings.

* The `create_database()` and `add_tables_db()` functions are not flexible and only create the bike project DB and add tables based on the bike project SQL. May not be a priority to deal with this.

* ~~Create a file name test function. Right now we test for whether the file name includes ABP, GPS etc right in the server and it's not flexible. Probably we want a function with test_filenames that will allow us to add rules.~~ There is a function to test prefixes -- right now it defaults to seeing if the prefix is one of "GPS", "ABP", "MAE", "MPM", "HXI"

* ~~May need to rethink the database connection and allow the user to set the ports etc. This would probably mean taking the `get_connection()` out of the Shiny app and put it either in `runShiny()` or in it's own function.~~ I put the connection parameters in Shiny.
* ~~Explicit disconnect from DB~~ I think this is not necessary.

* ~~Better warning messages about DB upload problems. Use `RPostgreSQL::dbListTables(.connection$con)` to get list of tables and test.~~ I added tests for the connection (`valid_connection()`), I added a test to see if a table exists (`table_exists()`), a function to list tables (`list_tables()`) and a test if a filename was already uploaded (`already_uploaded()`).
* ~~Make sure you have appropriately addressed the different micropem date formats:~~


*  ~~Add file name to all the tables in the database~~

* ~~Double check how the line in the microPEM (30-30 or a string) is being processed~~ if 30,30 is replaced by a string this is fine, the string is added in place of 30|30

* ~~Double check memory usage, particularly when processing many files.~~ I'm not too worried about this so I'll leave for now.

*  ~~Double check how the errors are being handled. Specifically are other files being processed after an error or does it stop there.~~ It processes until there is an error and then stops so that files in the queue after an error are not processed. I changed the error warning to give a list of properly uploaded files and those that were not processed.

* ~~Add a check to see if the data has already been uploaded by checking file name or file name information~~ It now checks the database before uploading and reports an error if the file has been uploaded.

* ~~Add a backup function~~

* ~~Rather than create new tables with session etc potentially use PostgreSQL "views"~~ don't think this is necessary.

* ~~Create a delete data function. We discussed adding this to Shiny but this may not be worth the effort, an R function may be enough.~~ We now have a delete function (`delete_data()`). This can be added to Shiny if desired but right now is just a function.


* ~~In some data files there may be missing dates or times (microPEM has "Errored lines"). This may not be an issue in the database itself, but it will be an issue in the aggregation/summarization~~ we now delete errored lines.


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


