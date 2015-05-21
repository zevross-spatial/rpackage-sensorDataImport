# Sensor Data Import

Package with functions for processing environmental sensor data, create and load a PostgreSQL database and run Shiny apps specific to the project needs.


### General structure

* Functions related to creating or manipulating the PostgreSQL database are in `postgresql.R`
* The shiny apps themselves live in `inst/shiny-apps`


### Including the Shiny App
http://www.r-bloggers.com/supplementing-your-r-package-with-a-shiny-app-2/

* devtools::load_all()
* runShiny("nyc")



### To Do

* PostgreSQL functions on a Mac?
* roxygen2::document() a problem with files on network drive. Issue with `file.access()` within the `digest` package.
* ~~When "objects listed as exports, but not present in namespace" delete NAMESPACE.md~~ I was getting this error I believe because I had a commented out function with roxygen comments. I deleted and the error went away
* May need to rethink the database connection and allow the user to set the ports etc. This would probably mean taking the `get_connection()` out of the Shiny app and put it either in `runShiny()` or in it's own function.

### If you're getting errors

* Sometimes deleting NAMESPACE and re-running document(), load_all() helped
* The rows might be getting uploaded to PostgreSQL but pgAdmin may not be updating so you may need to run `count` rather than `refresh`
