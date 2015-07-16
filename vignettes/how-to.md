---
title: "Using the `sensorDataImport` package"
author: "Zev Ross"
date: "2015-07-14"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{how-to}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---





## Installing

You need the `devtools` package installed and then you can use:

```
devtools::install_github("zross/sensorDataImport")
```

Keep in mind that the package is in the early stages of development. The database and table creation scripts have not been adapted for Macs. But you should be able to run the code in a Mac console. For example, the code used to create the database is:

```
createdb -h localhost -p 5432 -U postgres columbiaBike
```

The code to load the tables is: 

```
psql -p 5432 -U postgres -d columbiaBike -a -f create_tables.sql
```

The file `create_tables.sql` can be found in the sql folder in your package directory. Your package will be installed in one of the paths listed when you type:

```
.libPaths()
```

Within the folders you can find the sql folder and in that is a file called `create_tables.sql`. 

## Launch app and connect

To launch the Shiny app for uploading data you use:

```
library(sensorDataImport)
runShiny("nyc")
```

On the left you will need to make sure the parameters are correct. You will want to select the biking project. Your port is likely 5432. If you have the right parameters you should see "Connected to DB" in green at the bottom left.

## Uploading

To upload you can simply use the upload button and select as many files as you want (using CTRL/CMD to select multiple) and then click OK. You will see a progress indicator. Your filenames need to follow our file naming conventions exactly.

## Extracting and aggregating data

For now the extraction and aggregation is done within R (rather than a Shiny app). In order to do this you will need a valid connection to the database. The password will be the password you used when you installed Postgres. Please let me know if you have problems with this piece.

```
library(sensorDataImport)
get_connection("columbiaBike","PASSWORD",  host="localhost",
    port=5432, user="postgres")
```

The `get_connection` function creates an object called `.connection` that should look like:

```
src:  postgres 9.3.4 [postgres@localhost:5433/columbiaBike]
tbls: abp, gps, hxi, mae, mpm, pdr
```

With the valid connection you are ready to extract the data. The function is `get_sensor_data` and it allows you to get the raw data or aggregated data. The function arguments look like:

```
tablename = This is the three letter table name (e.g., 'gps')

do_aggregate = Do you want to aggregate? Default is FALSE

clean_first = Do you want to clean the data first (not really implemented). Defaults to TRUE

aggregation_unit = What time unit do you want to aggregate to? Defaults to "15 min". Options might include ("10 min", "1 day", etc). Allows you to use "complete" which means essentially, a time unit of 50000 days.

xtravars = In the RAW data, RAW data only, what extra variables do you want included. Defaults to "all" which gives you all the variables in the table

summarize_vars = What are the variables you want to summarize (average, mean etc). Defaults to NULL which is the only value allowed if you're not aggregating. 

grouping_vars = What variables should be used to aggregate by. Defaults to c("subjectid", "sessionid")
```

### Examples


#### Get connection


```r
library(sensorDataImport)
# your connection parameters will be different with, probably, port 5432
get_connection("columbiaBike","spatial",  host="localhost",
    port=5433, user="postgres")

.connection
## src:  postgres 9.3.4 [postgres@localhost:5433/columbiaBike]
## tbls: abp, gps, hxi, mae, mpm, pdr
```


#### Get info on tables


```r
list_tables()
## [1] "gps" "abp" "mae" "mpm" "hxi" "pdr"
table_exists("blah")
## [1] FALSE
table_exists("gps")
## [1] TRUE
get_column_names("hxi")
##           column_name                   data_type
## 1            datetime timestamp without time zone
## 2      breathing_rate                     numeric
## 3          heart_rate                     numeric
## 4  minute_ventilation                     numeric
## 5             cadence                     numeric
## 6      sleep_position                     numeric
## 7            activity                     numeric
## 8           subjectid                   character
## 9        instrumentid                   character
## 10          sessionid                   character
## 11           filename           character varying
## 12          projectid           character varying
## 13          uniquekey                     integer
## 14          dateadded timestamp without time zone
get_row_count("mpm")
##   count
## 1 33622
```


#### Grab raw data (but it will get cleaned first if there is a cleaning function):


```r
library(sensorDataImport)
# your connection parameters will be different with, probably, port 5432
get_connection("columbiaBike","spatial",  host="localhost",
    port=5433, user="postgres")

.connection
## src:  postgres 9.3.4 [postgres@localhost:5433/columbiaBike]
## tbls: abp, gps, hxi, mae, mpm, pdr
gps_raw_clean <- get_sensor_data("gps")
## [1] "I'm cleaning GPS data"
head(gps_raw_clean) 
## Source: local data frame [6 x 0]
```



#### For GPS, get subject-specific centroid by averaging all records by subject/session

Note that this is NOT averaging by a time interval -- it's averaging all records by the grouping variables.



```r
# by default aggregate is by subject, session
get_sensor_data("gps", do_aggregate=TRUE, aggregation_unit="complete",
                                   xtravars=NULL, summarize_vars=c("latitude", "longitude"))
## [1] "I'm cleaning GPS data"
## Source: local data frame [5 x 9]
## 
##   subjectid sessionid latitude_avg latitude_sd latitude_cnt longitude_avg longitude_sd
## 1  BIKE0001        01     40.89911  0.05539662         3624     -73.93822   0.01585117
## 2  BIKE0001        02     40.89854  0.05549700         3677     -73.93870   0.01617592
## 3  BIKE0001        03     40.89827  0.05513719         3738     -73.93871   0.01603509
## 4  BIKE0001        04     40.89829  0.05612170         3578     -73.93851   0.01613981
## 5  BIKE0001        10     40.16865  0.01308769         3203     -74.02161   0.00512813
## Variables not shown: longitude_cnt (int), tot_cnt (int)
```


#### You can aggregate at odd intervals if desired

The aggregation_unit argument accepts any number of minutes, hours or days.



```r
micropem <- get_sensor_data("mpm",
                do_aggregate     = TRUE,
                aggregation_unit = "13 min",
                xtravars         = NULL,
                summarize_vars   = c("temperature", "flow", "neph_rhcorrect"),
                grouping_vars    = c("subjectid", "sessionid"))
## [1] "I'm cleaning MPM data"

head(micropem)
## Source: local data frame [6 x 15]
## Groups: interval, begin, end, subjectid
## 
##   interval               begin                 end subjectid sessionid temperature_avg
## 1   13 min 2015-06-16 20:51:00 2015-06-16 21:04:00  BIKE0001         1        27.07000
## 2   13 min 2015-06-16 21:04:00 2015-06-16 21:17:00  BIKE0001         1        27.26923
## 3   13 min 2015-06-16 21:17:00 2015-06-16 21:30:00  BIKE0001         1        27.27692
## 4   13 min 2015-06-16 21:30:00 2015-06-16 21:43:00  BIKE0001         1        27.23846
## 5   13 min 2015-06-16 21:43:00 2015-06-16 21:56:00  BIKE0001         1        27.13846
## 6   13 min 2015-06-16 21:56:00 2015-06-16 22:09:00  BIKE0001         1        27.10000
## Variables not shown: temperature_sd (dbl), temperature_cnt (int), flow_avg (dbl), flow_sd (dbl),
##   flow_cnt (int), neph_rhcorrect_avg (dbl), neph_rhcorrect_sd (dbl), neph_rhcorrect_cnt (int),
##   tot_cnt (int)
```













