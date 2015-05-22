context("Process and upload tests")

gps<-try({process_data("X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0001_GPS01_S01_150306.gpx",
                       "BIKE0001_GPS01_S01_150306.gpx")}, silent=TRUE)

uploadgps<-try({upload_postgres(tablename="gps",data=gps)}, silent=TRUE)

test_that("process and upload GPS works", {
  
  expect_equal(is.error(gps), FALSE)
  expect_equal(is.error(uploadgps), FALSE)
})


micropem<-try({process_data("X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0002_MPM01_S99_BK0001_150306.csv",
                            "BIKE0002_MPM01_S99_BK0001_150306.csv")}, silent=TRUE)

uploadmicropem<-try({upload_postgres(tablename="mpm",data=micropem)}, silent=TRUE)

test_that("process and upload MicroPEM works", {
  
  expect_equal(is.error(micropem), FALSE)
  expect_equal(is.error(uploadgps), FALSE)
})