context("MicroPEM")

# *****************************************************************************
# A lot of different date formats possible with the micropem so we need to add
# a test that things work. We take the first date, make sure that it is, indeed
# a date and that the date is not after today + 2 (to make sure about the timezone)
# or earlier than 2010 (might be a better date to use)
# *****************************************************************************

# Format: 1/1/15
filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0003_MPM01_S99_BK0001_150306.csv"
filename<-"BIKE0003_MPM01_S99_BK0001_150306.csv"
tst1<-try({as.Date(process_micropem(filepath, filename, c("a", "b"))$datetime[1])}, silent=TRUE)


# Format: 08-Sep-14
filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0002_MPM02_S99_BK0001_150306.csv"
filename<-"BIKE0002_MPM02_S99_BK0001_150306.csv"
tst2<-try({as.Date(process_micropem(filepath, filename, c("a", "b"))$datetime[1])}, silent=TRUE)


#Format: 1/26/2015
filepath<-"X:/projects/columbia_bike/bikeStats/bikeApp/sample_data/BIKE0001_MPM01_S99_BK0001_150306.csv"
filename<-"BIKE0001_MPM01_S99_BK0001_150306"
tst3<-try({as.Date(process_micropem(filepath, filename, c("a", "b"))$datetime[1])}, silent=TRUE)



test_that("process and upload GPS works", {
  
  expect_equal(!is.error(tst1) && tst1<Sys.Date()+2 && tst1>as.Date("2010-01-01"), TRUE)
  expect_equal(!is.error(tst2) && tst2<Sys.Date()+2 && tst2>as.Date("2010-01-01"), TRUE)
  expect_equal(!is.error(tst3) && tst3<Sys.Date()+2 && tst3>as.Date("2010-01-01"), TRUE)
})
