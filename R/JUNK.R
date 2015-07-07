get_connection("columbiaBike","spatial",  host="localhost",
               port=5433, user="postgres")


x<-process_microaeth("X:/projects/columbia_bike/notes/issues_w_app/PrePilot_01/MicroAeth Data/Ltest1 - 636/", 
"BIKE0001_MAE36_S01_150616.csv", c("abc", "def"))



process_microaeth<-function(filepath, filename, fileinfo,metainfilename){
  
  #filepath<-"X:/projects/columbia_bike/notes/issues_w_app/PrePilot_01/MicroAeth Data/Ltest1 - 636/BIKE0001_MAE36_S01_150616.csv"
  #filename<-"BIKE0001_MAE36_S01_150616.csv"
  #fileinfo<-c("abc", "def")
  headinfo<-read.csv(filepath,as.is=T, nrow=5, header=FALSE)
  manufacturer<-headinfo[1,1]
  deviceid<-headinfo[2,1]
  softwareV<-headinfo[3,1]
  flowrate<-headinfo[4,1]
  freq<-headinfo[5,1]
  
  colNames<-names(read.csv(filepath, as.is=T, nrow=1, skip=7))
  #BC should be last column
  colNames<-tolower(colNames[1:which(colNames=="BC")])
  data<-read.csv(filepath, as.is=T, skip=9, header=FALSE)
  
  data%<>%dplyr::select(1:length(colNames)) # don't comment
  
  names(data)<-colNames
  
  
  
  
  data%<>%dplyr::mutate(time=addZero(time,8), date.yyyy.mm.dd.=paste(as.Date(date.yyyy.mm.dd.,"%m/%d/%y"), time))%>%
    dplyr::select(-time)%>%
    dplyr::rename(datetime=date.yyyy.mm.dd.)
  
  metadata<-generate_metadata(fileinfo, nrow(data), filename, metainfilename)
  data<-cbind(data, metadata)
  
  return(data)
  
  
}
