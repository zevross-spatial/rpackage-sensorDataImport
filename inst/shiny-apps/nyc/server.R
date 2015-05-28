
setwd(system.file("shiny-apps", "nyc", package = "sensorDataImport"))
#source("shiny_helper_functions.R", chdir=TRUE)

options(shiny.maxRequestSize = 1000*1024^2)

shinyServer(function(input, output, session) {
  writeLines("Begin NYC Shiny server, about to connect to DB")

  connectdb<-reactive({
    get_connection(dbname=input$dbname, 
                   host=input$host, 
                   port=input$port,
                   password=input$password,
                   user="postgres")
    if(is.error(.connection)){
      return("<span class='alert'>There is a problem with</br>your database connection</span>")
    }else{
      return("<span class='allgood'>Connected to DB</span>")
    }
  })

  
  process<-reactive({


    
    # if there is no infile, return NULL, this is not validation exactly
    if (is.null(input$file1)){
      return(NULL)
      
      # if there IS an infile
    }else{
      
  
      nfiles <- length(input$file1$datapath) #how many files chosen
      paths<-input$file1$datapath #temporary paths for the files
      filenames<-input$file1$name #names of files
      
      #extract characters 1-3 from the second element of each file name
      filetypes<-substring(sapply(stringr::str_split(filenames, "_"), "[[",2),1,3)
      
      # VALIDATION: Do first three letters match our rules   
      allOK<-all(filetypes%in%c("GPS", "ABP", "MAE", "MPM", "HXI"))
      validate(
        need(allOK, "One of your datasets is not a GPS, ABP, MAE, MPM or HXI file")
      )
      
      
      # With progress is just the progress bar, inside I run the real
      # code
      withProgress(message = 'Processing and uploading:\n',
                   value = 0, {   
                     
                     # loop through files
                     for(i in 1:nfiles){
                       
                       curpath     <- paths[i]
                       curfilename <- filenames[i]
                       curfiletype <- filetypes[i]
                       #*******************************************************
                       # Has file already been uploaded?
                       #*******************************************************
                       already<-try({already_uploaded(tablename = tolower(curfiletype),
                                        filename  = curfilename )}, silent=TRUE)
                       
                       already_msg<-NULL
                       
                       if(is.error(already)) {
                         
                         already_msg = error_report(currentfile_num=i, 
                                                 filenames=filenames,
                                                 stage="filename screening")     
                       }
                       
                       validate(need(!is.error(already), already_msg))
                       #*******************************************************
                       # Data processing
                       #*******************************************************
                       
                       # try and process the data
                       data <- try({process_data(filepath=curpath, 
                                            filename=curfilename)}, silent=TRUE)
                      
                      
                       data_msg <- NULL
                      
                       # if there is an error in the data processing
                       if(is.error(data)) {
                    
                               data_msg = error_report(currentfile_num=i, 
                                            filenames=filenames,
                                             stage="processing")     
                       }
                       
                      # end session and report error in data handling
                       validate(need(!is.error(data), data_msg))
                       
                      #*******************************************************
                      # Data upload
                      #*******************************************************
                      upload<-try({upload_postgres(
                        tablename=tolower(curfiletype),
                        data=data)}, silent=TRUE)
                      
        
                      
                      upload_msg <- NULL
                      
                      # if there is an error in the upload
                      if(is.error(upload)) {
                        
                        upload_msg = error_report(currentfile_num=i, 
                                                filenames=filenames, 
                                                stage="uploading")     
                      }
                      
                      # end session and report error in data handling
                      validate(need(!is.error(data), upload_msg))
                      
                      #*******************************************************
                      # Update progress indicator and clean up
                      #*******************************************************               
       
                       incProgress(1/nfiles, detail=paste("Working on file", i, "of", nfiles))
                       rm(data)   
                     }#end for loop through files        
                     
                     
                   })#end withProgress
      
      
    }#end else re: infile
    return(filenames)
  }) # end reactive
  

  output$dbconnectinfo<-renderUI({
    HTML(paste(connectdb(), collapse = '<br/>'))
    
  })
  
  output$contents<-renderUI({
    HTML(paste(process(), collapse = '<br/>'))
    
  })
 
  
  
  
})
  
