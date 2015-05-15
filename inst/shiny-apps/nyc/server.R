
setwd(system.file("shiny-apps", "nyc", package = "sensorDataImport"))
#source("shiny_helper_functions.R", chdir=TRUE)

options(shiny.maxRequestSize = 1000*1024^2)

shinyServer(function(input, output, session) {
  
  getConnection()
  #print("Running Columbia Shiny app")
  
  process<-reactive({
    # VALIDATION: Do you have a successful database connection?
    validate(
      need(!is.error(.connection), "There is a problem with your database connection")
    )
    
    
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
        need(allOK, "One of your datasets is not the right format")
      )
      
      
      # With progress is just the progress bar, inside I run the real
      # code
      withProgress(message = 'Processing and uploading:\n',
                   value = 0, {   
                     
                     for(i in 1:nfiles){
                       msg<-parseFileName(filepath=paths[i],
                                          filename=filenames[i])
                       
                       
                       if(i==1) completed<-" No files uploaded successfully."
                       if(i!=1) completed<-paste(" Files ", paste(filenames[1:(i-1)], collapse=", "), " uploaded successfully.", sep="")
                       
                       if(msg[[2]]=="prePGerror") whereOccurred<-" Error occurred before the database upload step."
                       if(msg[[2]]!="prePGerror") whereOccurred<-" Error occurred during the database upload step."
                       
                       validate(
                         
                         need(!is.error(msg[[1]]), 
                              paste("There is a problem with file ", filenames[i], ".", whereOccurred,
                                    completed, sep="")
                         )
                       )
                                              
                       incProgress(1/nfiles, detail=paste("Working on file", i, "of", nfiles))
                       
                     }#end for loop through files        
                     
                     
                   })#end withProgress
      
      
    }#end else re: infile
    return(filenames)
  }) # end reactive
  
  
  
  output$contents<-renderUI({
             
    HTML(paste(process(), collapse = '<br/>'))
    
  })
 
  
  
  
})
  
