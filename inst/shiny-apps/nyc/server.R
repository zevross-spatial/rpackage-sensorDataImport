
library(sensorDataImport)
library(shiny)
library(ggplot2)
library(gridExtra)

setwd(system.file("shiny-apps", "nyc", package = "sensorDataImport"))


options(shiny.maxRequestSize = 1000*1024^2)

shinyServer(function(input, output, session) {
  
  plot_reactive <- reactiveValues(makeplot = FALSE)
  
  writeLines("Begin NYC Shiny server, about to connect to DB")
  
  projectid<-reactive({
    return(input$projectid)
    }) #end reactive projectid
  
  connectdb<-reactive({
 
    
    get_connection(dbname=projectid(), 
                   host=input$host, 
                   port=input$port,
                   password=input$password,
                   user="postgres")
    if(!valid_connection()){
      return("<span class='alert'>There is a problem with</br>your database connection</span>")
    }else{
      return("<span class='allgood'>Connected to DB</span>")
    }
  }) # end reactive connectdb
  
  

  process<-reactive({
    
    projectid <- input$projectid
    
    # if there is no infile, return NULL, this is not validation exactly
    if (is.null(input$file1)){
      return(NULL)
      
      # if there IS an infile
    }else{
      
      
      nfiles    <- length(input$file1$datapath) #how many files chosen
      paths     <- input$file1$datapath #temporary paths for the files
      filenames <- input$file1$name #names of files
      
      metainfilename<-isolate(input$metadatainfilename) # not used now


      
      # extract characters 1-3 from the second element of each file name
      # TODO: this will need to get moved to project specific functions
      filetypes <- substring(sapply(stringr::str_split(filenames, "_"), "[[",2),1,3)
      
      isfilename_ok <- filename_ok(filenames, projectid)

      
      # VALIDATION: Do first three letters match our rules?
      validate(
        need(isfilename_ok[[1]], paste0("These filenames have problems: ", 
                                       paste(isfilename_ok[[2]], collapse=", "), 
                                       ". Nothing uploaded."))
      )
      
      
      # With progress is just the progress bar, inside I run the real
      # code
      withProgress(message = 'Processing and uploading:\n',
                   value = 0, {   
                     
                     plots <<- list()
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
                       
                       if(already) {
                         
                         already_msg = error_report(currentfile_num=i, 
                                                    filenames=filenames,
                                                    stage="filename screening")     
                       }
                       
                       validate(need(!already, already_msg))
                       #*******************************************************
                       # Data processing
                       #*******************************************************
                       
                       # try and process the data
                       data <- try({initiate_processing(filepath  = curpath, 
                                                 filename  = curfilename,
                                                 projectid = projectid,
                                                 metainfilename = metainfilename)}, silent=TRUE)
                      
                       
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
                       validate(need(!is.error(upload), upload_msg))
                       
                       #*******************************************************
                       # Create a plot
                       #******************************************************* 

                       p<-try({plot_qaqc(
                         tablename=tolower(curfiletype),
                         dat=data)}, silent=TRUE)
                       
                       if(!is.error(p)){
                         plots[[i]] <<- p
                       }
                       
                       
                       plot_msg <- NULL
                       
                       # if there is an error in the upload
                       if(is.error(p)) {
                         
                         plot_msg = error_report(currentfile_num=i, 
                                                   filenames=filenames, 
                                                   stage="uploading")     
                       }
                       
                       # end session and report error in data handling
                       validate(need(!is.error(p), plot_msg))                       
                       
                       #*******************************************************
                       # Update progress indicator and clean up
                       #*******************************************************               
                       
                       incProgress(1/nfiles, detail=paste("Working on file", i, "of", nfiles))
                       rm(data)   
                     }#end for loop through files        
                     
                     
                   })#end withProgress
      
      #plot_reactive$makeplot <- !plot_reactive$makeplot
    }#end else re: infile
    return(list(filenames = filenames, plots = plots))
  }) # end reactive process file
  
  
  output$dbconnectinfo<-renderUI({
    HTML(paste(connectdb(), collapse = '<br/>'))
    
  })
  
  output$contents<-renderUI({
    HTML(paste(process()$filenames, collapse = '<br/>'))
    
  })

  output$renderdbname<-renderUI({
    textInput("dbname", label = h3("Database name"), 
              value = projectid())
    
  })
  
  
  output$plotsQAQC <- renderUI({
    list(
    h2("Quality assurance plots"),
    plotOutput("plots")
    )

  })

  output$plots <- renderPlot({
    #input$getplots
    p <- process()$plots
    return(do.call(grid.arrange, c(p, ncol = 1)))
  })
  
  
  
})

