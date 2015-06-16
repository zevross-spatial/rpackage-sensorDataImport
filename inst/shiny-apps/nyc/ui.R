

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    
  ),
  img(src = "img/logo_footer.png"),
  titlePanel(
    
    h1("Columbia Health Sensor Data Uploads"),
    windowTitle = "Columbia Bike Project"
  ),
  
  fluidRow(
    
    column(4,
           wellPanel(
      h4("Project and Database Settings"),
      selectInput("projectid", label = h4("Select project"), 
                  choices = list("-", "Biking" = "columbiaBike", 
                                 "Second hand smoke" = "secondhandsmoke",
                                 "Ghana" = "ghana"), selected = "-"),
#       textInput("dbname", label = h3("Database name"), 
#                 value = "columbiaBike"),
      uiOutput('renderdbname'),
      textInput("port", label = h3("Port"), 
                value = "5432"),
      textInput("host", label = h3("Host"), 
                value = "localhost"),
      textInput("password", label = h3("password"), 
                value = "spatial"),
      br(),
      htmlOutput('dbconnectinfo')
      )
      ),# end main panel
        column(8,
          h2("Upload Files"),
          p("Choose the files to be imported. They can be GPX, ABP, microPEM, 
            microAeth or Hexoskin. The upload app will recognize the type and 
            upload appropriately."),
          hr(),
          
          br(),
          fileInput(inputId = 'file1', 
                    label = 'Choose text file(s)...', 
                    multiple = TRUE # can user select multiple files,
          ),
          checkboxInput("metadatainfilename", label = "Metadata is in the file name (NOT USED RIGHT NOW)", value = TRUE),
          h6("Max file size is 1 GB"),
          
#                     accept=c('text/csv', 
#                              'text/comma-separated-values,text/plain', 
#                              '.csv')),
          br(),
          hr(),
          h3("Uploaded files"),
          htmlOutput('contents'),
          tags$script(HTML("$('#file1_progress').remove();")),
          
          br())
    )
  ))