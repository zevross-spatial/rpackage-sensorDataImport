shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    
  ),
  img(src = "img/logo_footer.png"),
  titlePanel(
    
    h1("Columbia Bike Project Data Upload App"),
    windowTitle = "Columbia Bike Project"
  ),
  
  sidebarLayout(
 
        sidebarPanel(
          h2("Upload Files"),
          p("Choose the files to be imported. They can be GPX, ABP, microPEM, 
            microAeth or Hexoskin. The upload app will recognize the type and 
            upload appropriately."),
          br(),
          fileInput(inputId = 'file1', 
                    label = 'Choose text file(s)...', 
                    multiple = TRUE # can user select multiple files,
          ),
          helpText("Max file size is 1 GB"),
          
#                     accept=c('text/csv', 
#                              'text/comma-separated-values,text/plain', 
#                              '.csv')),
          br(),
          hr(),
          h3("Uploaded files"),
          htmlOutput('contents'),
          
          br(),
          width=6),
        mainPanel(
          tags$script(HTML("$('#file1_progress').remove();")),
          br(),
          width=6)# end main panel
    )
  ))