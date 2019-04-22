#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(gganimate)
library(ggplot2)
library(gganimate)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  fluidRow( class = "plotRow",
            
            # TODO height
            column(class = "myplot", width = 12, 
                   plotOutput(outputId = "particle", height = "600px"))
  ),
  
  fluidRow( class = "controlRow",
            style = 'height:10vh',
            column(width = 3, 
                   fileInput("file1", "Choose CSV File w/ persons",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv"))
            ),
            column(width = 3,
                   fileInput("file2", "Choose CSV File w/ antenna's",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values, text/plain",
                               ".csv"))
                   
            ),
            
            column(width = 3,
                   uiOutput("sliders")
            )
            
            # column(width = 3,
            #               downloadButton(outputId = "savegif",
            #                              label = "Save plot to gif"))
            
  ),
  fluidRow(
    column(width = 3,
           uiOutput("sel_agents")
    )),
  
  tags$head(tags$style("
                       .plotRow{height:700px;}
                       .myplot{height:600px}
                       .controlRow{height:100px;}
                       ")
  )
  )


# Define server logic 
server <- function(input, output) {
  
  # set file size limit
  options(shiny.maxRequestSize = 100*1024^2)
  
  #################### read data from csv ########################
  # agents
  agents <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, skip = 2, stringsAsFactors = FALSE, header = FALSE)
    
    return(tbl)
  })
  
  # antenna's
  antenna <- reactive({
    
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, stringsAsFactors = FALSE, header = FALSE)
    
    return(tbl)
  })
  
  ####################################################################  
  
  
  #################### create UI elements ############################  
  
  output$sliders <- renderUI(
    if (!is.null(agents)) {
      
      sliders <- sliderInput("time", "TIME:", min = min(agents()[,1]),
                             max(agents()[,1]), value = min(agents()[,1]), step = 1,
                             animate = animationOptions(interval = 500, loop = TRUE))
    })
  
  output$sel_agents <- renderUI(
    if(!is.null(agents)){
      
      sel_agents <- selectizeInput(inputId = "sa", label = "Select agent:", choices = c("ALL", agents()[,2]), selected = "ALL")
    })
  
  ####################################################################
  
  part_anim <- function(){
    
    df1 <- agents()[agents()[,1] == input$time, ]
    #df1 <- agents()[agents()[,1] <= input$time, ]
    if(input$sa == "ALL"){
      df1 
    } else {
      df1 <- df1[df1[,2] == input$sa,]  
    }
    
    df2 <- antenna()[]
    p <- ggplot(data = df1, aes(x = df1[,3], y = df1[,4])) + geom_point(aes(color = df1[,2], group = df1[,2], shape = ifelse(is.na(df1[,5]), "NoSIM", "SIM"), size = ifelse(is.na(df1[,5]), "NoSIM", "SIM"))) + scale_shape_manual(name = "", values = c(NoSIM = 2, SIM = 4)) + scale_size_manual(name = "", values = c(NoSIM = 2, SIM = 4)) 
    p <- p + geom_point(data = df1[!is.na(df1[,5]), ], aes(x = df1[!is.na(df1[,5]), 3], y = df1[!is.na(df1[,5]), 4], color = df1[!is.na(df1[,5]), 2], group = df1[!is.na(df1[,5]), 2])) 
    p <- p + xlim(0,10) + ylim(0,10)
    p <- p + geom_point(shape = 8, size = 6, data = df2, aes(x = df2[,2], y = df2[,3]), colour = "#CC0000")
    p <- p + xlab(label = "Longitude") + ylab("Latitude")
    p
    
  }
  
  
  output$particle <- renderPlot({
    req(part_anim)
    part_anim()
  })
  
  #  TODO gif save
  # output$savegif <- downloadHandler(
  #   filename = "test.gif",
  #   content = function(file){
  #     req(part_anim())
  #     saveGIF(
  #       for(i in 1:max(input$time)){
  #       part_anim()
  #       }, movie.name = "test.gif", interval = 0.1) 
  #     file.rename("test.gif", file)
  #   })
}

# Run the application 
shinyApp(ui = ui, server = server)