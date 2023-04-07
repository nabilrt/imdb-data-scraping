library(shiny)
library(DT)

ui <- fluidPage(
  tags$style(type = "text/css", "body {padding: 10px;}"),
  
  tags$div(
    style = "padding-right: 10px; float: left; width: 25%",
    h2("Welcome to Data Processing Dashboard!"),
    p("It contains complete steps of Data collecting to cleaning and visualization")
  ),
  
  tags$div(
    style = "padding-left: 10px; float: right; width: 75%",
  
  h3("Initial Raw Data of TOP 150 IMDB Movies and their Links"),
  DTOutput("table1"),
  
  h3("Initial Raw Data of TOP 150 Movies and their details"),
  DTOutput("table2"),
  
  
  h3("Pre-processed data of TOP 150 Movies"),
  DTOutput("table3"),
  h2("Descriptive Statistics"),
  verbatimTextOutput("console_output"),
  br(),
  br(),
  
  h2("Data Visualization"),
  tags$div(
    style = "padding-top: 30px",
    tags$div(
      style = "padding-right: 10px; float: left; width: 25%",
      selectInput("cinema_select", "Select a Graph:", choices = c("TOP 10 MOVIES BY REVIEW", "TOP 10 MOVIES BY POPULARITY",
                                                                  "TOP 10 MOVIES BY POPULARITY AND LENGTH",
                                                                  "RATING RANGES WITH FREQUENCIES",
                                                                  "MOVIE LENGTH RANGES WITH FREQUENCIES"))
    ),
    
    tags$div(
      style = "float: right; width: 75%",
      plotOutput("plot1")
    ),
    style = "clear: both"
  ),
 
  ),
  br(),
  tabPanel("Code",
           sidebarPanel(
             h3("Project Code"),
           ),
           mainPanel(
  tabsetPanel(
    type = "tab",
    tabPanel("scrapping.r", br(), verbatimTextOutput("scrapping")),
    tabPanel("data_pre-processing.r", br(), verbatimTextOutput("preprocess")),
    tabPanel("data-visualization.r", br(), verbatimTextOutput("vis")),
    tabPanel("ui.r", br(), verbatimTextOutput("ui")),
  )
           )
  )
)

server <- function(input, output) {
  output$table1 <- renderDT({
    datatable(final_data, options=list(pageLength=10))
  })
  
  output$table2 <- renderDT({
    datatable(uncleaned_movies, options=list(pageLength=10))
  })
  
  output$table3 <- renderDT({
    datatable(total_movies, options=list(pageLength=10))
  })
  output$table4 <- renderDT({
    datatable(summary_df, options=list(pageLength=10))
  })
  
  output$console_output <- renderPrint({
    capture.output({
      summary_df
    })
  })
  output$scrapping <- renderPrint({
    cat(paste(readLines("scrapping.r"), collapse="\n"), sep = "\n")
  })
  output$preprocess <- renderPrint({
    cat(paste(readLines("data_pre-processing.r"), collapse="\n"), sep = "\n")
  })
  
  output$ui <- renderPrint({
    cat(paste(readLines("ui.r"), collapse="\n"), sep = "\n")
  })
  output$vis <- renderPrint({
    cat(paste(readLines("data-visualization.r"), collapse="\n"), sep = "\n")
  })

  output$plot1 <- renderPlot({
    if(input$cinema_select == "TOP 10 MOVIES BY REVIEW") {
      top_10_movies_by_review
    }else if(input$cinema_select == "TOP 10 MOVIES BY POPULARITY"){
      top_10_movies_by_popularity
    }else if(input$cinema_select=="TOP 10 MOVIES BY POPULARITY AND LENGTH"){
      top_10_based_on_name_length
    }
    else if(input$cinema_select=="RATING RANGES WITH FREQUENCIES"){
      rating_frequency
    }else if(input$cinema_select=="MOVIE LENGTH RANGES WITH FREQUENCIES"){
      movie_length_frequency
    }
  })
  
 
}

shinyApp(ui, server)



