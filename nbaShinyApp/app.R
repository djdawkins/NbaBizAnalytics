#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

get_data <- source("get_data.R")$value
current_college_players <- get_data()

nba_rf_model <- source("nbaRandomForrest.R")$value


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NBA Top Scout"),
    fluidRow(
      column(12, actionButton("run_model", "Run Model", class = "btn-block"))
    ),
    fluidRow(
      column(12, verbatimTextOutput('x4'))
    ),
    
    hr(),
    fluidRow(
      column(12, verbatimTextOutput('x5'))
    ),
    
    hr(),
    h1('Prospective Players:'),

    fluidRow(
      column(12, DTOutput('tbl')),
    ),

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$tbl = renderDT(
      current_college_players, options = list(lengthChange = FALSE))
    
    output$x4 = renderPrint({
      s = input$tbl_rows_selected
      if (length(s)) {
        cat('These players were selected:\n\n')
        if(length(s) == 1){cat(current_college_players$namePlayer[[s]])}
        else{
          for(i in 1:length(s)){
            idx = s[i]
            cat(current_college_players$namePlayer[[idx]])
            if(i != length(s)){cat(" ,")}

          }
        }
      }
    })
    
    observeEvent(input$run_model, {
      s = input$tbl_rows_selected
      
      if (length(s)) {
        
        output$x5 = renderPrint({
        
        cat('Player Predictions:\n\n')
        y_pred <- nba_rf_model(as_tibble(current_college_players[s,]))
        
        if(length(s) == 1){cat(as.character(y_pred)[1])}
        else{
          for(i in 1:length(s)){
            # idx = s[i]
            cat(as.character(y_pred)[i])
            if(i != length(s)){cat(" ,")}
            
          }
        }
          })
      }
    })
    
     
}
# Run the application 
shinyApp(ui = ui, server = server)
