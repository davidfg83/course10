# app.R

library(shiny)
library(shinythemes)
library(shinyjs)
library(dplyr)
library(stringr)
library(tm)

# Load helper functions
source("helpers.R")

# Load data once and keep in memory
unigrams_tbl <- readRDS("data/unigrams_tbl.rds")
bigrams_tbl <- readRDS("data/bigrams_tbl.rds")
trigrams_tbl <- readRDS("data/trigrams_tbl.rds")
co_occurrence_df <- readRDS("data/co_occurrence_df.rds")

# Define UI
ui <- fluidPage(
        theme = shinytheme("cerulean"),
        useShinyjs(),
        tags$head(tags$script(HTML("
    $(document).on('keypress', function(e) {
      if (e.which == 13) {
        $('#predict').click();
      }
    });
  "))),
        titlePanel("Next Word Prediction"),
        sidebarLayout(
                sidebarPanel(
                        textInput("sentence", "Enter a sentence:", value = "This is a test", width = "100%"),
                        div(style = "margin-top: 10px;",
                            p("Type your sentence and press Enter or click the button to see predictions."),
                            actionButton("predict", "Predict Next Word")
                        )
                ),
                mainPanel(
                        h3("Predicted Next Words"),
                        verbatimTextOutput("predictions")
                )
        )
)

# Define server logic
server <- function(input, output, session) {
        # Increase timeout and request size
        options(shiny.maxRequestSize = 120*1024^2)
        options(shiny.http.keepalive.timeout = 3600)
        
        observeEvent(input$predict, {
                req(input$sentence)
                suggestions <- predict_next_word(input$sentence, unigrams_tbl, bigrams_tbl, trigrams_tbl, co_occurrence_df)
                output$predictions <- renderPrint({
                        suggestions
                })
        })
}

# Run the application
shinyApp(ui = ui, server = server)