# app.R

library(shiny)
library(shinythemes)
library(shinyjs)
library(dplyr)
library(stringr)
library(tm)

# Load helper functions
source("helpers.R")

# Load data
unigrams_tbl <- readRDS("data/unigrams_tbl.rds")
bigrams_tbl <- readRDS("data/bigrams_tbl.rds")
bigrams_no_stop_tbl <- readRDS("data/bigrams_no_stop_tbl.rds")
co_occurrence_df <- readRDS("data/co_occurrence_df.rds")

# Define UI
ui <- fluidPage(
        theme = shinytheme("cerulean"),
        useShinyjs(),
        titlePanel("Next Word Prediction"),
        sidebarLayout(
                sidebarPanel(
                        textInput("sentence", "Enter a sentence:", value = "This is a test", width = "100%"),
                        div(style = "margin-top: 10px;",
                            p("Type your sentence and stop for a moment to see predictions."),
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
        debounce_timer <- reactiveVal(Sys.time())
        
        observeEvent(input$sentence, {
                debounce_timer(Sys.time())
        })
        
        observe({
                invalidateLater(500, session)
                if (Sys.time() - debounce_timer() > 2) {
                        req(input$sentence)
                        suggestions <- predict_next_word(input$sentence, unigrams_tbl, bigrams_tbl, bigrams_no_stop_tbl, co_occurrence_df)
                        output$predictions <- renderPrint({
                                suggestions
                        })
                }
        })
        
        observeEvent(input$predict, {
                req(input$sentence)
                suggestions <- predict_next_word(input$sentence, unigrams_tbl, bigrams_tbl, bigrams_no_stop_tbl, co_occurrence_df)
                output$predictions <- renderPrint({
                        suggestions
                })
        })
}

# Run the application
shinyApp(ui = ui, server = server)