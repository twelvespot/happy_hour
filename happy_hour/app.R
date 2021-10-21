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
team_names <- c("Jarad", "Prakesha", "Utti")
team_data <- tibble(
    name = c("Jarad", 
             "Prakesha", 
             "Utti"),
    address = c("2927 Sycamore St, Alexandria, VA 22305", 
                "5812 Lone Oak Dr, Bethesda, MD 20814",
                "1901 Altamount Ave, District Heights, MD 20747")
)
# Define UI for application that draws a histogram
ui <- fluidPage(
    checkboxGroupInput(inputId = "members", "Who's Coming to Happy Hour", choices = team_names),
    tableOutput(outputId = "team_table")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$team_table <- renderTable({
        team_data %>% filter(name %in% input$members)})
}

# Run the application 
shinyApp(ui = ui, server = server)
