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
library(leaflet)
library(tidygeocoder)
team_data <- tibble(
    name = c("Jarad", 
             "Prakesha", 
             "Utti"),
    address = c("2927 Sycamore St, Alexandria, VA 22305", 
                "5812 Lone Oak Dr, Bethesda, MD 20814",
                "1901 Altamount Ave, District Heights, MD 20747")
)

ui <- fluidPage(
    checkboxGroupInput(inputId = "members", "Who's Coming to Happy Hour", choices = team_data$name),
    tableOutput(outputId = "team_table"),
    leafletOutput("happy_map"),
    p()
)


server <- function(input, output) {
    geo_table <- reactive(team_data %>% 
        filter(name %in% input$members) %>% 
        geocode(address = address))
    output$team_table <- renderTable({
        geo_table()})
    
    output$happy_map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            addMarkers(data = geo_table())
    })
    
}


shinyApp(ui = ui, server = server)
