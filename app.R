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
library(geosphere)
team_data <- tibble(
    name = c("Jarad", 
             "Prakesha", 
             "Utti"),
    address = c("2927 Sycamore St, Alexandria, VA 22305", 
                "5812 Lone Oak Dr, Bethesda, MD 20814",
                "1901 Altamount Ave, District Heights, MD 20747")
)

ui <- fluidPage(
    checkboxGroupInput(inputId = "members", "Who's Coming to Happy Hour", 
                       choices = team_data$name,
                       selected = "Jarad"),
    tableOutput(outputId = "team_table"),
    leafletOutput("happy_map")
)


server <- function(input, output) {
    geo_table <- reactive(team_data %>% 
        filter(name %in% input$members) %>% 
        geocode(address = address))
    centroid_matrix <- reactive(as.matrix(geo_table()[c("long", "lat")]))
    centroid_point <- reactive(centroid(centroid_matrix()))
    output$team_table <- renderTable({
        geo_table()})
    
    output$happy_map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            addMarkers(data = geo_table()) %>% 
            addMarkers(data = centroid_point())
    })
    
}


shinyApp(ui = ui, server = server)
