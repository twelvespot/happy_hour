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

ui <- fluidPage(
    textInput("name", "Name"),
    textInput("street", "Street Address"),
    textInput("city", "City"),
    textInput("state", "State"),          
    textInput("zip", "Zipcode"),
    actionButton(inputId = "add", label = "Add Person"),
    tableOutput(outputId = "team_table"),
    actionButton("geocode", label = "Find Happy Hour"),
    actionButton("clear", label = "Start Over"),
    leafletOutput("happy_map")
)

server <- function(input, output, session) {
    team_data_react <- reactiveValues(loc_table = tibble(Name = NA,
                                              `Street Address` = NA,
                                              City = NA,
                                              State = NA,
                                              Zipcode = NA))
    observeEvent(input$add, {
        team_data_react$loc_table <- tibble(Name = input$name,
                                            `Street Address` = input$street,
                                            City = input$city,
                                            State = input$state,
                                            Zipcode = input$zip) %>% 
                                     bind_rows(team_data_react$loc_table) %>% 
                                     filter(!is.na(Zipcode))
        updateTextInput(session, "name", value = "")
        updateTextInput(session, "street", value = "")
        updateTextInput(session, "city", value = "")
        updateTextInput(session, "state", value = "")
        updateTextInput(session, "zip", value = "")
    })
    output$team_table <- renderTable({team_data_react$loc_table})
    
    observeEvent(input$geocode, {
        team_data_react$geo_table <- team_data_react$loc_table %>% 
            geocode(street = `Street Address`, city = City, state = State,
                    postalcode = Zipcode, method = "census") %>% 
            filter(!is.na(long))
        team_data_react$centroid_matrix <- cbind(team_data_react$geo_table$long, team_data_react$geo_table$lat)
        team_data_react$centroid_point <- 
            if (nrow(team_data_react$centroid_matrix) > 2) {
            centroid(team_data_react$centroid_matrix)
        } else if (nrow(team_data_react$centroid_matrix) == 2) {
            midPoint(team_data_react$centroid_matrix[1,], team_data_react$centroid_matrix[2,])
        } else {
            team_data_react$centroid_matrix
        }
        team_data_react$team_map <- leaflet() %>% 
            addTiles() %>% 
            addMarkers(data = team_data_react$geo_table) %>%
            addMarkers(data = team_data_react$centroid_point, label = "Middle Ground!!")
    })
    
    output$happy_map <- renderLeaflet({team_data_react$team_map})
    observeEvent(input$clear, {
        team_data_react$loc_table <- tibble(Name = NA,
                                            `Street Address` = NA,
                                            City = NA,
                                            State = NA,
                                            Zipcode = NA)
    })
}

shinyApp(ui = ui, server = server)
