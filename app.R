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
    leafletOutput("happy_map"),
    textOutput(outputId = "happy_address")
)

server <- function(input, output, session) {
    team_data_react <- reactiveValues(loc_table = tibble(Name = NA,
                                              `Street Address` = NA,
                                              City = NA,
                                              State = NA,
                                              Zipcode = NA,
                                              Country = "USA"))
    observeEvent(input$add, {
        team_data_react$loc_table <- tibble(Name = input$name,
                                            `Street Address` = input$street,
                                            City = input$city,
                                            State = input$state,
                                            Zipcode = input$zip,
                                            Country = "USA") %>% 
                                     bind_rows(team_data_react$loc_table) %>% 
                                     filter(!is.na(Zipcode)) %>% 
            mutate(across(everything(), ~ na_if(.x, "")))
        updateTextInput(session, "name", value = "")
        updateTextInput(session, "street", value = "")
        updateTextInput(session, "city", value = "")
        updateTextInput(session, "state", value = "")
        updateTextInput(session, "zip", value = "")
    })
    output$team_table <- renderTable({team_data_react$loc_table})
    
    observeEvent(input$geocode, {
        team_data_react$geo_table <- 
            if (all(!is.na(team_data_react$loc_table))) {
                team_data_react$loc_table %>% 
                geocode(street = `Street Address`, city  = City,
                        state = State, postalcode = Zipcode,
                        method = "census") %>% 
                    filter(!is.na(long))
            } else {
                team_data_react$loc_table %>% 
                geocode(postalcode = Zipcode, country = Country, 
                        method = "osm") %>% 
                    filter(!is.na(long))
            }
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
        team_data_react$happy_address <- team_data_react$centroid_point %>% 
            as_tibble() %>% 
            reverse_geocode(lat = lat, long = lon) %>% 
            pull(address)
    })
    
    output$happy_map <- renderLeaflet({team_data_react$team_map})
    observeEvent(input$clear, {
        team_data_react$loc_table <- tibble(Name = NA,
                                            `Street Address` = NA,
                                            City = NA,
                                            State = NA,
                                            Zipcode = NA)
        team_data_react$geo_table <- tibble(Name = NA,
                                            `Street Address` = NA,
                                            City = NA,
                                            State = NA,
                                            Zipcode = NA,
                                            lat = NA,
                                            long = NA)

    })
    output$happy_address <- renderText(team_data_react$happy_address)
    
}

shinyApp(ui = ui, server = server)
