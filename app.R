
library(shiny)
library(tidyverse)
library(leaflet)
library(tidygeocoder)
library(geosphere)
library(bslib)

ui <- fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    titlePanel(
        "Let's Find a Place We Can All Meet"
    ),
    sidebarLayout(
        sidebarPanel(
            textInput("name", "Name"),
            textInput("street", "Street Address"),
            textInput("city", "City"),
            textInput("state", "State"),          
            textInput("zip", "Zipcode"),
            actionButton(inputId = "add", label = "Add Person"),
            actionButton("clear", label = "Start Over"),
            actionButton("geocode", label = "Find Happy Hour")
        ),
        mainPanel(
            leafletOutput("happy_map"),
            textOutput(outputId = "happy_address")
        )
    ),
    tableOutput(outputId = "team_table")
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
        icon_blue <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = "blue"
        )
        icon_orange <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = "orange"
        )
        team_data_react$team_map <- leaflet() %>% 
            addTiles() %>% 
            addAwesomeMarkers(data = team_data_react$geo_table, icon = icon_blue) %>%
            addAwesomeMarkers(data = team_data_react$centroid_point, label = "Middle Ground!!", icon = icon_orange)
        team_data_react$happy_address <- team_data_react$centroid_point %>% 
            as_tibble() %>% 
            rename(lat = 2, long = 1) %>% 
            reverse_geocode(lat = lat, long = long) %>% 
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
