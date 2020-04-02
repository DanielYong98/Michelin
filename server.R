library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
#C:/Users/DAN/Desktop/IVP proj/CW2/RStudio/Michelin
#setwd("C:/Users/DAN/Desktop/IVP proj/CW2/RStudio/Michelin/Michelin")
mcl <-
  read.csv("michelin3star.csv", stringsAsFactors = FALSE)

server <- function(input, output) {
  
  output$cuisineOutput <- renderUI({
    pickerInput(
      "cuisineInput",
      label = "Select cuisine",
      choices = c(sort(unique(
        mcl$Cuisine_Type
      ))),
      selected = "French Contemporary",
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
      
    )
  })
  filtered <- reactive({
    mcl %>%
      filter(
        Price_Upper >= input$priceInput[1],
        Price_Upper <= input$priceInput[2],
        Cuisine_Type %in% input$cuisineInput,
        if (input$vegetarianInput == TRUE) {
          Vegetarian == input$vegetarianInput
        } else
          TRUE,
        if (input$outdoorInput == TRUE) {
          Outdoor_Dining == input$outdoorInput
        } else
          TRUE,
        Comfortable_Level >= input$comfortInput[1],
        Comfortable_Level <= input$comfortInput[2],
      )
  })
  
  
  #map function
  output$mymap <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     dragging = FALSE)) %>% clearBounds() %>%
      addProviderTiles(providers$OpenStreetMap.DE) %>% addMarkers(
        data = filtered(),
        lat = ~ Latitude,
        lng = ~ Longitude,
        label = ~ Restaurant_name,
        popup = ~ paste("<strong>",Restaurant_name, "</strong>","<br>",
                        "Cuisine: ", Cuisine_Type, "<br>",
                        "Awarded year: ", Awarded_since, "<br>",
                        "Chef: ", Chef, "<br>",
                        "Specialty: ", Specialty, "<br>"),

      )
  })
  
  
  
  #table/list
  output$results <- renderTable({
    filtered()
  })
}
