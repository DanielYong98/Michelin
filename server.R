library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(rgdal)
#C:/Users/DAN/Desktop/IVP proj/CW2/RStudio/Michelin
setwd("C:/Users/DAN/Desktop/IVP proj/CW2/RStudio/Michelin/Michelin")
mcl <-
  read.csv("michelin3star.csv", stringsAsFactors = FALSE)

#polygon
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="TM_WORLD_BORDERS_SIMPL-0.3.zip")
# ## Unzip them ##
# unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")
myspdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")

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
    leaflet(data=myspdf,
      options = leafletOptions(
        zoomControl = FALSE,
        dragging = FALSE,
        zoomSnap = 0.5,
        zoomDelta = 0.5,
        minZoom = 2.5
      )
    ) %>% setView(lng = 10,
                  lat = 42,
                  zoom = 2.5) %>%
      addProviderTiles(providers$OpenStreetMap.DE) %>% addMarkers(
        data = filtered(),
        lat = ~ Latitude,
        lng = ~ Longitude,
        label = ~ Restaurant_name,
        popup = ~ paste(
          "<strong>",
          Restaurant_name,
          "</strong>",
          "<br>",
          "Cuisine: ",
          Cuisine_Type,
          "<br>",
          "Awarded year: ",
          Awarded_since,
          "<br>",
          "Chef: ",
          Chef,
          "<br>",
          "Specialty: ",
          Specialty,
          "<br>"
        ),
        
      )%>%addPolygons(fillColor = "green",
                      highlight = highlightOptions(weight = 5,
                                                   color = "red",
                                                   fillOpacity = 0.7,
                                                   bringToFront = TRUE),
                      )
    
  })
  observe(
    {  click = input$mymap_shape_click
    if(is.null(click))
      return()
    else
      leafletProxy("mymap") %>%
      setView(lng = click$lng , lat = click$lat, zoom = 4)
    
    }
  )
  
  
  
  #table/list
  output$results <- renderTable({
    filtered()
  })
}
