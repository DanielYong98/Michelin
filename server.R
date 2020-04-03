library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(rgdal)
#C:/Users/DAN/Desktop/IVP proj/CW2/RStudio/Michelin
#setwd("C:/Users/DAN/Desktop/IVP proj/CW2/RStudio/Michelin/Michelin")
mcl <-
  read.csv("michelin3star.csv", stringsAsFactors = FALSE)

#polygon
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="TM_WORLD_BORDERS_SIMPL-0.3.zip")
# ## Unzip them ##
# unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")
myspdf = readOGR(dsn = getwd(), layer = "TM_WORLD_BORDERS_SIMPL-0.3")

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
    leaflet(
      data = myspdf,
      options = leafletOptions(
        zoomControl = FALSE,
        dragging = TRUE,
        zoomSnap = 0.5,
        zoomDelta = 0.5,
        minZoom = 2.5,
        maxZoom = 4
      )
    ) %>% setView(lng = 10,
                  lat = 42,
                  zoom = 2.5) %>% setMaxBounds(
                    lng1 = -180,
                    lat1 = -85,
                    lng2 = 180,
                    lat2 = 85
                  ) %>% addProviderTiles(providers$OpenStreetMap.DE) %>% addMarkers(
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
                    
                  ) %>% addPolygons(
                    fillColor = "green",
                    opacity = .7,
                    weight = 1,
                    color = "null",
                    highlight = highlightOptions(
                      weight = 1,
                      color = "black",
                      fillOpacity = 0.5,
                      bringToFront = TRUE
                    ),
                    label =  ~ NAME,
                    layerId = ~ NAME
                  )
    
  })
  observe({
    click = input$mymap_shape_click
    #  subset the spdf object to get the lat, lng and country name of the selected shape (Country in this case)
    sub = myspdf[myspdf$NAME == input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
    lat = sub$LAT
    lng = sub$LON
    nm = sub$NAME
    if (is.null(click))
      return()
    else
      leafletProxy("mymap") %>%
      setView(lng = lng ,
              lat = lat,
              zoom = 5)
    
    
  })
  
  
  
  #table/list
  output$results <- renderTable({
    filtered()
  })
}
