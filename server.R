library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(rgdal)
library(geojsonio)
#C:/Users/DAN/Desktop/IVP proj/CW2/RStudio/Michelin
#setwd("C:/Users/DAN/Desktop/IVP proj/CW2/RStudio/Michelin/Michelin")
mcl <- read.csv("michelin3star.csv", stringsAsFactors = FALSE)

# #polygon
# # download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="TM_WORLD_BORDERS_SIMPL-0.3.zip")
# # ## Unzip them ##
# # unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")
myspdf = readOGR(dsn = getwd(), layer = "TM_WORLD_BORDERS_SIMPL-0.3")

worldcountry = geojson_read("countries.geojson", what = "sp")

# select Micheline countries for mapping polygons
mcl_countries = mcl$Country_ID
if (all(mcl_countries %in% worldcountry$id) == FALSE) {
  print("Error: inconsistent country names")
}

# create plotting parameters for map
bins = c(1, 10, 20, 30)
legend <- colorBin("green", domain = mcl_countries, bins = bins)
plot_map = worldcountry[worldcountry$id %in% mcl_countries,]

server <- function(input, output) {
  output$cuisineOutput <- renderUI({
    pickerInput(
      "cuisineInput",
      label = "Select cuisine",
      choices = c(sort(unique(
        mcl$Cuisine_Type
      ))),
      selected = mcl$Cuisine_Type,
      multiple = TRUE,
      options = list(`actions-box` = TRUE),
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
  
  bluePinIcon <- makeIcon(
    iconUrl = "blue-pin.png",
    iconWidth = 30,
    iconHeight = 30,
    iconAnchorX = 15,
    iconAnchorY = 30
  )
  
  #map function
  output$mymap <- renderLeaflet({
    leaflet(
      data = plot_map,
      options = leafletOptions(
        zoomControl = FALSE,
        dragging = TRUE,
        zoomSnap = 5,
        minZoom = 2.3,
        maxZoom = 4.5
      )
    ) %>% setView(lng = 10,
                  lat = 42,
                  zoom = 2.3) %>% setMaxBounds(
                    lng1 = -180,
                    lat1 = -85,
                    lng2 = 180,
                    lat2 = 85
                  ) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        data = filtered(),
        lat = ~ Latitude,
        lng = ~ Longitude,
        label = ~ Restaurant_name,
        labelOptions = labelOptions(
          direction = "top",
          style = list(
            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
            "font-size" = "14px",
            "font-style" = "italic",
            "border-color" = "rgba(0,0,0,0.5)"
          )
        ),
        icon = bluePinIcon,
        popup = ~ paste(
          "<h4 style='color:green'>",
          Restaurant_name,
          "</h4>",
          "<b>",
          "Cuisine: ",
          "</b>",
          Cuisine_Type,
          "<br>",
          "<b>",
          "Awarded year: ",
          "</b>",
          Awarded_since,
          "<br>",
          "<b>",
          "Chef: ",
          "</b>",
          Chef,
          "<br>",
          "<b>",
          "Specialty: ",
          "</b>",
          Specialty,
          "<br>"
        ),
        
      ) %>% addPolygons(
        stroke = TRUE,
        weight = 1,
        #stroke
        color = "black",
        #stroke
        smoothFactor = 0.2,
        fillColor = "green",
        fillOpacity = 0.1,
        highlight = highlightOptions(
          stroke = TRUE,
          weight = 1,
          #stroke
          color = "black",
          #stroke
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        label =  ~ name,
        labelOptions = labelOptions(
          direction = "top",
          style = list(
            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
            "font-size" = "14px",
            "border-color" = "rgba(0,0,0,0.5)"
          )
        ),
        layerId = ~ name
        
      ) %>% addLegend(
        "bottomright",
        pal = legend,
        values = ~ mcl_countries,
        title = "<small>Number of Michelin Restaurants</small>"
      )
    
    
  })
  observe({
    click = input$mymap_shape_click
    #  subset the spdf object to get the lat, lng and country name of the selected shape (Country in this case)
    sub = myspdf[myspdf$NAME == click$id, c("LAT", "LON", "NAME")]
    lat = sub$LAT
    lng = sub$LON
    if (is.null(click))
      return()
    else
      leafletProxy("mymap") %>%
      setView(lng = lng ,
              lat = lat,
              zoom = 4)
    
  })
  
  
  
  # #table/list
  # output$results <- renderTable({
  #   filtered()
  # })
}
