library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(rgdal)
library(geojsonio)
# library(plotly)

library(plyr)


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
legend <- colorBin("green", domain = mcl$countries, bins = bins)
plot_map = worldcountry[worldcountry$id %in% mcl_countries, ]

server <- function(input, output) {
  output$cuisineOutput <- renderUI({
    pickerInput(
      "cuisineInput",
      label = p("Cuisine Type:", style = "font-weight:bold"),
      choices = c(sort(unique(
        mcl$Cuisine_Type
      ))),
      selected = mcl$Cuisine_Type,
      multiple = TRUE,
      options = list(`actions-box` = TRUE),
    )
  })
  
  output$restaurantOutput <- renderUI({
    x <- paste0(filtered()$Restaurant_name, sep = "<br>")
    HTML(x)
  })
  

  
  output$Country <-renderPlot({
    temp = as.data.frame(table(filtered()$Country))
    ggplot(temp, aes(x = reorder(Var1, Freq),y = Freq, main="Michelin 3-Starred Restaurants")) + geom_bar(stat = "identity",fill="#9A1F33") + coord_flip() + labs(y = "No. of restaurants") + theme(axis.title.y = element_blank()) 
    
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
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$resOutput <-renderPlot({
    data <- data.frame(x=(filtered()$Price_Lower + filtered()$Price_Upper) / 2, y=filtered()$Country)
    ggplot(data, aes(x= x, y= y)) + 
      geom_point(size=3,color="#9A1F33") +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE) +
      theme(axis.title.y = element_blank()) +
      labs(x = "Average Price")
  },width=300)
  
  #custom marker
  starIcon <- makeIcon(
    iconUrl = "star1.png",
    iconWidth = 20,
    iconHeight = 20,
    iconAnchorX = 0,
    iconAnchorY = 10
  )
  
  #map function
  output$mymap <- renderLeaflet({
    leaflet(
      data = plot_map,
      options = leafletOptions(
        zoomControl = FALSE,
        dragging = TRUE,
        zoomSnap = .55,
        minZoom = 2.3
        #maxZoom = 10
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
        icon = starIcon,
        popup = ~ paste(
          "<h4 style='color:#9A1F33'>",
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
        fillColor = "#9A1F33",
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
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  # #table/list
  # output$results <- renderTable({
  #   filtered()
  # })
}