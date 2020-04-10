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
legend <- colorBin("green", domain = mcl$countries, bins = bins)
plot_map = worldcountry[worldcountry$id %in% mcl_countries, ]

server <- function(input, output) {
  output$cuisineOutput <- renderUI({
    pickerInput(
      "cuisineInput",
      label = p("Cuisine Type:", style = "font-weight:bold;color:#9A1F33;"),
      choices = c(sort(unique(
        mcl$Cuisine_Type
      ))),
      selected = mcl$Cuisine_Type,
      multiple = TRUE,
      options = list(`actions-box` = TRUE),
    )
  })
  

  output$restaurantOutput <- renderUI({
    x <- paste0(img(src="star1.png"),filtered()$Restaurant_name, sep = "<br>")
    HTML(x)
  })
  
  
  
  output$Country <- renderPlotly({
    temp = as.data.frame(table(filtered()$Country))
    p = ggplot(temp,
           aes(
             x = reorder(Var1, Freq),
             y = Freq,
             label = Freq
           )) +
      geom_bar(stat = "identity", fill = "#F6C444") +
      coord_flip() +
      labs(y = "No. of Restaurants") +
      theme(axis.title.y = element_blank()) +
      theme(axis.text=element_text(size=8))
    
    ggplotly(p, tooltip=c("label"))%>%
      config(displayModeBar = FALSE)
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
  
  output$resOutput <- renderPlotly({
    res <-
      data.frame(
        AVG_Price = (filtered()$Price_Lower + filtered()$Price_Upper) / 2,
        Country = filtered()$Country,
        Restaurant_Name = filtered()$Restaurant_name
      )
    q = ggplot(res, aes(x = AVG_Price, y = Country, z = Restaurant_Name)) +
        geom_point(size = 2, color = "#9A1F33") +
        coord_cartesian(xlim = ranges$x,
                        ylim = ranges$y,
                        expand = TRUE) +
        theme(axis.title.y = element_blank()) +
        theme(axis.text=element_text(size=8)) +
        labs(x = "Average Price")
    
    ggplotly(q, tooltip=c("z", "y", "x"))%>%
      config(displayModeBar = FALSE)
  
  })
  
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
        minZoom = 2
      )
    ) %>% setView(lng = 10, #2
                  lat = 42,
                  zoom = 2) %>% setMaxBounds(

                    lng1 = -180, #240
                    lat1 = -85,  #-55
                    lng2 = 180,  #242
                    lat2 = 85    #82
                  ) %>% addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(

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
          ),
        ),
        layerId = ~ name
        
      )%>% addEasyButton(easyButton(
        icon = "fa-globe",
        title = "Zoom to Default",
        onClick = JS("function(btn, map){ map.setZoom(2);map.setView([42, 10], 0); }")
      ))
    
  })
  
  observe({
    outofBounds <- FALSE
    click = input$mymap_shape_click
    #  subset the spdf object to get the lat, lng and country name of the selected shape (Country in this case)
    sub = myspdf[myspdf$NAME == click$id, c("LAT", "LON", "NAME")]
    lat = sub$LAT
    lng = sub$LON
    zoom5.5 <-
      c(
        "Japan",
        "Norway",
        "United Kingdom",
        "Germany",
        "Italy",
        "Spain",
        "South Korea"
      )
    zoom4.5 <- c("China")
    zoom6.5 <- c("Netherlands", "Belgium", "Denmark", "Switzerland","Austria")
    zoom7 <- c("Singapore", "Taiwan")
    zoom5 <- c("Sweden")
    zoom6 <- c("France")
    if (sub$NAME[1] %in% zoom5.5) {
      temp = 5.5
    }
    else if (sub$NAME[1] %in% zoom4.5) {
      temp = 4.5
    }
    else if (sub$NAME[1] %in% zoom7) {
      temp = 7
    }
    else if (sub$NAME[1] %in% zoom5) {
      temp = 5
    } else if (sub$NAME[1] %in% zoom6.5) {
      temp = 6.5
    }else if (sub$NAME[1] %in% zoom6) {
      temp = 6
    }
    
    
    if (is.null(click))
      return()
    
    if (click$id == "South Korea") {
      leafletProxy("mymap") %>%
        setView(lng = click$lng,
                lat = click$lat,
                zoom = 7.5)
    } else if (click$id == "United States of America") {
      leafletProxy("mymap") %>%
        setView(lng = click$lng,
                lat = click$lat,
                zoom = 4)
    } else{
      leafletProxy("mymap") %>%
        setView(lng = lng,
                lat = lat,
                zoom = temp)
    }
    
    
  })
  #
  # selected_points <- reactiveVal()
  #
  # observeEvent(input$plot2_click,{
  #   res <- data.frame(y = filtered()$Country, z = filtered()$Restaurant_name, x=(filtered()$Price_Lower + filtered()$Price_Upper) / 2)
  #
  #
  #   selected_points(nearPoints(res, input$plot2_click))
  # })
  #
  # # show a modal dialog
  # observeEvent(selected_points(), ignoreInit=T,ignoreNULL = T, {
  #   if(nrow(selected_points())>0){
  #     showModal(modalDialog(
  #       title = "Important message",
  #       paste0("You have selected: ",paste0(rownames(selected_points()),collapse=', ')),
  #       easyClose = TRUE
  #     ))
  #   }
  # })
  
  
  output$dynamic <- renderUI({
    req(input$plot2_click)
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderPrint({
    click <- input$plot2_click
    # print(str(hover)) # list
    res <-
      data.frame(
        y = filtered()$Country,
        z = filtered()$Restaurant_name,
        x = (filtered()$Price_Lower + filtered()$Price_Upper) / 2
      )
    y <- nearPoints(res, input$plot2_click)
    req(nrow(y) != 0)
    y
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot2_dblclick, {
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  #static label if zoom reaches a certain level
  observe({
      print(input$mymap_zoom)           # Display zoom level in the console
    if(is.null(input$mymap_zoom)){
      print("is null")
    }else
      if(input$mymap_zoom > 7){
        print("bigger than 5")
        leafletProxy("mymap") %>%clearMarkers()%>%addMarkers(
          data = filtered(),
          lat = ~ Latitude,
          lng = ~ Longitude,
          label = ~ Restaurant_name,
          labelOptions = labelOptions(
            noHide = T,
            direction = "top",
            style = list(
              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
              "font-size" = "14px",
              "font-style" = "italic",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          ),
          icon = starIcon,
          uLink <- ~URL,
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
            "<br>",
            "<b>",
            "<a href='http://guide.michelin.com/en'>Visit Website</a>",
            "</b>",
            "<br>"
          ),
          
        )
      }else if(input$mymap_zoom < 7){
        print("less than 5")
        leafletProxy("mymap") %>%clearMarkers()%>%addMarkers(
          data = filtered(),
          lat = ~ Latitude,
          lng = ~ Longitude,
          label = ~ Restaurant_name,
          labelOptions = labelOptions(
            hide = T,
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
            "<br>",
            "<b>",
            "<a href='http://guide.michelin.com/en'>Visit Website</a>",
            "</b>"
          ),
          
        )
      }
    }
  )
  
  
  
  # #table/list
  # output$results <- renderTable({
  #   filtered()
  # })
}