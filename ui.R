library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(rgdal)
library(plotly)
library(plyr)



ui <- fluidPage(
  titlePanel(setBackgroundColor("#bd2330"),
             title =
               div(
                 align = "center",
                HTML('<img src="michelin-star.png" style="width:30px;height:30px;position:absolute;top:22px;left:370px;"<img/>'),
                HTML('<img src="michelin-star.png" style="width:30px;height:30px;position:absolute;top:22px;left:1045px;"<img/>'),
                #HTML('<img src="michelin-man.png" style="width:70px;height:70px;position:absolute;top:6px;left:320px;"<img/>'),

                HTML(
                   '<p style="color:white;font-family:Arial Black, Gadget, sans-serif;font-weight:bold;font-style:italic;font-stretch:ultra-condensed;text-decoration:underline;text-decoration-color:#f4c912;"><i class="fa fa-star" style = "color:#f4c912;"></i> 3-STARRED MICHELIN RESTAURANTS</p>'
                 )
               )),
  fluidRow(
    tags$head(includeCSS("styles.css")),
    leafletOutput(outputId = "mymap", height = "815px"),
    #RESET ZOOM
    actionButton("zoomer", "Zoom Out",style="position:absolute;top:70px;left:276px;color:#bd2330;"),
    
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      top = "8%",
      left = "0.5%",
      width = "18%",
      fixed = TRUE,
      draggable = FALSE,
      height = "750px",
      tags$style(
        HTML(
          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #bd2330;border-top: 1px solid #bd2330;border-bottom: 1px solid #bd2330;} .irs-from, .irs-to, .irs-single { background: #bd2330 }"
        )
      ),
      tags$style(
        HTML(
          ".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #bd2330;border-top: 1px solid #bd2330;border-bottom: 1px solid #bd2330;} .irs-from, .irs-to, .irs-single { background: #bd2330 }"
        )
      ),
      
      
      #Price selector
     sliderInput("priceInput", p("Price",style="font-weight:bold;color:#bd2330;font-size:15px;"), 17, 900, c(17, 900), pre = "$"),
      
      #Comfort level selector
      sliderInput("comfortInput",p("Comfort Level",style="font-weight:bold;color:#bd2330;font-size:15px;"), 1, 5, c(1, 5), pre = ""),

      
      #Cuisine selector
      uiOutput("cuisineOutput"),
      
      
      #Preference selector
      checkboxInput("vegetarianInput", p("Vegetarian",style="font-weight:bold;color:#bd2330;font-size:15px;"), FALSE),
      checkboxInput("outdoorInput", p("Outdoor Dining",style="font-weight:bold;color:#bd2330;font-size:15px;"), FALSE),
                  
      div( style = "position: relative;height:380px;position:absolute; top:393px;width:100%;left:0px;",
            plotlyOutput("Country", width = "100%"),
                    )
    ),
    
    absolutePanel(
    id = "controls", class = "panel panel-default",
    top = "8%", right = "0.5%", width = "17%", fixed=TRUE,
    draggable = FALSE, height = "385px",
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #bd2330;border-top: 1px solid #bd2330;border-bottom: 1px solid #bd2330;} .irs-from, .irs-to, .irs-single { background: #bd2330 }")),
    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #bd2330;border-top: 1px solid #bd2330;border-bottom: 1px solid #bd2330;} .irs-from, .irs-to, .irs-single { background: #bd2330 }")),
                  
    p("Restaurant List",align="center",style = "font-weight:bold;color:#f4c912;font-size:15px;"),
    div(style = "overflow-y: auto; height:340px;position:relative; top:0;left:0;", uiOutput("restaurantOutput")),
    div(style = "overflow-x: auto;height:510px; width:100%;position:absolute; top:384px;right:0;", plotlyOutput("resOutput",width = "100%"), 
                                                                                                                                      click = "plot2_click", uiOutput("dynamic")
      ),
      
    ),
    
  )
)