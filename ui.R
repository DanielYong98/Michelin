library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(rgdal)
library(plotly)
library(plyr)



ui <- fluidPage(
  titlePanel(setBackgroundColor("#bd2333"),
             title =
               div(
                 align = "center",
                 HTML(
                   '<p style="color:white;font-weight:bolder;font-style:italic;font-stretch:ultra-condensed;text-decoration:underline;text-decoration-color:#F6C444;"><i class="fa fa-star" style = "color:#F6C444;"></i> 3-Starred Michelin Restaurants</p>'
                 )
               )),
  fluidRow(
    tags$head(includeCSS("styles.css")),
    leafletOutput(outputId = "mymap", height = "810px"),
    
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      top = "9%",
      left = "1%",
      width = "18%",
      fixed = TRUE,
      draggable = FALSE,
      height = "750px",
      tags$style(
        HTML(
          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #bd2333;border-top: 1px solid #bd2333;border-bottom: 1px solid #bd2333;} .irs-from, .irs-to, .irs-single { background: #bd2333 }"
        )
      ),
      tags$style(
        HTML(
          ".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #bd2333;border-top: 1px solid #bd2333;border-bottom: 1px solid #bd2333;} .irs-from, .irs-to, .irs-single { background: #bd2333 }"
        )
      ),
      
      #RESET ZOOM
      actionButton("zoomer", "Reset zoom"),
      
      #Price selector
     sliderInput("priceInput", p("Price",style="font-weight:bold;color:#bd2333;font-size:15px;"), 17, 900, c(17, 900), pre = "$"),
      
      #Comfort level selector
      sliderInput("comfortInput",p("Comfort Level",style="font-weight:bold;color:#bd2333;font-size:15px;"), 1, 5, c(1, 5), pre = ""),

      
      #Cuisine selector
      uiOutput("cuisineOutput"),
      
      
      #Preference selector
      checkboxInput("vegetarianInput", p("Vegetarian",style="font-weight:bold;color:#bd2333;font-size:15px;"), FALSE),
      checkboxInput("outdoorInput", p("Outdoor Dining",style="font-weight:bold;color:#bd2333;font-size:15px;"), FALSE),
                  
      div( style = "position: relative;height:380px;position:absolute; top:393px;width:100%;left:0px;",
            plotlyOutput("Country", width = "100%")
                    )
    ),
    
    absolutePanel(
    id = "controls", class = "panel panel-default",
    top = "9%", right = "1%", width = "17%", fixed=TRUE,
    draggable = FALSE, height = "385px",
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #bd2333;border-top: 1px solid #bd2333;border-bottom: 1px solid #bd2333;} .irs-from, .irs-to, .irs-single { background: #bd2333 }")),
    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #bd2333;border-top: 1px solid #bd2333;border-bottom: 1px solid #bd2333;} .irs-from, .irs-to, .irs-single { background: #bd2333 }")),
                  
    p("Restaurant List",style = "font-weight:bold;color:#F6C444;font-size:15px;"),
    div(style = "overflow-y: auto; height:340px;position:relative; top:0;left:0;", uiOutput("restaurantOutput")),
    div(style = "overflow-x: auto;height:510px; width:100%;position:absolute; top:384px;right:0;", plotlyOutput("resOutput",width = "100%"), 
                                                                                                                                      click = "plot2_click", uiOutput("dynamic")
      ),
      
    ),
    
  )
)