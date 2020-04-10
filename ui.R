library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(rgdal)
library(plotly)
library(plyr)


ui <- fluidPage(
  titlePanel(setBackgroundColor("#9A1F33"),
             title =
               div(
                 align = "center",
                 HTML(
                   '<p style="color:white;"><i class="fa fa-star" style = "color:#F6C444;"></i> 3-Starred Michelin Restaurants</p>'
                 )
               )),
  fluidRow(
    tags$head(includeCSS("styles.css")),
    leafletOutput(outputId = "mymap", height = "950px"),
    
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      top = "10%",
      left = "1%",
      width = "17%",
      fixed = TRUE,
      draggable = FALSE,
      height = "750px",
      tags$style(
        HTML(
          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #9A1F33;border-top: 1px solid #9A1F33;border-bottom: 1px solid #9A1F33;} .irs-from, .irs-to, .irs-single { background: #9A1F33 }"
        )
      ),
      tags$style(
        HTML(
          ".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #9A1F33;border-top: 1px solid #9A1F33;border-bottom: 1px solid #9A1F33;} .irs-from, .irs-to, .irs-single { background: #9A1F33 }"
        )
      ),
      
      #Price selector
      sliderInput(
        "priceInput",
        p("Price", style = "font-weight:bold;color:#9A1F33;"),
        17,
        900,
        c(17, 900),
        pre = "$"
      ),
      
      #Comfort level selector
      sliderInput(
        "comfortInput",
        p("Comfort Level", style = "font-weight:bold;color:#9A1F33;"),
        1,
        5,
        c(1, 5),
        pre = ""
      ),
      
      #Cuisine selector
      uiOutput("cuisineOutput"),
      
      
      #Preference selector
      checkboxInput(
        "vegetarianInput",
        p("Vegetarian", style = "font-weight:bold;color:#9A1F33;"),
        FALSE
      ),
      checkboxInput(
        "outdoorInput",
        p("Outdoor Dining", style = "font-weight:bold;color:#9A1F33;"),
        FALSE
      ),
      
      div(style = "position: relative;height:410px;position:absolute; top:370px;width:100%;left:0px;",
          plotlyOutput("Country", width = "100%"),)
    ),
    
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      top = "10%",
      right = "1%",
      width = "17%",
      fixed = TRUE,
      draggable = FALSE,
      height = "370px",
      tags$style(
        HTML(
          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #9A1F33;border-top: 1px solid #9A1F33;border-bottom: 1px solid #9A1F33;} .irs-from, .irs-to, .irs-single { background: #9A1F33 }"
        )
      ),
      tags$style(
        HTML(
          ".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #9A1F33;border-top: 1px solid #9A1F33;border-bottom: 1px solid #9A1F33;} .irs-from, .irs-to, .irs-single { background: #9A1F33 }"
        )
      ),
      
      p("Restaurant List", style = "font-weight:bold;color:#F6C444;"),
      div(style = "overflow-y: auto; height:320px;position:relative; top:0;left:0;", uiOutput("restaurantOutput")),
      div(
        style = "overflow-x: auto;height:500px; width:100%;position:absolute; top:370px;right:0;",
        plotlyOutput("resOutput", width = "100%"),
        click = "plot2_click",
        uiOutput("dynamic")
      ),
      
    ),
    
  )
)