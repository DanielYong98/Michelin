library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(rgdal)
library(plotly)
library(plyr)


ui <- fluidPage(titlePanel( 
                  title = 
                    div(align = "center", h1(img(src="C:/Users/user/Desktop/IVP/star1.png"), "3-Starred Michelin Restaurants"))
                    
                ),
                fluidRow(
                  tags$head(includeCSS("styles.css")),
                  leafletOutput(outputId = "mymap",height="950px"),
                  
                  absolutePanel(
                    id = "controls", class = "panel panel-default",
                    top = "13%", left = "1%", width = "17%", fixed=TRUE,
                    draggable = FALSE, height = "800px",
                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #9A1F33;border-top: 1px solid #9A1F33;border-bottom: 1px solid #9A1F33;} .irs-from, .irs-to, .irs-single { background: #9A1F33 }")),
                    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #9A1F33;border-top: 1px solid #9A1F33;border-bottom: 1px solid #9A1F33;} .irs-from, .irs-to, .irs-single { background: #9A1F33 }")),
                    
                    #Price selector
                    sliderInput("priceInput", "Price", 17, 900, c(17, 900), pre = "$"),
                    
                    #Comfort level selector
                    sliderInput("comfortInput", "Comfort level", 1, 5, c(1, 5), pre = ""),
                    
                    #Cuisine selector
                    uiOutput("cuisineOutput"),
                    
                    
                    #Preference selector
                    checkboxInput("vegetarianInput", p("Vegetarian",style="font-weight:bold"), FALSE),
                    checkboxInput("outdoorInput", p("Outdoor Dining",style="font-weight:bold"), FALSE),
                  
                    div( style = "/*position: relative; top: 50px;height:420px;position:absolute; top:370px;width:100%;left:5px; */",
                          plotlyOutput("Country", width = "100%"),
                    )
                  ),  
                    
                  absolutePanel(
                      id = "controls", class = "panel panel-default",
                      top = "13%", right = "1%", width = "12%", fixed=TRUE,
                      draggable = FALSE, height = "350px",
                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #9A1F33;border-top: 1px solid #9A1F33;border-bottom: 1px solid #9A1F33;} .irs-from, .irs-to, .irs-single { background: #9A1F33 }")),
                      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #9A1F33;border-top: 1px solid #9A1F33;border-bottom: 1px solid #9A1F33;} .irs-from, .irs-to, .irs-single { background: #9A1F33 }")),
                  
                         p("Restaurant List",style = "font-weight: bold;"),
                         div(style = "overflow-y: auto; height:300px; background-color:#f0dcdf;position:relative; top:0px;left:0;", uiOutput("restaurantOutput")),
                         div(style = "overflow-x: auto;height:500px; width:100%;position:absolute; top:350px;right:15px;", plotlyOutput("resOutput",width = "100%"), 
                                                                                                                                      click = "plot2_click", uiOutput("dynamic")
                         ),
                         
                  ),

))