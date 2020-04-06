library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(rgdal)

ui <- fluidPage(titlePanel("3 star Michelin restaurant"),
                fluidRow(
                  column(
                    2,
                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #9A1F33;border-top: 1px solid #9A1F33;border-bottom: 1px solid #9A1F33;} .irs-from, .irs-to, .irs-single { background: #9A1F33 }")),
                    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #9A1F33;border-top: 1px solid #9A1F33;border-bottom: 1px solid #9A1F33;} .irs-from, .irs-to, .irs-single { background: #9A1F33 }")),
                    
                    #Price selector
                    sliderInput("priceInput", "Price", 17, 500, c(17, 500), pre = "$"),
                    
                    #Comfort level selector
                    sliderInput("comfortInput", "Comfort level", 1, 5, c(1, 5), pre = ""),
                    
                    #Cuisine selector
                    uiOutput("cuisineOutput"),
                    
                    
                    #Preference selector
                    checkboxInput("vegetarianInput", p("Vegetarian",style="font-weight:bold"), FALSE),
                    checkboxInput("outdoorInput", p("Outdoor Dining",style="font-weight:bold"), FALSE),
                    
                    
                    fixedRow( style = "background-color:#f0dcdf;", 
                              plotOutput("Country", 
                                         width = "100%", 
                                         height = "400px",
                                        ),
                    )
                  ),
                 
                  column(8,
                         leafletOutput(outputId = "mymap",height="900"),
                         #tableOutput("results")
                         ),
                         
                  column(2,p("Restaurant List",style = "font-weight: bold;"),
                         div(style = "overflow-y: auto; height:300px; background-color:#f0dcdf;", uiOutput("restaurantOutput")),
                         fixedRow(style = "", plotOutput("resOutput",width = "100%", height="450px")),
                )),
                )
