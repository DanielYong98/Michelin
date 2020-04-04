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
                    
                    #Price selector
                    sliderInput("priceInput", "Price", 17, 500, c(17, 500), pre = "$"),
                    
                    #Comfort level selector
                    sliderInput("comfortInput", "Comfort level", 1, 5, c(1, 5), pre = ""),
                    
                    #Cuisine selector
                    uiOutput("cuisineOutput"),
                    
                    
                    #Preference selector
                    checkboxInput("vegetarianInput", "Vegetarian", FALSE),
                    checkboxInput("outdoorInput", "Outdoor dining", FALSE)
                    
                    
                  ),
                  
                  
                  column(8,
                         leafletOutput(outputId = "mymap",height="900"),
                         #tableOutput("results")
                         ),
                         
                  column(2,
                         sliderInput("comfosrtInput", "Placeholder for restaurant list", 1, 5, c(1, 5), pre = ""),
                         
                )),
                )
