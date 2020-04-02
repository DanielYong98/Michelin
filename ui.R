library(leaflet)
ui <- fluidPage(titlePanel("3 star Michelin restaurant"),
                fluidRow(
                  column(
                    2,
                    
                    #Price selector
                    sliderInput("priceInput", "Price", 0, 500, c(25, 400), pre = "$"),
                    
                    uiOutput("cuisineOutput"),
                    
                    
                    
                    #Preference selector
                    checkboxInput("vegetarianInput", "Vegetarian", FALSE),
                    checkboxInput("outdoorInput", "Outdoor dining", FALSE),
                    
                    #Comfort level selector
                    sliderInput("comfortInput", "Comfort level", 1, 5, c(1, 5), pre = "")
                  ),
                  
                  
                  column(8,
                         leafletOutput(outputId = "mymap"),
                         tableOutput("results")
                         ),
                         
                  column(2,
                         sliderInput("comfosrtInput", "Placeholder for restaurant list", 1, 5, c(1, 5), pre = ""),
                         fluidRow(verbatimTextOutput("Click_text"))
                )),
                )
