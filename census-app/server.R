library(shiny)
library(maps)
library(mapproj)

source("helpers.R")

counties <- readRDS('data/counties.rds') 

shinyServer(
        function(input, output) {
                
                output$map <- renderPlot({
                        data <- switch(input$var, 
                                       "Percent White" = counties$white,
                                       "Percent Black" = counties$black,
                                       "Percent Hispanic" = counties$hispanic,
                                       "Percent Asian" = counties$asian)
                        mapcolor <- switch(input$var,
                                           "Percent White" = "darkgreen",
                                           "Percent Black" = "red",
                                           "Percent Hispanic" = "orange",
                                           "Percent Asian" = "purple")
                        maptitle <- switch(input$var,
                                           "Percent White" = "Good Places to Live",
                                           "Percent Black" = "Good Places to get Shot",
                                           "Percent Hispanic" = "Foreign Areas",
                                           "Percent Asian" = "Math Places to live")
                        
                        
                        percent_map(var = data,
                                    color = mapcolor,
                                    legend.title = maptitle,
                                    max = input$range[2],
                                    min = input$range[1])
                        
                        
                })
                
        }
                )


