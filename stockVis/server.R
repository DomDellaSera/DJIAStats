# server.R-This contains the R Code which receives inputs from ui.R, serving as the backend
library(dplyr)
library(RMySQL)
library(xts)
library(dygraphs)
library(shiny)
source("/home/dominic/Documents/Programming/Connections/RtoMySQLconnection.R")

shinyServer(function(input, output) {
      output$dygraph <- renderDygraph({
            # Renders Dygraph plot. Reruns every time user changes an input.
            
            sqlstatement<-paste("SELECT date,views,",input$metric, " FROM wikiviews WHERE stocktickers = '", input$ticker,
                                "' AND date > '", input$dates[1], "' AND date < '", input$dates[2],
                                "' AND ",input$metric, " > 0",sep = "")
            table<-dbGetQuery(conn = localcon,statement = sqlstatement)
            table_arranged<- arrange(table, date)
            table <-table_arranged
            
            
            
            table_date <-as.Date(table[,1])
            table_views <- table[,2]
            table_firm <- table[,3]
            
            # DyGraphs require Xtensible Time Series (xts) format
            table.xts <- xts(data.frame(table_views, table_firm), order.by = table_date)
            # fitted local regression makes interweekely variations in page views tolerable 
            smoothed_views <-fitted(loess(table.xts[,1]~ as.numeric(table_date),span = input$slider1))
            smoothed_views.df <- xts(data.frame(table_date, smoothed_views), order.by = table_date)
            smoothed_firm <-fitted(loess(table.xts[,2]~ as.numeric(table_date),span = input$slider1))
            smoothed_firm.df <-  xts(data.frame(table_date, smoothed_firm), order.by = table_date)
            
            
            # Equation for calculating daily percent change for future versions
            # table.xts_calc <- (diff(table.xts)/table.xts[-nrow(table.xts),] * 100)
            
            
            
            if('wiki_percent_change'%in% input$variable && 'stock_metric_change' %in% input$variable){
                  table.dygraph<- merge(smoothed_views.df, smoothed_firm.df)[,c(2,4)]
                  view_column <-"smoothed_views"
                  firm_column <- "smoothed_firm"
            } else if('wiki_percent_change' %in% input$variable){
                  table.dygraph<- merge(smoothed_views.df, table.xts$table_firm)[,2:3]#These mergers are being subsetted because the colum
                  #used to merge was screwing with the graph colors
                  view_column <-"smoothed_views"
                  firm_column <- "table_firm"
            } else if('stock_metric_change' %in% input$variable){
                  table.dygraph<- merge(table.xts$table_views, smoothed_firm.df)[,c(1,3)]
                  view_column <-"table_views"
                  firm_column <- "smoothed_firm"
                  
            } else {
                  table.dygraph<- merge(table.xts$table_views, table.xts$table_firm)
                  view_column <-"table_views"
                  firm_column <- "table_firm"
                  
            }
            
            
            
            
            dygraph(table.dygraph, main = "Stock Data and Wikipedia Page Views") %>%
                  dySeries(firm_column, axis = "y2", label = input$metric, color = '#4F037F') %>%
                  dySeries(view_column, axis = "y", label = "Page Views", color='#4FAF7F')%>%
                  dyAxis("y", label = "Page views")%>%
                  dyAxis("y2", label = input$metric, independentTicks = TRUE)%>%
                  dyRangeSelector(dateWindow = c(input$dates[1], input$dates[2]))%>%
                  dyHighlight(highlightCircleSize = 2, 
                              highlightSeriesBackgroundAlpha = 0.8,
                              hideOnMouseOut = TRUE)
      })
      
})

