

ui <- shinyUI(pageWithSidebar(
        headerPanel('Iris k-means clustering'),
        sidebarPanel(
                selectInput('ticker', 'Ticker', filter(ticker)),
                selectInput('measure', 'Measure', names(master.table[5:9]))
                            #,selected=names(iris)[[2]]),
               # numericInput('clusters', 'Cluster count', 3,
                             #min = 1, max = 9)
        ),
        mainPanel(
                plotOutput('plot1')
        )
))



palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

server <-shinyServer(function(input, output, session) {
        
        # Combine the selected variables into a new data frame
        selectedData <- reactive({
                master.table[input$filter(master.table, ticker == stock.tickers), input$measure]
        })
        
        #clusters <- reactive({
               # kmeans(selectedData(), input$clusters)
      #  })
        
        output$plot1 <- renderPlot({
                
                plot(verizon.table$Date, verizon.table$Close)
        })
        
})
shinyApp(ui=ui, server=server)
