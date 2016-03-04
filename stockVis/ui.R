library(dygraphs)

firm.equal.ticker.encoding <-c("3M" = "MMM",
                               "American Express"= "AXP",
                               "Apple Inc." = "AAPL",
                               "Boeing" = "BA",  
                               "Caterpillar Inc." = "CAT",
                               "Chevron Corporation" = "CVX",
                               "Cisco Systems" = "CSCO",
                               "The Coca-Cola Company" = "KO", 
                               "DuPont" = "DD",
                               "ExxonMobil" = "XOM", 
                               "General Electric" = "GE",
                               "Goldman Sachs" = "GS", 
                               "The Home Depot" = "HD",
                               "Intel" = "INTC", 
                               "IBM" = "IBM",
                               "Johnson & Johnson" = "JNJ", 
                               "JPMorgan Chase" = "JPM",
                               "McDonalds" = "MCD", 
                               "Merck & Co." = "MRK",
                               "Microsoft" = "MSFT", 
                               "Nike, Inc." = "NKE",
                               "Pfizer" = "PFE", 
                               "Procter & Gamble" = "PG",
                               "The Travelers Companies" = "TRV", 
                               "UnitedHealth Group" = "UNH",
                               "United Technologies Corporation" = "UTX",
                               "Verizon Communications" = "VZ",
                               "Visa Inc." = "V",  
                               "Walmart" = "WMT",
                               "The Walt Disney Company" = "DIS")

shinyUI(fluidPage(
    
    titlePanel("Can Wikipedia Views Predict Market Movements"),
    
    sidebarLayout(position = "right",
                  
                  sidebarPanel(
                      
                      # User inputs values used as variables in server.R as input$inputId
                      
                      selectInput(inputId = "ticker",
                                  h5("Company",
                                     style ="font-family:'times';font-si16pt"),
                                  selected = "3M",
                                  choices = firm.equal.ticker.encoding), 
                      
                      selectInput(inputId = "metric",
                                  label = h5("Stock Metric",
                                             style ="font-family:'times';font-si16pt"),
                                  selected = "Close",
                                  choices = list("Open" = "Open", "High" = "High", "Low"= "Low",
                                                 "Volume" = "Volume", "Close"= "Close", "Adjusted"= "Adjusted")),
                      
                      dateRangeInput("dates", 
                                     h5("Data Loaded",
                                        style ="font-family:'times';font-si16pt"),
                                     start = "2015-01-01", 
                                     end = "2016-01-20"),
                      
                      checkboxGroupInput("variable", h4("Local Regression",style ="font-family:'times';font-si16pt"),
                                         c("Wikipedia Views" = "wiki_percent_change",
                                           "Stock Metrics" = "stock_metric_change")),
                      
                      sliderInput("slider1", 
                                  label = h6("Smoothing Parameter",style ="font-family:'times';font-si16pt"),
                                  min = 0.05, max = 0.6, value = .3, step = 0.05),
                      
                      
                      helpText(h6("Data Smoothing removes extremes, changing axis sizes.",
                                  style ="font-family:'times';font-si10pt"))),
                  
                  mainPanel(
                      dygraphOutput("dygraph"),
                      br(),
                      br(),
                      p("Moat and colleagues analyzed Wikipedia page view data for the thirty
                        companies composing the Dow Jones Industrial Average, an index tracking
                        price movements of firms deemed representative of major U.S. Industries.
                        Using Internet usage data as a measure of investor behavior (during information gathering 
                        stages of stock trading), the authors found significant insights into historical market 
                        trends on the basis of simple algorithmically conducted trades.",
                        style ="font-family:'times';font-si16pt"),
                      p("Data reproduced from: ",
                        style ="font-family:'times';font-si16pt"),
                      a("Quantifying Wikipedia Usage Patterns Before Stock Market Moves, ",
                        href="http://www.nature.com/articles/srep01801"),
                      p("(Moat et. al, 2013)"),
                      message("good paragraph")
                      )
                  )
    )
    )

