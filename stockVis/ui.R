library(shiny)
library(dplyr)

firm.equal.ticker.encoding <-c("3M" = " MMM",  "American Express"= "AXP", 
  "Apple Inc." = "AAPL", "Boeing" = "BA",  
  "Caterpillar Inc." = "CAT", "Chevron Corporation" = "CVX", 
  "Cisco Systems" = "CSCO", "The Coca-Cola Company" = "KO", 
  "DuPont" = "DD",  "ExxonMobil" = "XOM", 
  "General Electric" = "GE", "Goldman Sachs" = "GS", 
  "The Home Depot" = "HD", "Intel" = "INTC", 
  "IBM" = "IBM",  "Johnson & Johnson" = "JNJ", 
  "JPMorgan Chase" = "JPM", "McDonalds" = "MCD", 
  "Merck & Co." = "MRK", "Microsoft" = "MSFT", 
  "Nike, Inc." = "NKE", "Pfizer" = "PFE", 
  "Procter & Gamble" = "PG", "The Travelers Companies" = "TRV", 
  "UnitedHealth Group" = "UNH", "United Technologies Corporation" = "UTX",
  "Verizon Communications" = "VZ", "Visa Inc." = "V",  
  "Walmart" = "WMT", "The Walt Disney Company" = "DIS")





shinyUI(fluidPage(
        titlePanel("Wikipedia Views of the Dow Jones Industrial Average"),
        
        sidebarLayout(
                sidebarPanel(
                        helpText("Select a stock to examine. 
        Information will be collected from yahoo finance."),
                        
                        textInput("symb", "Symbol", "SPY"),
                        selectInput(inputId = "ticker",
                                    label = "Company",
                                    selected = "3M",
                                    choices = firm.equal.ticker.encoding), 
                        
                        dateRangeInput("dates", 
                                       "Date range",
                                       start = "2013-01-01", 
                                       end = "2016-01-31",
                                       
                                       br(),
                                       br(),
                                       
                                       checkboxInput("log", "Plot y axis on log scale", 
                                                     value = FALSE),
                                       
                                       checkboxInput("adjust", 
                                                     "Adjust prices for inflation", value = FALSE)
                        )),
                        
                        mainPanel(plotOutput("plot"))
                
        )))