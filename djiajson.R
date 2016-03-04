#This was the first project I ever built. 
#This is data scraping portion I used to get and clean the data
#
# Function call map:
#     DJIA.Wiki{
#         
#         wikipageURL.cat{
#             Wikilinkpaste{}, pagerURLgen{}
#         }
#         tablegen{
#             json.data.extractor{}
#             return(table)
#         }
#     }
library(rjson)
library(XML)
library(RCurl)
library(dplyr) 
library(quantmod) 

errorlog <- NULL
generate_stock_data.tbl <- function(){
    # This function does two things:
    #   1) Scrapes the stock data from quantmod
    #   2) Creates a table to relate the JSON data to stock data by relating
    #       URLs to Stock tickers
    
    djia.companies.url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"
    djia.handle <- getURL(djia.companies.url)
    djia.xml <- xmlTreeParse(djia.handle, useInternalNodes = TRUE, isHTML = TRUE)
    stock.tickers <- xpathSApply(djia.xml, "//tr/td[3]/a", xmlValue)
    
    getSymbols(stock.tickers) 
    
    links.raw <-xpathSApply(djia.xml, "//tr/td[1]/a/@href")#main tmplate
    links.end <- as.character(links.raw)#nice string
    Firm<- substring(links.end[7:36], 7, last = 128L)
    print(Firm)
    
    stock.linkdf <- data.frame(stock.tickers, Firm)
    link.ticker.rrdbs <- tbl_df(stock.linkdf)
    master.stock.table = NULL
    print("Downloading stock tickers...")
    for(i in stock.tickers){
        # Takes 30 finanical data tables and binds by rows them so they can be
        # later merged by columns with wikipedia view data.
        financial.data <- get(i)
        print(i)
        financial.df <- data.frame(index(financial.data), financial.data, rep(i, length(index(financial.data))))
        colnames(financial.df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted", "stock.tickers")
        financial.data.table <- tbl_df(financial.df)
        if(is.null(master.stock.table)==TRUE){
            master.stock.table <- financial.data.table
            } else if(is.null(master.stock.table)==FALSE){
                master.stock.table <- bind_rows(master.stock.table, financial.data.table)
            }
        }
    financial.table.list <- list("link.ticker.rrdbs"=link.ticker.rrdbs, "master.stock.table" =master.stock.table)
    return(financial.table.list)
}

json.data.extractor <- function(json.url){
    # Function to be called in sapply loop
    # Inputs:
    #   Url in form of http://stats.grok.se/json/en/[YYYYMM]/[Company]
    # Outputs:
    #   Date,Views dataframe with length(rows)==days/month   
    
    #json.url <- 
    message(json.url)
    tryCatch({
        # Exception Handler + Log generator because fromJSON will fail
        # to get a couple of these links
        json.raw <- fromJSON(getURL(json.url))
    },
    warning=function(cond){
        message(cond)
    },
    error=function(cond){
        message("json", url, "could not be extracted")
        if(is.null(errorlog)==TRUE){
            errorlog <-c(jsonerrorcounter, Sys.time(), json.url, cond)
        } else if(is.null(errorlog) == FALSE){
            errorlog<-rbind(errorlog,c(jsonerrorcounter,Sys.time(), json.url, cond))
        } 
        message(cond)
        return("JSON Failed to Extract, See errorlog")
    },
    finally={print("extracting json...")
    })
    
    json.views <- sapply(1:length(json.raw$daily_views), function(x) json.raw$daily_views[[x]])# interger pageview values
    json.dates <-names(json.raw$daily_views)
    monthlyFirmViews <- data.frame(json.dates, json.views,
                                   rep(substr(json.url, 37, 60), length(json.raw$daily_views))) # Firm name column for merging later
    colnames(monthlyFirmViews) <- c("Date", "Views", "Firm")
    monthlyFirmViews <- tbl_df(monthlyFirmViews)%>%
        filter(Views > 0)%>%
        mutate(Date = as.Date(Date), Views, Firm)
    
    print(paste(substr(json.url, 37, 60), json.raw$month, "downloaded"))
    return(monthlyFirmViews)        
}

tablegen <- function(urls){
    # Takes master list of URLs extracts JSON, and row binds.
    #
    # Input: 
    #   30*months list of URLs
    # Returns:
    #   
    
    print("Downloading JSON")
    jsonerrorcounter <- 0  # fromJSON will fail a couple of times
    firm.tbls <-lapply(urls[1:length(urls)], json.data.extractor)
    
    
    print("JSON Extracted, Creating Tables")
    firmtable.master = NULL
    firm.name = NULL
    counter = 0
    
    for(i in firm.tbls){
        firm.identifier = substr(i, 37, nchar(i))
        i.dplyr <- filter(as.data.frame(i), Views > 0)
        if(is.null(firmtable.master == TRUE)){firmtable.master = i.dplyr}
        else{firmtable.master <- bind_rows(firmtable.master, i.dplyr)}
        print("Tables Combined:")
        print(counter)
        counter <- counter + 1
    }
    attach(generate_stock_data.tbl())
    linknticker.tbl <- link.ticker.rrdbs
    #firmtable.master.nice <- firmtable.master 
    
    firmtable.master.nice <- left_join(firmtable.master, linknticker.tbl, by= "Firm")
    print(firmtable.master)
    print(master.stock.table)
    message("Combining Data tables...")
    ultimate.data.table <- left_join(firmtable.master, master.stock.table )
    
    
    
    
    return(ultimate.data.table)        
}

DJIA.Wiki<- function(months.to.query, current.month = FALSE){ 
    # Main functin which takes Firm URLs format from DJIA wikipedia page and uses as  
    # a template for extraction at another source.
    #
    # Args:
    #   months.to.query: The number of months to scrape for the 30 companies
    #   current.month: If FALSE the fucntion starts a month before today. 
    #       If TRUE data collection starts today. Defualt is FALSE
    # Returns:
    #   Concatenated URLs containing JSON format data of wikipedia views for 
    #       the month, in the format 1 List of 30 lists of months.to.query URLs
    #

    
    
    pageURLgen<- function(){
        # Returns:
        #   30 URL Formated firm names
        
        print("Finding Dow Jones Industrial Average Companies...")
        djia.companies.url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"#       Dow Jones industrial Average
        djia.handle <- getURL(djia.companies.url)                                               #       Company aggregator
        djia.xml <- xmlTreeParse(djia.handle, useInternalNodes = TRUE, isHTML = TRUE)
        links.raw <-xpathSApply(djia.xml, "//tr/td[1]/a/@href")#main tmplateD
        links.end <- as.character(links.raw)#nice string
        djia.end.url<- substring(links.end[7:36], 6, last = 1000L)
        print("Done")
        return(djia.end.url)#this only returns the first part of the links unique to the differen companies
        
    }
    
    
    wikipageURL.cat <- function(months.to.query){
        # Completes parent function by seqencing dates and concatenating into URL
        
        if(current.month == FALSE){
            monthprior <- seq.Date(as.Date("2016-02-01"), length = 2, by = "-1 month")[2]
            dates.raw <- seq.Date(monthprior, length = months.to.query, by="-1 month")
            } else if (current.month == TRUE){
                dates.raw<- seq.Date(Sys.Date(), length = months.to.query, by="-1 month")
                }
        date.processed <-format.Date(dates.raw, "%Y%m")
        WikiLinkPaste <- function(x) {
            # Sapply with this function to concat link parts
            paste("http://stats.grok.se/json/en/", date.processed, x, sep = "")
            }
        urllists <- sapply(pageURLgen(), WikiLinkPaste)
        return(urllists)
    }
    
    
    
    
    
    
    
    
    #######################
    # Main Function calls #
    #######################
    
    
    
    #pageURLgen()
    aggregatelinks<-wikipageURL.cat(months.to.query)
    return(tablegen(aggregatelinks))
    
    
    }




