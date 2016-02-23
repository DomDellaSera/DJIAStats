library(rjson)
library(XML)
library(RCurl)
library(dplyr)
library(quantmod)




DJIA.Wiki<- function(months.to.query, current.month = FALSE){ 
        djia.links <- function(){
                #this function takes url data from DJIA and uses it to extract
                #view data from a different site
                print("Finding Dow Jones Industrial Average Companies...")
                djia.url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"#       Dow Jones industrial Average
                xData <- getURL(djia.url)                                               #       Company aggregator
                djia.xmel <- xmlTreeParse(xData, useInternalNodes = TRUE, isHTML = TRUE)
                links.raw <-xpathSApply(djia.xmel, "//tr/td[1]/a/@href")#main tmplateD
                links.ra <- as.character(links.raw)#nice string
                djia.links.processed<- substring(links.ra[7:36], 6, last = 1000L)
                print("Done")
                return(djia.links.processed)#this only returns the first part of the links unique to the differen companies
                
        }
        
        
        djia.wikilinks <- function(months.to.query){
                ######################################################################################        
                #This function combines the links into a http://www.xyz.com format, and also converts#
                #months into a url format from the location we're going to extract data from         #
                ######################################################################################        
                if(current.month == FALSE){
                        monthprior <- seq.Date(Sys.Date(), length = 2, by = "-1 month")[2]
                        dates.raw <- seq.Date(monthprior, length = months.to.query, by="-1 month")
                }
                
                else if (current.month == TRUE){
                        dates.raw<- seq.Date(Sys.Date(), length = months.to.query, by="-1 month")
                }
                date.processed <-format.Date(dates.raw, "%Y%m")
                WikiLinkPaste <- function(x) {          #FUNCTION REALLY ONLY DEFINED FOR SAPPLY USE# 
                        #It allows for a complex loop to be inside a simple one
                        
                        paste("http://stats.grok.se/json/en/", date.processed, x, sep = "")
                }
                urllists <- sapply(djia.links(), WikiLinkPaste)
                return(urllists)
        }
        djia.links()
        aggregatelinks<-djia.wikilinks(months.to.query)
        #print(aggregatelinks)
        tablegen(aggregatelinks)
}
#THIS IS NOT RETURNING A HUGE TABLE AND I HAVE NO IDEA WHY, THE FUNCTIONS ARE WORKING INDIVIDUALLY
#BUT ARE NOT POINTING TO THEMSELVES ACCURATELY

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#t.url<- "http://stats.grok.se/json/en/201511/3M"
#testurls

generate_stock_data.tbl <- function(){#creates link/ticker relational table;A very inefficient,but quick fix
        djia.url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"#       Dow Jones industrial Average
        xData <- getURL(djia.url)
        djia.xmel <- xmlTreeParse(xData, useInternalNodes = TRUE, isHTML = TRUE)
        #Xpath:
        #/html/body/div[3]/div[3]/div[4]/table[2]/tbody/tr[1]/td[3]/a
        
        stock.tickers <- xpathSApply(djia.xmel, "//tr/td[3]/a", xmlValue)
        getSymbols(stock.tickers)
        links.raw <-xpathSApply(djia.xmel, "//tr/td[1]/a/@href")#main tmplateD
        links.ra <- as.character(links.raw)#nice string
        Firm<- substring(links.ra[7:36], 7, last = 128L)
        print(Firm)
        #break###TESTING
        stock.linkdf <- data.frame(stock.tickers, Firm)
        link.ticker.rrdbs <- tbl_df(stock.linkdf)
        master.stock.table = NULL
        print("Downloading stock tickers...")
        for(i in stock.tickers){
                financial.data <- get(i)
                print(i)
                financial.df <- data.frame(index(financial.data), financial.data, rep(i, length(index(financial.data))))
                colnames(financial.df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted", "stock.tickers")
                financial.data.table <- tbl_df(financial.df)
                if(is.null(master.stock.table)==TRUE){master.stock.table <- financial.data.table}
                else if(is.null(master.stock.table)==FALSE){master.stock.table <- bind_rows(master.stock.table, financial.data.table)}
                
                
        }
        financial.table.list <- list("link.ticker.rrdbs"=link.ticker.rrdbs, "master.stock.table" =master.stock.table)
        return(financial.table.list)
}

json.data.extractor <- function(json.url){#Take URL and creates a table from a single URL
        #json.url <- "http://stats.grok.se/json/en/201504/3M"
        testjson <- fromJSON(getURL(json.url))
        
        json.ints<- sapply(1:length(testjson$daily_views), function(x) testjson$daily_views[[x]])#interger pageview values
        tmp <-as.Date(names(testjson$daily_views))
        #3M April date from JSON has a 31st day as 0, and there is no April 31st so error message is priduced
        #Need to write a way to remove errored value
        df.j.test <- data.frame(tmp, json.ints, rep(substr(json.url, 37, 60), length(testjson$daily_views)))
        colnames(df.j.test) <- c("Date", "Views", "Firm")###THIS MAY BREAK THE PROGRAM
        print(paste(substr(json.url, 37, 60), testjson$month, "downloaded"))
        #close(url(json.url), type = "r")
        return(df.j.test)        
}

tablegen <- function(urls){#Generates a single clean table from a list of urls
        print("Downloading JSON")
        firmtabs <-lapply(urls[1:length(urls)], json.data.extractor)
        print("JSON Extracted, Creating Tables")
        firmtable.master = NULL
        firm.name = NULL
        counter = 0
        
        for(i in firmtabs){
                firm.identifier = substr(i, 37, nchar(i))
               
                i.firmtab.df <- as.data.frame(i)
                i.dplyr<- tbl_df(i.firmtab.df)
        
               
                i.dplyr.tidy <- filter(i.dplyr, Views > 0)
                if(is.null(firmtable.master == TRUE)){firmtable.master = i.dplyr.tidy}
                else{firmtable.master <- bind_rows(firmtable.master, i.dplyr.tidy)}
                print("Tables Combined:")
                print(counter)
                counter <- counter + 1
        }
        #wiki.link2ticker()
        attach(generate_stock_data.tbl())
        linknticker.tbl <- link.ticker.rrdbs
        firmtable.master.nice <- firmtable.master #DELETING BELOW BROKE MY PROGREAM
        #this is just here to unbreak it and is useless
        #firmtable.master.nice <- rename(firmtable.master,
                                        #'Date' = tmp,
                                        #'Views' = json.ints,
                                        #'Firm' = rep.testjson.title..length.testjson.daily_views..
                                        #)
        firmtable.master.nice <- left_join(firmtable.master.nice, linknticker.tbl, by= "Firm")
        print(firmtable.master.nice)
        print(master.stock.table)
        ultimate.data.table <- left_join(firmtable.master.nice, master.stock.table )
                
        
        
        
        return(ultimate.data.table)        
}



#Calls DIJI.Wiki, 