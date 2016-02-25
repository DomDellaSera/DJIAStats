library(RMySQL)
library(ggplot2)
source("Documents/Programming/MySQL/Rconnection.R")
dbReadTable(conn = localcon, name = wikiviews)


test.vz.table <-dbGetQuery(conn = localcon, 
                                  statement = "SELECT date,open FROM wikiviews WHERE 
                                  stocktickers = 'VZ' AND 
                                  date > 2016-01-01 AND 
                                  Open > 0")
plot(as.Date(test.vz.table$date), test.vz.table$Open, pch = 20)
ggplot(test.vz.table, aes(as.Date(test.vz.table$date), test.vz.table$open))+geom_line()
test.vz.table <-tbl_df(dbGetQuery(conn = localcon, 
                                  statement = "SELECT * FROM wikiviews WHERE 
                                  stocktickers = 'VZ' AND 
                                  date > 2016-01-01 AND 
                                  Open > 0"))