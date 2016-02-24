library(gtable)
library(grid)
c("3M" = " MMM",  "American Express",= "AXP", 
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
  "UnitedHealth Group" = "UNH", "United Technologies Corporation" = "UTX"
  "Verizon Communications" = "VZ", "Visa Inc." = "V",  
  "Walmart" = "WMT", "The Walt Disney Company" = "DIS")

wiki.views<-read.csv("bigwikiviewstable.csv")
wiki.views.tbl <- tbl_df(wiki.views)
rm(wiki.views)
wiki.views.tbl<- mutate(wiki.views.tbl, Date = as.Date(Date))

#MIN DATE 2011-02-01
#MAX DATE 2016-01-20
filter(wiki.views.tbl, stock.tickers == "MMM")


select(wiki.views.tbl, Date, Firm, Views, Open)


filter(wiki.views.tbl, Date > "2015-01-01")
filter(wiki.views.tbl, Date < "2012-01-01")


selected.table <-wiki.views.tbl %>%
        filter(stock.tickers == "VZ") %>%
        arrange(Date) %>%
        select(Date, Views, Close)%>%
        filter(is.na(Close) == FALSE) %>%
        filter(Date > "2015-01-01")

sel



table.df<- data.frame(selected.table) #MUST convert
table.xts <- xts(table.df, order.by = table.df[,1])
aes()
selected.table.mav <- mutate(selected.table, MA = rollmean(x = selected.table$Views, k=7, fill = NA))
selected.view.plot.mav<- ggplot(selected.table.mav,
                            aes(x = selected.table.mav$Date,
                                y=selected.table.mav$Views))+geom_line()
        #+geom_line(aes(selected.table.mav$Date,
         #             selected.table.mav$MA),
          #        color="red") +ylim(400,1500)








selected.view.plot<- ggplot(selected.table,
                                aes(x = selected.table$Date,
                                    y=selected.table$Views))+geom_point(color = "red", , size = 0.5)+
        ylim(400, 1600)+
        stat_smooth(color = "black", span = .02)
selected.close.plot <- ggplot(selected.table,
                              aes(x = selected.table$Date,
                                  y=selected.table$Close),)+geom_line( color = "blue")+
        theme(panel.background = element_rect(fill = NA))
        
p1<-selected.view.plot
p2<-selected.close.plot

#p1 <- ggplot(, aes(mpg, disp)) + geom_line(colour = "blue") + theme_bw()
#p2 <- ggplot(mtcars, aes(mpg, drat)) + geom_line(colour = "red") + theme_bw() %+replace% 
        theme(panel.background = element_rect(fill = NA))



grid.newpage()
# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)

