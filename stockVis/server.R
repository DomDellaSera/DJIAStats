# server.R
library(dplyr)
library(quantmod)
library(ggplot2)
library(gtable)
library(grid)
source("helpers.R")
source("/Users/Dominic/Documents/Programming/MySQL/Rconnection.R")

shinyServer(function(input, output) {
        

        output$plot <- renderPlot({
                
                sqlstatement<-paste("SELECT ",
                      "date,", input$metric,
                      " FROM wikiviews WHERE ",
                      "stocktickers ='",input$ticker, "' AND date > '",
                      input$dates[1],"' AND date < '",
                      input$dates[2],"' AND ",
                      input$metric, " > 0",
                      sep = "")
                sqlstatement.views<-paste("SELECT ",
                                    "date, views",
                                    " FROM wikiviews WHERE ",
                                    "stocktickers ='",input$ticker, "' AND date > '",
                                    input$dates[1],"' AND date < '",
                                    input$dates[2],"' AND ",
                                    input$metric, " > 0",
                                    sep = "")
                message(sqlstatement)
                message(input$dates[1])
                
                table <-dbGetQuery(conn = localcon,statement = sqlstatement)
                message(tail(table))
            message(length(table[,1]))
            message(length(table[,2]))
            
            table.views <-dbGetQuery(conn = localcon,statement = sqlstatement.views)
            message(str(table.views))
            
            
            
        p1 <- ggplot(data = table,
                       aes(x =as.Date(table[,1]), y=table[,2]))+
                        geom_line(color = "red")+ labs(x ="Date", y = input$metric)
                
        p2 <-ggplot(data = table.views,
                       aes(x =as.Date(table.views[,1]), y=table.views[,2]))+
                        geom_line(color = "blue")+theme_bw() %+replace% 
                theme(panel.background = element_rect(fill = NA)) #labs(x ="Date", y = input$metric)
            
            
        
            
            ######
        grid.newpage()
        
        # two plots
        # from http://rpubs.com/kohske/dual_axis_in_ggplot2
        
        #p1 <-ggplot(crimebytime,aes(dates,crime)) + geom_line(colour="red", size=2) + theme_bw()
        #p2 <-ggplot(weatherxts,aes(dates,temperature)) + geom_line(colour="blue", size=2) + theme_bw() %+replace% 
                theme(panel.background = element_rect(fill = NA))
        
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
            
                })
})
