library(shinydashboard)
library(shiny)
library(devtools)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(rsconnect)


server <- function(input, output) {
  
  
    #Structure
  output$Structure <- renderPrint(
    Sales_Data%>%
      str()
  )
  
    #Summary
    output$summary <- renderPrint(
      Sales_Data %>%
        summary()
    )
    
    #Data
    output$Table <- renderDataTable(
      Sales_Data
    )
    
    #Charts
    output$gls <- renderPlotly({
      G <- plot_ly( regsale, labels = ~Publisher, values = ~Global_Sales, type = "pie")
        
        G <- G %>% layout(title = "Global Sales of Publishers",  showlegend = F,
               xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
               yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
      print(G)
        })
    
    output$nas <- renderPlotly({
      N <- plot_ly( regsale, labels = ~Publisher, values = ~NA_Sales, type = "pie")
      
      N <- N %>% layout(title = "North American Sales of Publishers",  showlegend = F,
                        xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
                        yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
      print(N)
      })
    
    output$eus <- renderPlotly({
      E <- plot_ly( regsale, labels = ~Publisher, values = ~EU_Sales, type = "pie")
      
      E <- E %>% layout(title = "European Sales of Publishers",  showlegend = F,
                        xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
                        yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
      print(E)
    })
    output$jps <- renderPlotly({
      J <- plot_ly( regsale, labels = ~Publisher, values = ~JP_Sales, type = "pie")
      
      J <- J %>% layout(title = "Japanese Sales of Publishers",  showlegend = F,
                        xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
                        yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
      print(J)
    })
    output$ots <- renderPlotly({
      O <- plot_ly( regsale, labels = ~Publisher, values = ~Other_Sales, type = "pie")
      
      O <- O %>% layout(title = "Other Region Sales of Publishers",  showlegend = F,
                        xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
                        yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
      print(O)
    })
    
    
    
    
    output$gen <- renderPlot({
      Sales_Data %>%
        group_by(Genre,Publisher) %>%
        summarise(count=n()) %>%
        filter(count>10) %>%
        arrange((count)) %>%
        ggplot(aes(Publisher,count,fill=Genre)) +
        geom_bar(stat="identity") +coord_flip()  +
        theme(legend.position = "top")
    })
    
    
    
    
}

      
    
