library(shinydashboard)
library(shiny)
library(devtools)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(rsconnect)


##################DATA###############################################

Sales_Data <- reactive({ read_csv("Sales Data.csv")})

colSums(is.na(Sales_Data))

#clean missing values as we do not need that data
Sales_Data <- Sales_Data[complete.cases(Sales_Data), ]
colSums(is.na(Sales_Data))


Regionsales <- Sales_Data %>% select(Publisher, NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales)
regsale <- Regionsales %>% group_by(Publisher) %>% summarise(., across(everything(), ~sum(.)))


#graph of game Genre by Publisher
PubGenre <- Sales_Data%>%
    group_by(Genre,Publisher)%>%
    summarise(count=n())%>%
    filter(count>20)%>%
    arrange((count))%>%
    ggplot(aes(Publisher,count,fill=Genre)) +
    geom_bar(stat="identity") +coord_flip()  +
    theme(legend.position = "top") 

PubGenre

#total NA_sales of all publishers

sale1 <- Sales_Data%>%
    select(Publisher,NA_Sales)%>%
    group_by(Publisher)%>%
    summarize(., across(everything(), ~sum(.)))%>%
    filter(NA_Sales>50)%>%
    ggplot(aes(Publisher,NA_Sales,fill=NA_Sales)) +geom_bar(stat="identity") + coord_flip()

sale1

#total EU_sales of all publishers
sale2 <- Sales_Data%>%
    select(Publisher,EU_Sales)%>%
    group_by(Publisher)%>%
    summarize(., across(everything(), ~sum(.)))%>%
    filter(EU_Sales>50)%>%
    ggplot(aes(Publisher,EU_Sales,fill=EU_Sales)) +geom_bar(stat="identity") + coord_flip()

sale2

#total JP_sales of all publishers
sale3 <- Sales_Data%>%
    select(Publisher,JP_Sales)%>%
    group_by(Publisher)%>%
    summarize(., across(everything(), ~sum(.)))%>%
    filter(JP_Sales>25)%>%
    ggplot(aes(Publisher,JP_Sales,fill=JP_Sales)) +geom_bar(stat="identity") + coord_flip()

sale3

#total Other_sales of all publishers
sale4 <- Sales_Data%>%
    select(Publisher,Other_Sales)%>%
    group_by(Publisher)%>%
    summarize(., across(everything(), ~sum(.)))%>%
    filter(Other_Sales>50)%>%
    ggplot(aes(Publisher,Other_Sales,fill=Other_Sales)) +geom_bar(stat="identity") + coord_flip()

sale4

#total Global_sales of all publishers

sale5 <- Sales_Data%>%
    select(Publisher,Global_Sales)%>%
    group_by(Publisher)%>%
    summarize(., across(everything(), ~sum(.)))%>%
    filter(Global_Sales>50)%>%
    plot_ly(labels = ~Publisher, values = ~Global_Sales) %>%
    add_pie(hole = 0.5) %>%
    layout(title = "Global Sales of Publishers",  showlegend = F,
           xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = F))

sale5



#######################################################################################################






#################################UI#################################################################
## ui.R ##

header <- dashboardHeader(title = "Do Publishers Perform Better in Different Regions?",
                          titleWidth = 650,
                          tags$li(class= "dropdown", tags$a (href="https://github.com/joeydlf/Project-2", "Source Code", target= "_blank")
                          ))


side <- dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                menuItem("Analysis", tabName = "An", icon = icon("address-card")),
                menuItem("Dataset", tabName = "Data", icon = icon("database")),
                menuItem(text = 'Visualization', tabName = "viz"),
                menuItem( text = "Interactive/COULDNOTPRODUCE", tabName = "inter")
    )
)


body <-  dashboardBody(
    tabItems(
        tabItem(tabName = "An",
                tabBox(id="t2", width = 12, 
                       tabPanel("About", icon = icon("address-card"), h4("Using the same dataset on videogame sales as Project 1, I wanted to further my original analysis to see if publishers performed differently in other regions. The regions listed are North American, European, Japanese, and other. 
                                                                     The dataset and code used can be found in my Github linked above."),h4(
                                                                         
                                                                         "                                                                                      
                                                                    I was unable to produce an interactive graph in the time assigned for the project. I ran into many troubles in rendering my visualizations that I neglected finding a proper route to create the interactive graph.")),
                       tabPanel(title = "Analysis", icon = icon("address-card"), h4( "For my first project, I wanted to see if there was a relationship between units sold and review scores. I found that there was a stronger relationship between critic scores and sales versus user scores. When researching the data, I wanted to further analyze my topic by seeing if the publishers listed performed differently per region where units are sold. The date range for the data goes from 1985 to 2016, and the three main regions are North America, Europe, and Japan. Other are the regions that don’t fit into those categories like the middle east, Southeast Asia, and south America. I have also included data visualizations for global sales."), 
                                h4( "I first began my analysis by producing a graph visualizing the different genres the top publishers specialized in. For instance, Electronic Arts produced more sports games than the rest of the publishers combined, Warner Bros Interactive, Capcom, Activision, Koei Tecmo, and Ubisoft were leaders in action type games, Nintendo focused on family friendly experiences, and Square Enix primarily focused on role playing. This visualization gives us a good idea on what each publisher brings to market."),
                                h4( "Looking into North American sales, 17.5% of games sold came from Electronic Arts with Nintendo following behind at 13.6% in the given time period. Taking what we have learned from the first visualization, we can estimate that a large portion of games sold were sports and action titles. We can also see that most of the companies with the highest percentages with units sold are western publishers.   With only 3 of the companies (Nintendo, Sega, and Sony Computer Interactive) in the top 10 being Japanese. Europe and the Other regions showed a very similar pattern of sales to North America. One interesting observation to note, however, Ubisoft, a French company, sold a higher number of units in Europe as opposed to North America."),
                                h4( "Japan shows a major shift from the patterns set by the other regions. Almost 40% of games sold in Japan come from Nintendo alone. That’s about the same number of units sold by them in the other regions combined. EA was the western developer with the most units sold at around 2.5%. The other top selling publishers in Japan were Japanese founded companies marking the difference in tastes."),
                                h4( "Why is it that Japanese publishers barely made dents in the other regions? Does the data show fundamental differences in video game preferences? Until recently, Microsoft, and its XBOX, had barely any presence in Japan. The only consoles dominating the marketplace were Nintendo and Playstation, which were paired almost exclusively with Family, Role-playing, and action games. As seen by the data, sports games hardly made a mark in the sales numbers. America and the other regions, however, had the Xbox, which was predominantly filled with games published by American companies like EA, Take Two, and Activision. This contributed to the sales numbers by giving another stream of revenue to these publishers that was not available in Japan."),
                                h4(	"One aspect I would have liked to include was visualizations of the different regions and the genres that sold the most. I would have also liked to get recent data and focus on a smaller date range as the data I used was outdated and bloated. Due to personal and time issues, I was not able to produce an interactive graph for the relevant data.")           ),
                )
        ),
        
        
        tabItem(tabName = "Data",
                tabBox(id="t1", width = 12, 
                       tabPanel(title = "Dataset", icon = icon("address-card"),dataTableOutput("Table")),
                       tabPanel(title = "structure", icon = icon("address-card"), verbatimTextOutput("Structure")),
                       tabPanel(title = "Summary", icon = icon("address-card"), verbatimTextOutput("summary")),
                )
        ),
        
        
        tabItem(tabName = "viz",
                tabBox(id="t2", width = 12,
                       
                       tabPanel(title = "Genre Count/Publisher", value = "trends", plotOutput("gen")),
                       tabPanel(title = "North America Sales", value = "trends", plotlyOutput("nas")),
                       tabPanel(title = "Europe Sales", value = "trends", plotlyOutput("eus")),
                       tabPanel(title = "Japan Sales", value = "trends", plotlyOutput("jps")),
                       tabPanel(title = "Other", value = "trends", plotlyOutput("ots")),
                       tabPanel(title = "Global", value = "trends", plotlyOutput("gls")),
                       
                )
        ),
        
        tabItem(tabName = "inter",
                tabBox(id="t2", width = 12,
                       tabPanel( title = "UNABLE TO PRODUCE",
                                 selectInput(inputId = "sel_Region",
                                             label = "Choose Region",
                                             list("North America", "Eruope", "Japan", "Other", "Global")),
                                 plotOutput("plot"),
                       ),
                       
                ),
                
        )
        
        
    )
)

ui <- dashboardPage(header, side, body)

###########################################SERVER#########################################

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

runApp()
deployApp()

