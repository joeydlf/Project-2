library(shinydashboard)
library(shiny)
library(devtools)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(rsconnect)

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
