library(shinydashboard)
library(shiny)
library(devtools)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(rsconnect)

Sales_Data <- read_csv("Sales Data.csv")


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
