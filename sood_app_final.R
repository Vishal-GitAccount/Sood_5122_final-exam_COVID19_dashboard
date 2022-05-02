library(shinydashboard)
library(shiny)
library(tidyverse)
library(maps)
library(viridis)
library(tmap)
library(leaflet)
library(ggplot2)
library(tidytext)
library(tidylo)
library(dbplyr)
library(urbnmapr)
library(ggmap)
library(sqldf)
library(dplyr)
library(RColorBrewer)
library(shinythemes)
library(dplyr)
library(plotly)
library(ggthemes)
library(gridExtra)
library(scales)
library(lubridate)


case_daily <- readr::read_csv('data/case_daily_trends_united_states.csv',skip=3)
death_daily <- readr::read_csv('data/death_daily_trends_united_states.csv',skip=3)
covid_cases <- readr::read_csv('data/united_states_covid19_cases_and_deaths_by_state.csv',skip=3)

colnames(covid_cases)[1] = "NAME"
colnames(covid_cases)[2] = "Total_case"


total_cases <- sum(covid_cases$Total_case)
total_death <- sum(covid_cases$`Total Deaths`)
avg_count <- sum(covid_cases$`Cases in Last 7 Days`)

case_daily$Date <- format(as.Date(case_daily$Date,format="%b %d %Y"),"%Y-%b-%d")
case_daily <- case_daily %>% 
  mutate(Date=as.Date(Date, format="%Y-%b-%d"))

death_daily$Date <- format(as.Date(death_daily$Date,format="%b %d %Y"),"%Y-%b-%d")
death_daily <- death_daily %>% mutate(Date=as.Date(Date, format="%Y-%b-%d"))

plot1 <- ggplot(case_daily,aes(Date,`New Cases`)) +
  geom_bar(stat = "identity",na.rm=TRUE,color="light blue") +
  ggtitle("Daily Trends in Number of COVID-19 Cases in the United States Reported to CDC") +
  xlab("Date")+ylab("Cases") +
  scale_x_date(labels=date_format("%b"),breaks=date_breaks("2 month")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 10)) +
  theme(text = element_text(size=18)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot2 <- ggplot(death_daily,aes(Date,`New Deaths`)) +
  geom_bar(stat = "identity",na.rm=TRUE,color="red") +
  ggtitle("Daily Trends in Number of COVID-19 Deaths in the United States Reported to CDC") +
  xlab("Date") + 
  ylab("Deaths Number") +
  scale_x_date(labels=date_format("%b"),breaks=date_breaks("2 month")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 10)) +
  theme(text = element_text(size=18)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_dailycase <- ggplotly(plot1)
plot_deathcase <- ggplotly(plot2)

plot_deathcase
plot_dailycase

total_cases <- sum(covid_cases$Total_case)
total_death <- sum(covid_cases$`Total Deaths`)
avg_count <- sum(covid_cases$`Cases in Last 7 Days`)

states <- geojsonio::geojson_read("gz_2010_us_040_00_500k.json", what = "sp")
require(sp)

final_table <- merge(states,covid_cases,by="NAME",all.x=TRUE)

bin <- c(0,30000,110000,255000,492000,979000,1341000,5000000)

pal <- colorBin("Blues" ,domain=final_table$Total_case,bins=bin)


labels <- paste(                 
  final_table@data$NAME,"<br/>", 
  "Total Cases",final_table@data$Total_case,"<br/>",
  "Total Death",final_table@data$`Total Deaths`,"<br/>",
  ""
) %>%
  lapply(htmltools::HTML)

m <- leaflet(final_table)%>%
  setView(-96, 37.8, 4)%>%
  addProviderTiles("Stamen.Toner",options = providerTileOptions(noWrap = TRUE))%>%
  addPolygons( data=final_table,
               weight=1,
               color="#9ecae1",
               smoothFactor=1,
               dashArray=3,
               fillOpacity=0.7)
m <- m %>% addPolygons(
  fillColor = ~pal(Total_case),
  weight = 2,
  opacity = 1,
  color = "black",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#FFFFFF",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

m %>% addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Total Number of COVID-19 Cases in the US Reported to the CDC, by State",
                position = "bottomright")


ui <- dashboardPage(
  dashboardHeader(title = "More Information"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName = "mainpage",icon=icon("dashboard")),
      menuItem("Cases and Death by States",tabName = "Map", icon = icon("&#8505")),
      hr(),
      menuItem("Daily Cases Trends",tabName = "casetrend",icon=icon("&#8505")),
      hr(),
      
      menuItem("Daily Death Trends",tabName="deathtrend",icon=icon("&#8505"))
    )
  ),
  dashboardBody(
    tags$img(
      src = "https://southkingstownri.com/ImageRepository/Document?documentID=3809",
      height="95%",width="85%",
      style = 'position: absolute'
    ),
    tabItems(
      
      tabItem(tabName = "mainpage",
              h1(p(strong("United States COVID-19 Cases and Deaths by State"))),
              p(em("Reported to CDC until 2020")),
              fluidRow(box(title=h3(p(strong("Total Cases in US"))),hr(),h1(p(strong(total_cases))),height=200,width=3,background="light-blue")),
              fluidRow(box(title=h3(p(strong("Total Deaths in US"))),hr(),h1(p(strong(total_death))),height=200,width=3,background="red")),
              fluidRow(box(title=h3(p(strong("Total Cases in last 7 days"))),hr(),h1(p(strong(avg_count))),height=200,width=3,background="blue")),
              
      ),
      tabItem(tabName="Map",leafletOutput("map","100%",1000)),
      tabItem(tabName = "casetrend",plotlyOutput("plot1","100%",850)),
      tabItem(tabName = "deathtrend",plotlyOutput("plot2","100%",850))
      
    )
    
    
  ))

server <- function(input,output){
  output$map <- renderLeaflet({
    m
  })
  
  output$plot1 <- renderPlotly({
    plot_dailycase
  })
  
  output$plot2 <- renderPlotly({
    plot_deathcase
  })
  
}

shinyApp(ui,server)
