#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#

library(shiny)
library(shinydashboard)
library(aplore3)
library(ggplot2)
library(plotly)
library(tidyverse)
data(burn1000)

### Deployment to web:

#library(rsconnect)

rsconnect::setAccountInfo(name='ninadyatchenko',
                          token='63F0AA52207F5958366083BB7500FBA2',
                          secret='E/M3TSZm1CC3189NIdMr7lb2XPQf2wnFK8BR+rAp')

rsconnect::deployApp()


ui <- dashboardPage(skin = "blue",
                    
                    
                    dashboardHeader(titleWidth = 190,
                                    title = "Nina Dyatchenko"
                                    
                    ),
                    
                    
                    dashboardSidebar(
                        sidebarUserPanel("BST692 Final Exam", subtitle = "Burn Dataset"),
                        
                        collapsed = FALSE,
                        width = 190,
                        
                        sidebarMenu(
                            menuItem("Summary Statistics", tabName = "Statistics"),
                                menuSubItem("Age at admission",
                                            icon = icon("angle-right")),
                            
                            
                            
                            
                            
                            menuItem("Predictions Models", tabName = "test2")
                        )
                    ),
                    
                    
                    dashboardBody(
                        
                        tabItems(
                            tabItem(tabName = "Statistics", 
                                    
                                    tabsetPanel(
                                        tabPanel("Age at admission",
                                                 
                                                 
                                                 h1("Imput age plots"), 
                                                 box(title = "Distibution of Age at admission",
                                                     plotlyOutput("summaryage", height = 300),
                                                     
                                                     width = 7), 
                                                 
                                                 box(title = "2",
                                                     verbatimTextOutput("tableage"),
                                                     width = 5)
                                                 
                                        ),
                                        tabPanel("Gender", 
                                                 
                                                 
                                                 h3("Imput Gender plots"),
                                                 box(
                                                     plotOutput("plotgender", height = 300),
                                                     
                                                     width = 7)
                                                 
                                        ),
                                        tabPanel("Race",
                                                 
                                                 h3("plots"),
                                                 
                                                 box(
                                                     plotOutput("plotrace", 
                                                                height = 300),
                                                     width = 7)
                                        ),
                                        
                                        tabPanel("Total burn surface area", 
                                                 
                                                 h3("Area"),
                                                 box(plotlyOutput("plottbsa", 
                                                                  height = 300),
                                                     
                                                     width = 7),
                                                 
                                                 
                                                 box(title = "2",
                                                     verbatimTextOutput("tabletbsa"),
                                                     width = 5)
                                                 
                                        ),
                                        
                                        tabPanel("Burn involved inhalation injury", 
                                                 
                                                 h3("inh"),
                                                 box(
                                                     plotOutput("inhinj",
                                                                height = 300),
                                                     width = 7)
                                        ),
                                        
                                        
                                        tabPanel("Flame involved in burn injury",
                                                 
                                                 h3("plots"),
                                                 box(
                                                     plotOutput("flameinj",
                                                                height = 300),
                                                     width = 7)
                                        ), 
                                        
                                        tabPanel("Hospital discharge status", 
                                                 
                                                 
                                                 h3("death"),
                                                 
                                                 box(
                                                     plotOutput("isdead",
                                                                height = 300),
                                                     width = 7)
                                        )
                                        
                                        
                                    )
                                    
                            )
                        )
                    )
                    
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$summaryage <- renderPlotly({
        ageplot <-  ggplot(burn1000, aes(x = age)) +
            geom_density(fill = "lightgray", bw = 1) + 
            scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
            scale_y_continuous(breaks = 0) +
            theme_minimal() +
            labs(x = "Age (years)", y = "")
        
        ggplotly(ageplot, tooltip = "age")
    })
    
    output$tableage <- renderPrint({  
        round(summary(burn1000$age), 0)
        #align - center
    })
    
    
    output$plotgender <- renderPlot({
        GendVar <- ggplot(data = burn1000, 
                          aes(x = gender)) + 
            geom_bar(width = 0.6, 
                     fill = c("lightpink", "lightblue"), color = "gray") +
            theme_minimal() +
            
            scale_y_continuous(breaks = seq(from = 0, to = 750, by = 300)) +
            geom_text(stat = 'count', aes(label = ..count..), vjust = 2) +
            theme_minimal() +
            labs(title = "Gender", x = "", y = "Count")
        GendVar
    })
    
    
    output$plotrace <- renderPlot({
        RaceVar <- ggplot(data = burn1000, aes(x = race)) + 
            geom_bar(fill = c("lightgray", "white"), 
                     color = "black", 
                     width = 0.6) +
            theme_minimal() +
            
            scale_y_continuous(breaks = seq(from = 0, to = 750, by = 100)) +
            geom_text(stat = 'count', aes(label = ..count..), vjust = 2) +
            labs(title = "Race", x = " ", y = "Count")
        RaceVar
    })
    
    
    output$plottbsa <- renderPlotly({
        tbsplot <-  ggplot(burn1000, aes(x = tbsa)) +
            geom_density(fill = "lightgray", bw = 1) + 
            scale_y_continuous(breaks = 0)+
            scale_x_continuous(breaks = seq(0, 100, 10))+
            theme_minimal() +
            labs(title = "Total burn surface area", x = "Total burn surface area (%)", y = " ")
        ggplotly(tbsplot, tooltip = "tbsa")
    })
    
    output$tabletbsa <- renderPrint({  
        round(summary(burn1000$tbsa), 0)
    })
    
    
    
    output$inhinj <- renderPlot({
        InhInjVar <- ggplot(data = burn1000, aes(x = inh_inj)) + 
            geom_bar(fill = c("lightgray", "darkgray"), width = 0.6) +
            theme_minimal() +
            scale_y_continuous(breaks = seq(from = 0, to = 900, by = 100)) +
            geom_text(stat = 'count', aes(label = ..count..), vjust = 2) +
            labs(title = "Burn involved inhalation injury", 
                 x = "Burn involved inhalation injury", 
                 y = "Count")
        InhInjVar
    })
    
    output$flameinj <- renderPlot({
        FlameVar <- ggplot(data = burn1000, aes(x = flame)) + 
            geom_bar(fill = c("lightgray", "darkgray"), width = 0.6) +
            theme_minimal() +
            scale_y_continuous(breaks = seq(from = 0, to = 900, by = 100)) +
            geom_text(stat = 'count', aes(label = ..count..), vjust = 2) +
            labs(title = "Flame involved in burn injury", 
                 x = "Flame involved in burn injury", 
                 y = "Count")
        FlameVar
    })
    
    
    
    output$isdead <- renderPlot({
        DeathVar <- ggplot(data = burn1000, aes(x = death)) + 
            geom_bar(fill = c("lightgray", "red"), width = 0.6) +
            theme_minimal() +
            scale_y_continuous(breaks = seq(from = 0, to = 900, by = 100)) +
            geom_text(stat = 'count', aes(label = ..count..), vjust = 2) +
            labs(title = "Life status", 
                 x = "Status", 
                 y = "Count")
        DeathVar
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
