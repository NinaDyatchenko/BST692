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
library(gmodels)
data(burn1000)

# Normalizing data

normalize <- function(x){
  return((x-min(x))/(max(x)- min(x)))
}


#### Creating Training and Test datasets:

burnNorm <- burn1000 %>% 
  #select(tbsa, age) %>% 
  map(., as.numeric) %>% 
  map(., normalize) %>% 
  as.data.frame()

summary(burnNorm$age)


trainingSet <- unlist(caret::createDataPartition(burn1000$death, p = 0.80))
trainingSet

burn_train <- burnNorm[trainingSet,]

burn_test <- burnNorm[-trainingSet, ]

burn_train1 <- burnNorm[trainingSet,]

burn_test2 <- burnNorm[-trainingSet, ]

burn_train1_isdead <- burn_train1$death
burn_test2_isdead <- burn_test2$death
burn_train1 <-  burn_train1 %>% select(-death)
burn_test2 <-  burn_test2 %>% select(-death)



### Deployment to web:

#library(rsconnect)

#rsconnect::setAccountInfo(name='ninadyatchenko',
 #                         token='63F0AA52207F5958366083BB7500FBA2',
  #                        secret='E/M3TSZm1CC3189NIdMr7lb2XPQf2wnFK8BR+rAp')
#rsconnect::deployApp()


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
            
              
            menuItem("Predictions Models", tabName = "Regressions")
        )
    ),

    
    dashboardBody(
       
         tabItems(
            tabItem(tabName = "Statistics", 
                    
                    
                     tabsetPanel(
                         tabPanel("Age at admission",
        
                                  
                                  h1("Input age plots"), 
                                  box(title = "Distibution of Age at admission",
                                      plotlyOutput("summaryage", height = 300),
                                      
                                      width = 7), 
                                  
                                  box(title = "2",
                                      verbatimTextOutput("tableage"),
                                      width = 5)
                                  
                                  ), # end of Age panel
                         
                         tabPanel("Gender", 
                                  
                                  
                                  h3("Imput Gender plots"),
                                  box(
                                      plotOutput("plotgender", height = 300),
                                      
                                      width = 7)
                                  
                                  ), # end of Gender panel
                         tabPanel("Race",
                                  
                                  h3("plots"),
                                
                                    box(
                                      plotOutput("plotrace", 
                                                 height = 300),
                                      width = 7)
                                  ), # end of Race panel
                         
                         tabPanel("Total burn surface area", 
                                  
                                  h3("Area"),
                                  box(plotlyOutput("plottbsa", 
                                                   height = 300),
                                      
                                      width = 7),
                                 
                         
                         box(title = "2",
                             verbatimTextOutput("tabletbsa"),
                             width = 5)
                         
                     ), # end of tbsa panel
                         
                         tabPanel("Burn involved inhalation injury", 
                                  
                                  h3("inh"),
                                  box(
                                      plotOutput("inhinj",
                                                 height = 300),
                                      width = 7)
                                  ), # end of inhalation panel
                                 
                         
                         tabPanel("Flame involved in burn injury",
                                  
                                  h3("plots"),
                                  box(
                                      plotOutput("flameinj",
                                                 height = 300),
                                      width = 7)
                                  ), # end of flame panel
                         
                         tabPanel("Hospital discharge status", 
                                  
                                  
                                  h3("death"),
                                  
                                  box(
                                      plotOutput("isdead",
                                                 height = 300),
                                      width = 7)
                                  ), # end of death panel
                     ), # end of vertical tabset
            ), # end of tabItem Stats
            
                     tabItem(tabName = "Regressions", 
                             
                             h1("Input KNN"), 
                             box(title = "KNN",
                                 verbatimTextOutput("knn_table"),
                                 
                                 width = 7) 
                     
                                  
                         ) # end of Regression Panel
                         
                     ) # end of TabItems Stats and Regr
                    ) # end of DashboardBody
                ) # end of DashboardPage
        


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
        
     
        output$knn_table <- renderPrint({
          burn_test_pred <- knn(train = burn_train1, 
                                test = burn_test2, 
                                cl = burn_train1_isdead,  # true class of training
                                k = 21)
          
          # Evaluate KNN prediction model:
          
          
          CrossTable(x = burn_test2_isdead, y = burn_test_pred, prop.chisq = FALSE)
          # 13 false negatives from 1000 people
          
          # Getting Kappa of KNN:
          
          library(caret)
          confusionMatrix(as.factor(burn_test_pred), 
                          as.factor(burn_test2_isdead), 
                          positive = "1")
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
