
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
library(caret)
library(class)
library(rattle)					# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)		# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)
library(randomForest)
#library(reprtree)
library(party)
library(e1071)

source("appAnalysis.R")
url <- "https://www.rdocumentation.org/packages/aplore3/versions/0.7/topics/burn1000"

## Building Graphical User Interface (GUI)

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(titleWidth = 190,
                                    title = "Nina Dyatchenko"
                                    ),  # end of dashboardHeader
                    
                    dashboardSidebar(
                        collapsed = FALSE,
                        width = 190,
                        
                        sidebarMenu(
                          menuItem("Dataset Introduction", tabName = "Dataset"),
                          
                          menuItem("Summary Statistics", tabName = "Statistics"),
                          
                          menuItem("Prediction Models", tabName = "Predictions")

                        ) # end of sidebarMenu
                    ), # end of dashboardSidebar
                    
                    
                    dashboardBody(
                        
                        tabItems(
                          tabItem(tabName = "Dataset",
                               #   box(
                                  p("The dataset Burn1000 describes demographic 
              characteristics, injury level, and hospital discharge status of 
              1000 patients. Using the data and machine learning algoritms, I 
              construct and evaluate various prediction models that help foretell
              hospital discharge status.",
              br(),
              br(),
              "Read more about the source of the dataset in this",
   a(href="https://www.rdocumentation.org/packages/aplore3/versions/0.7/topics/burn1000", 
                                      "link"), ".", 
                                    style = "font-size:18px")
                          #        )
                          ),
                          
                            tabItem(tabName = "Statistics", 
                                    
                                    tabsetPanel(
                                        tabPanel("Age at admission",

                                                 box(title = "Distibution of Age at admission:",
                                                     style = "font-size:18px",
                                                     plotlyOutput("summaryage", 
                                                                  height = 300),
                                                      width = 7), 
                                                 
                                                 box(title = "Statistics of Age at admission (years)",
                                                     verbatimTextOutput("tableage"),
                                                     width = 5)
                                                 
                                        ), # end of Age panel
                                        
                                        tabPanel("Gender", 

                                                 box(plotOutput("plotgender", 
                                                                height = 300),
                                                     width = 7)
                                                 ), # end of Gender panel
                                        
                                        tabPanel("Race",

                                                 box(plotOutput("plotrace", 
                                                                height = 300),
                                                     width = 7)
                                        ), # end of Race panel
                                        
                                        tabPanel("Total burn surface area", 

                                                 box(plotlyOutput("plottbsa", 
                                                                  height = 300),
                                                     width = 7),
                                                 
                                                 box(title = "Statistics of Total burn surface area (%)",
                                                     verbatimTextOutput("tabletbsa"),
                                                     width = 5)
                                                 
                                        ), # end of tbsa panel
                                        
                                        tabPanel("Burn involved inhalation injury", 

                                                 box(plotOutput("inhinj",
                                                                height = 300),
                                                     width = 7)
                                        ), # end of inhalation panel
                                        
                                        tabPanel("Flame involved in burn injury",

                                                 box(plotOutput("flameinj",
                                                                height = 300),
                                                     width = 7)
                                        ), # end of flame panel
                                        
                                        tabPanel("Hospital discharge status", 

                                                 box(plotOutput("isdead",
                                                                height = 300),
                                                     width = 7)
                                        ) # end of Discharge status tabPanel
                                      
                                    ) # end of tabsterPanel (vertical panels within Stats)
                            ),# end of tabItem Stats
                            
                            
                            tabItem(tabName = "Predictions", 
                                    
                                    tabsetPanel(
                                        tabPanel("K Nearest Neighbor ",
    p("K Nearest Neighbor (KNN) is a supervised machine learning algorithm. Between 
    all models presented here, KNN model is most accurate (accuracy = 94%). The 
    Cohen's Kappa of >0.74 shows a substential agreement and represents the 
    extent to which the data collected in the study are correct representations 
    of the variables measured.",
      br(),
      "21 nearest neighbors were used in this model.", 
      style = "font-size:18px"),
                                                 box(
                                                     verbatimTextOutput("knn_table"))
                                        ), # end of KNN tabset
                                        
                                        tabPanel("Logistic Regression",
       p("Logistic Regression is a machine learning algorithm which is used for 
       the classification problems, it is a predictive analysis algorithm and 
       based on the concept of probability. The Logistic Regression model is 
       90.5% accurate with a moderate reliability Cohen’s Kappa statistic of ~0.51.", 
         style = "font-size:18px"),
                                                 box(
                                                     verbatimTextOutput("logistic")) 
                                        ), # end of logistic tabset
                                        
                                    
                                        tabPanel("Decision Tree",
                                                
        p("The Decision Tree model predicts the chance of person to be dead at 
          discharge from the hospital based on total burn surface area (tbsa), 
          burn involved inhalation injury, and age. This model has lowest 
          accuracy and reliability.", 
          style = "font-size:18px"),
                                                  box(width = 800,
                                                      plotOutput("tree", width = 800))
                                                  
                                                  
                                        ), # end of tree tabset
                                        
                                        tabPanel("Random Forest",
        p("Random forest consists of a large number of individual decision trees 
        that operate as an ensemble. Each individual tree in the random forest 
        spits out a class prediction and the class with the most votes becomes 
          our model’s prediction. The simple Random Forest was performed using 
          all variables of the data and is second most accurate and reliable 
          (accuracy is 93% and Kappa 0.70).",
         style = "font-size:18px"),
                                                 box(title = "Random Forest", 
                                                     #height = 300,
                                                     verbatimTextOutput("rf"))
                                                 
                                        ) # end of RF tabset
                                    ) # end of tabsetS for Decision
                                )  # end of tabItem Tree
                        ) # end of tabItemS
                   ) # end of dashboardBody
) # end of dashboardPage

# Define server logic 
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
            labs(title = "Number of Males and Females in the sample:", x = "", y = "Count")
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
            labs(title = "Number of Non-White and White race subjects in the sample:", 
                 x = " ", y = "Count")
        RaceVar
    })
    
    
    output$plottbsa <- renderPlotly({
        tbsplot <-  ggplot(burn1000, aes(x = tbsa)) +
            geom_density(fill = "lightgray", bw = 1) + 
            scale_y_continuous(breaks = 0)+
            scale_x_continuous(breaks = seq(0, 100, 10))+
            theme_minimal() +
            labs(title = "Total burn surface area in percent:", 
                 x = "Total burn surface area (%)", y = " ")
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
            labs(title = "Number of burn injuries involved inhalation:", 
                 x = "Burn injury involved inhalation", 
                 y = "Count")
        InhInjVar
    })
    
    output$flameinj <- renderPlot({
        FlameVar <- ggplot(data = burn1000, aes(x = flame)) + 
            geom_bar(fill = c("lightgray", "darkgray"), width = 0.6) +
            theme_minimal() +
            scale_y_continuous(breaks = seq(from = 0, to = 900, by = 100)) +
            geom_text(stat = 'count', aes(label = ..count..), vjust = 2) +
            labs(title = "Number of burn injuries involving flame:", 
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
            labs(title = "Life status upon discharge from the hospital:", 
                 x = "Status", 
                 y = "Count")
        DeathVar
    })
    
    
    
    # Getting Kappa of KNN:
    output$knn_table <- renderPrint({
        #   library(caret)
        knntab <-  confusionMatrix(as.factor(burn_test_pred), 
                                   as.factor(burn_test2_isdead), 
                                   positive = "1")
        knntab
    })
    
    output$logistic <- renderPrint({
        # Evaluate GLM prediction model:
        caret::confusionMatrix(predicted, 
                               truth, 
                               positive = "1")
    })
    
    output$tree <- renderPlot({
        rpart.plot(cartdata.hp, box.palette = c("Reds"), 
                   under.cex = 1.4, cex.main = 1.5,
                   type = 4)
    })
    
    output$rf <- renderPrint({
        caret::confusionMatrix(burn_rf_pred , 
                               truth_f, 
                               positive = "1")
        
        
        ### run on train, c50.0 on test, then predict on test, then confusionmatrix
    })
}

# Run the application 
shinyApp(ui = ui, server = server)