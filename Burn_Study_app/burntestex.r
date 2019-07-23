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
                    
                    sidebar <- dashboardSidebar(
                      hr(),
                      sidebarMenu(id="tabs",
                                  menuItem("Stats", tabName="stats", icon=icon("line-chart"), selected=TRUE),
                                 
                                           menuSubItem("age", tabName = "age", icon = icon("angle-right")),
                                        #   menuSubItem("gender", tabName = "ui", icon = icon("angle-right")),
                                           menuSubItem("gender", tabName = "server", icon = icon("angle-right"))
                                  ),
                                  menuItem("Regress", tabName = "readme", icon=icon("mortar-board")),
                                  menuItem("About", tabName = "about", icon = icon("question"))
                      ),
                      hr(),

