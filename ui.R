# Yao Xu
# Discrimination Reporting
# Interactive Descriptives
# 5/12/22
# options(download.file.method = "wininet")
# shiny::runGitHub(repo = 'Discrimination_Reporting',username ='yx1441')
# devtools::install_github("yx1441/Discrimination_Reporting@master")

# source("global.R")
#install.packages("radiant.data")
#install.packages("radiant", repos = "https://radiant-rstats.github.io/minicran/")
library(shiny)
library(tidyverse);library(devtools);library(readtext);library(xtable);library(janitor);library(cowplot);library(survey);library(ggmap)
library(ggplot2);library(reshape2);library(data.table);library(openxlsx);library(rlang);library(ggpubr);library(rgdal);library(spdep)
library(maptools);library(tigris);library(leaflet);library(sf);library(maps);library(janitor);library(censusxy);library(dplyr);library(rgeos)
library(geosphere); require(dplyr) 

githubURL<-"https://github.com/yx1441/Discrimination_Reporting/blob/32862e023bc5a74444680df80720b9ab8c82d659/reporting_test_20220512.RData?raw=true"
load(url(githubURL))
# source(githubURL)


# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("Employment Discrimination Reporting",
               tabPanel("Bases",selectInput("basis", "Select a basis to visualize", choices = ba_all),
                        selectInput("year", "Select which years to visualize", choices = year_choice)),
                                                       mainPanel(plotOutput("basisPlot")),
               tabPanel("Harms",
                        selectInput("harm", "Select a harm to visualize", choices = har_all),
               selectInput("year", "Select which years to visualize", choices = year_choice)),
               mainPanel(plotOutput("harmPlot")),
               tabPanel("Time Series",
                        selectInput("basis", "Select a basis to visualize", choices = ba_all)),
               #mainPanel(plotOutput("distPlot")),
               ))


