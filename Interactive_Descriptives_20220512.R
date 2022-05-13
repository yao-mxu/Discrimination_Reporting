# Yao Xu
# Discrimination Reporting
# Interactive Descriptives
# 5/12/22
options(download.file.method = "wininet")
shiny::runGitHub(repo = 'Discrimination_Reporting',username ='yx1441')


devtools::install_github("yx1441/Discrimination_Reporting@master")


# source("global.R")
#install.packages("radiant.data")
#install.packages("radiant", repos = "https://radiant-rstats.github.io/minicran/")
library(radiant.data)
library(shiny)
library(tidyverse);library(devtools);library(readtext);library(xtable);library(janitor);library(cowplot);library(survey);library(ggmap)
library(ggplot2);library(reshape2);library(data.table);library(openxlsx);library(rlang);library(ggpubr);library(rgdal);library(spdep)
library(maptools);library(tigris);library(leaflet);library(sf);library(maps);library(janitor);library(censusxy);library(dplyr);library(rgeos)
library(geosphere); require(dplyr) 

load_url <- function (url, ..., sha1 = NULL) {
    # based very closely on code for devtools::source_url
    stopifnot(is.character(url), length(url) == 1)
    temp_file <- tempfile()
    on.exit(unlink(temp_file))
    request <- httr::GET(url)
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)
    file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
    if (is.null(sha1)) {
        message("SHA-1 hash of file is ", file_sha1)
    }
    else {
        if (nchar(sha1) < 6) {
            stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
        }
        file_sha1 <- substr(file_sha1, 1, nchar(sha1))
        if (!identical(file_sha1, sha1)) {
            stop("SHA-1 hash of downloaded file (", file_sha1, 
                 ")\n  does not match expected value (", sha1, 
                 ")", call. = FALSE)
        }
    }
    load(temp_file, envir = .GlobalEnv)
}

githubURL<-"https://github.com/yx1441/Discrimination_Reporting/blob/32862e023bc5a74444680df80720b9ab8c82d659/reporting_test_20220512.RData?raw=true"
load(url(githubURL))
# source(githubURL)

year_full<-c(2014,2015,2016,2017,2018,2019)
year_4<-c(2015,2016,2017,2018)

year_choice<-c("2015-2018","2014-2019")

time_series<-function(ba_category, year){
    see<-reporting_test[reporting_test$complaint_year %in% year,] %>% group_by(mth_case_file_date_new) %>%  mutate(percentage_month = mean(eval(parse(text=ba_category)))*100) %>% select(mth_case_file_date_new, percentage_month, complaint_year)%>% distinct()
    ggplot(see, aes(x=mth_case_file_date_new,y=percentage_month,fill=complaint_year)) + geom_bar(stat="identity")+xlab("Months")+ylab(paste0("Cases Filed by Percentage: ",ba_category)) +theme(#panel.grid.major = element_blank(),
        axis.text.x=element_text(angle = 40, hjust=0.5,vjust=0.5,size = rel(1)),
        axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
        axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
        legend.position  = "right",
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))# +theme(axis.text.x = element_text(angle = 90,size=6, vjust = 0.5, hjust=1))
}
time_series("ba_race",year_full)
# Application title
# titlePanel(),
# Sidebar with a slider input for number of bins 
# selectInput("variable", "Select a year to visualize", choices = c("2014", "2015","2016","2017","2018","2019")),

ba_all <-colnames(reporting_test)[str_detect(colnames(reporting_test),"ba_")]
har_all <-colnames(reporting_test)[str_detect(colnames(reporting_test),"har_")]

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

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$basisPlot <- renderPlot({
        if (input$year=="2014-2019"){
            time_series(input$basis,year_full)+ylim(0,100)
        }
        if (input$year=="2015-2018"){
            time_series(input$basis,year_4)+ylim(0,65)
        }
    })
    output$harmPlot <- renderPlot({
        if (input$year=="2014-2019"){
            time_series(input$harm,year_full)#+ylim(0,65)
        }
        if (input$year=="2015-2018"){
            time_series(input$harm,year_4)#+ylim(0,65)
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

