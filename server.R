# Yao Xu
# Discrimination Reporting
# Interactive Descriptives
# 5/12/22

library(shiny)
library(tidyverse);library(devtools);library(readtext);library(xtable);library(janitor);library(cowplot);library(survey);library(ggmap)
library(ggplot2);library(reshape2);library(data.table);library(openxlsx);library(rlang);library(ggpubr);library(rgdal);library(spdep)
library(maptools);library(tigris);library(leaflet);library(sf);library(maps);library(janitor);library(censusxy);library(dplyr);library(rgeos)
library(geosphere); require(dplyr) 

githubURL<-"https://github.com/yx1441/Discrimination_Reporting/blob/32862e023bc5a74444680df80720b9ab8c82d659/reporting_test_20220512.RData?raw=true"
load(url(githubURL))

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
# time_series("ba_race",year_full)
# Application title
# titlePanel(),
# Sidebar with a slider input for number of bins 
# selectInput("variable", "Select a year to visualize", choices = c("2014", "2015","2016","2017","2018","2019")),

ba_all <-colnames(reporting_test)[str_detect(colnames(reporting_test),"ba_")]
har_all <-colnames(reporting_test)[str_detect(colnames(reporting_test),"har_")]

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
# shinyApp(ui = ui, server = server)

