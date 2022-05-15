# Yao Xu
# Discrimination Reporting
# Interactive Descriptives
# 5/15/22

# runGitHub(repo = 'Discrimination_Reporting',username ='yx1441')

library(shiny);library(tidyverse);library(devtools);library(readtext);library(xtable);library(janitor);library(cowplot)
library(ggplot2);library(reshape2);library(data.table);library(openxlsx);library(rlang);library(ggpubr)
library(leaflet);library(janitor);library(dplyr);library(corrplot)

githubURL<-"https://github.com/yx1441/Discrimination_Reporting/blob/32862e023bc5a74444680df80720b9ab8c82d659/reporting_test_20220512.RData?raw=true"
load(url(githubURL))
reporting_test<- reporting_test %>% dplyr::mutate(record_type2=case_when(reporting_test$record_type=="Employment" ~ "Complaint Only",
                                                            reporting_test$record_type=="Right to Sue" ~ "Right to Sue Only"))
year_full<-c(2014:2019)
year_4<-c(2015:2018)

year_choice<-c("2015-2018","2014-2019")

time_series<-function(reporting_test, ba_category, year, y_axis, harmorbasis){
    if (harmorbasis=="Basis"){
        if (y_axis=="Percentage"){
            see<-reporting_test[reporting_test$complaint_year %in% year,] %>% group_by(mth_case_file_date_new) %>%  mutate(percentage_month = mean(eval(parse(text=ba_category)))*100) %>% select(mth_case_file_date_new, percentage_month, complaint_year)%>% distinct()
            ggplot(see, aes(x=mth_case_file_date_new,y=percentage_month,fill=complaint_year)) + geom_bar(stat="identity")+xlab("Months")+ylab(paste0("Cases Filed by Percentage")) +theme(#panel.grid.major = element_blank(),
                axis.text.x=element_text(angle = 40, hjust=0.5,vjust=0.5,size = rel(1)),
                axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
                axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
                legend.position  = "right",
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))+ylim(0,65)+ labs(fill = "Complaint Year")
            # +theme(axis.text.x = element_text(angle = 90,size=6, vjust = 0.5, hjust=1))
        }
        else {
            ggplot(reporting_test[reporting_test$complaint_year %in% year,], aes(y=eval(parse(text=ba_category)),x=mth_case_file_date_new,fill=complaint_year)) + geom_bar(stat="identity")+ylab(paste0("Cases Filed by Count"))+xlab("By Months") +theme(#panel.grid.major = element_blank(),
                axis.text.x=element_text(angle = 40,hjust=0.5,vjust=0.5,size = rel(1)),
                axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
                axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
                legend.position  = "right",
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))+ labs(fill = "Complaint Year")+ylim(0,800)
        }
    }
    
    else if (harmorbasis=="Harm"){
    if (y_axis=="Percentage"){
        see<-reporting_test[reporting_test$complaint_year %in% year,] %>% group_by(mth_case_file_date_new) %>%  mutate(percentage_month = mean(eval(parse(text=ba_category)))*100) %>% select(mth_case_file_date_new, percentage_month, complaint_year)%>% distinct()
        ggplot(see, aes(x=mth_case_file_date_new,y=percentage_month,fill=complaint_year)) + geom_bar(stat="identity")+xlab("Months")+ylab(paste0("Cases Filed by Percentage")) +theme(#panel.grid.major = element_blank(),
            axis.text.x=element_text(angle = 40, hjust=0.5,vjust=0.5,size = rel(1)),
            axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
            axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
            legend.position  = "right",
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))+ylim(0,80)+ labs(fill = "Complaint Year")
        # +theme(axis.text.x = element_text(angle = 90,size=6, vjust = 0.5, hjust=1))
        
    }
   else {
    ggplot(reporting_test[reporting_test$complaint_year %in% year,], aes(y=eval(parse(text=ba_category)),x=mth_case_file_date_new,fill=complaint_year)) + geom_bar(stat="identity")+ylab(paste0("Cases Filed by Count"))+xlab("By Months") +theme(#panel.grid.major = element_blank(),
        axis.text.x=element_text(angle = 40,hjust=0.5,vjust=0.5,size = rel(1)),
        axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
        axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
        legend.position  = "right",
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+ labs(fill = "Complaint Year")+ylim(0,1200)
   }}
    }

overall_time_series<-function(year, record_type2, time_unit){
    if (time_unit=="By quarters"){
        if (record_type2=="Full"){
            ggplot(reporting_test[reporting_test$complaint_year %in% year,], aes(x=qtr_case_file_date_new,fill=complaint_year)) + geom_bar(color="white")+ylab("Cases Filed")+xlab("By Quarters") +theme(#panel.grid.major = element_blank(),
                axis.text.x=element_text(angle = 40,hjust=0.5,vjust=0.5,size = rel(1)),
                axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
                axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
                legend.position  = "right",
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))+ labs(fill = "Complaint Year")
            
        } else {
            ggplot(reporting_test[reporting_test$complaint_year %in% year & reporting_test$record_type2==record_type2,], aes(x=qtr_case_file_date_new,fill=complaint_year)) + geom_bar(color="white")+ylab("Cases Filed")+xlab("By Quarters") +theme(#panel.grid.major = element_blank(),
                axis.text.x=element_text(angle = 40,hjust=0.5,vjust=0.5,size = rel(1)),
                axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
                axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
                legend.position  = "right",
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))+ labs(fill = "Complaint Year")
            
        }
        
    }
    else if (time_unit=="By months"){
        if (record_type2=="Full"){
            ggplot(reporting_test[reporting_test$complaint_year %in% year,], aes(x=mth_case_file_date_new,fill=complaint_year)) + geom_bar(color="white")+ylab("Cases Filed")+xlab("By Months") +theme(#panel.grid.major = element_blank(),
                axis.text.x=element_text(angle = 40,hjust=0.5,vjust=0.5,size = rel(1)),
                axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
                axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
                legend.position  = "right",
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))+ labs(fill = "Complaint Year")
            
        } else {
            ggplot(reporting_test[reporting_test$complaint_year %in% year & reporting_test$record_type2==record_type2,], aes(x=mth_case_file_date_new,fill=complaint_year)) + geom_bar(color="white")+ylab("Cases Filed")+xlab("By Months") +theme(#panel.grid.major = element_blank(),
                axis.text.x=element_text(angle = 40,hjust=0.5,vjust=0.5,size = rel(1)),
                axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
                axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
                legend.position  = "right",
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))+ labs(fill = "Complaint Year")
            
        }  
    }
    else if (time_unit=="By ISO weeks"){
        if (record_type2=="Full"){
            ggplot(reporting_test[reporting_test$complaint_year %in% year,], aes(x=wks_case_file_date_new,fill=complaint_year)) + geom_bar(color="white")+ylab("Cases Filed")+xlab("By ISO Weeks") +theme(#panel.grid.major = element_blank(),
                axis.text.x=element_text(angle = 90,hjust=0.5,vjust=0.5,size = rel(1)),
                axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
                axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
                legend.position  = "right",
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))+ labs(fill = "Complaint Year")
            
        } else {
            ggplot(reporting_test[reporting_test$complaint_year %in% year & reporting_test$record_type2==record_type2,], aes(x=wks_case_file_date_new,fill=complaint_year)) + geom_bar(color="white")+ylab("Cases Filed")+xlab("By ISO Weeks") +theme(#panel.grid.major = element_blank(),
                axis.text.x=element_text(angle = 90,hjust=0.5,vjust=0.5,size = rel(1)),
                axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
                axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
                legend.position  = "right",
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))+ labs(fill = "Complaint Year")
            
        }
    }
    else if (time_unit=="By years"){
        if (record_type2=="Full"){
            ggplot(reporting_test[reporting_test$complaint_year %in% year,], aes(x=complaint_year,fill=complaint_year)) + geom_bar(color="white")+ylab("Cases Filed")+xlab("By Years") +theme(#panel.grid.major = element_blank(),
                axis.text.x=element_text(angle = 90,hjust=0.5,vjust=0.5,size = rel(1)),
                axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
                axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
                legend.position  = "right",
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))+ labs(fill = "Complaint Year")
            
        } else {
            ggplot(reporting_test[reporting_test$complaint_year %in% year & reporting_test$record_type2==record_type2,], aes(x=complaint_year,fill=complaint_year)) + geom_bar(color="white")+ylab("Cases Filed")+xlab("By Years") +theme(#panel.grid.major = element_blank(),
                axis.text.x=element_text(angle = 90,hjust=0.5,vjust=0.5,size = rel(1)),
                axis.title.x=element_text(hjust=0.5,vjust=0.5,size = rel(0.9)),
                axis.text.y=element_text(hjust=0.5,vjust=0.5,size = rel(1)),
                legend.position  = "right",
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))+ labs(fill = "Complaint Year")
            
        }
    }
}

corrplot2 <- function(data, tl.pos="d", method = "pearson",sig.level = 0.05, order = "original",diag = FALSE, type = "upper",tl.srt = 90,number.font = 1,number.cex = 1,mar = c(0, 0, 0, 0)) {
    data_incomplete <- data
    data <- data[complete.cases(data), ]
    mat <- cor(data, method = method,use = "complete.obs")
    cor.mtest <- function(mat, method) {
        mat <- as.matrix(mat)
        n <- ncol(mat)
        p.mat <- matrix(NA, n, n)
        diag(p.mat) <- 0
        for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
                tmp <- cor.test(mat[, i], mat[, j], method = method)
                p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
            }
        }
        colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
        p.mat
    }
    p.mat <- cor.mtest(data, method = method)
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot(mat,
             method = "color", col = col(200), number.font = number.font,
             mar = mar, number.cex = number.cex,
             type = type, order = order,
             addCoef.col = "black", # add correlation coefficient
             tl.col = "black", tl.srt = tl.srt, # rotation of text labels combine with significance level
             p.mat = p.mat, sig.level = sig.level, insig = "blank",# hide correlation coefficients on the diagonal
             diag = diag
    )
}

ba_all <-colnames(reporting_test)[str_detect(colnames(reporting_test),"ba_")]
har_all <-colnames(reporting_test)[str_detect(colnames(reporting_test),"har_")]
y_axis<-c("Percentage","Count")
c_type<-c("Full","Complaint Only","Right to Sue Only")
unit_choice<-c("By ISO weeks","By months", "By quarters", "By years")

#mainPanel(plotOutput("basisPlot")),
?get

ui <- fluidPage(
    navbarPage("Employment Discrimination Reporting",
               tabPanel("Times Series",
                        tabsetPanel(
                            tabPanel("Overall Trends", br(),
                                 selectInput("time_unit", "Select a time unit", choices = unit_choice),
                                 selectInput("year_o", "Select a year range", choices = year_choice),
                                 radioButtons(
                                     "record_type2", "Select a case type: ", 
                                     choices = c_type, 
                                     inline = TRUE),
                                 plotOutput(outputId = "OPlot") #, width = "1000px", height = "500px"
                                     ),
                            tabPanel("Bases",br(),
                                 selectInput("basis", "Select a basis", choices = ba_all),
                                 selectInput("year", "Select a year range", choices = year_choice),
                                 radioButtons(
                                     "complaint_type", "Select a case type: ", 
                                     choices = c_type, 
                                     inline = TRUE),
                                 radioButtons(
                                     "y_axis", "y axis: ", 
                                     choices = y_axis, 
                                     inline = TRUE),
                                 plotOutput(outputId = "basisPlot")
                                 ),
                            tabPanel("Harms", br(),
                                 selectInput("harm", "Select a harm", choices = har_all, width = "50%"),
                                 selectInput("year2", "Select a year range", choices = year_choice),
                                 radioButtons(
                                     "complaint_type2", "Select a case type: ", 
                                     choices = c_type, 
                                     inline = TRUE),
                                 radioButtons(
                                     "y_axis2", "y axis: ", 
                                     choices = y_axis,
                                     inline = TRUE),
                                 plotOutput(outputId = "harmPlot")
                                 ),
                            ),
                        ),
               tabPanel("Correlations",
                        tabsetPanel(
                            tabPanel("Bases", br(),
                                     selectInput("ba_cor","Select bases to create correlation matrices",
                                    ba_all, multiple = TRUE, width = "50%"), # selectInput("year", "Select which years to visualize", choices = year_choice)
                                    plotOutput(outputId = "bacorPlot")),
                            tabPanel("Harms", br(),
                                     selectInput("har_cor","Select harms to create correlation matrices",
                                                 har_all, multiple = TRUE, width = "50%"), # selectInput("year", "Select which years to visualize", choices = year_choice)
                                     plotOutput(outputId = "harcorPlot")),
                        
                        )
                        ),
               tabPanel("About",
                        "TABLE 1: COMPLAINTS FILED BY LAW IN 2017, 4346",
                        textOutput(outputId = "tab1_text"),
                        dataTableOutput(outputId = "ic_table")
                        ),
               #mainPanel(plotOutput("distPlot")),
               ))
nrow(reporting_test[reporting_test$complaint_year==2019&reporting_test$record_type=="Employment",])
nrow(reporting_test[reporting_test$complaint_year==2017&reporting_test$record_type=="Employment",])
nrow(reporting_test[reporting_test$complaint_year==2018&reporting_test$record_type=="Employment",])
nrow(reporting_test[reporting_test$complaint_year==2016&reporting_test$record_type=="Employment",])
nrow(reporting_test[reporting_test$complaint_year==2016,])

nrow(reporting_test[reporting_test$complaint_year==2015&reporting_test$record_type=="Employment",])
nrow(reporting_test[reporting_test$complaint_year==2015,])

#new_empl<-dat_empl[,1:20]
#dat_empl<-subset(reporting_test[reporting_test$record_type=="Employment",], select=colnames(reporting_test)[str_detect(colnames(reporting_test),"ba_")])

# overall_time_series(2015,"Complaint Only","By months")
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$bacorPlot <- renderPlot({
        corrplot2(data=reporting_test[,input$ba_cor], order = "original", diag = FALSE, type = "upper")
    })
    output$harcorPlot <- renderPlot({
        corrplot2(data=reporting_test[,input$har_cor], order = "original", diag = FALSE, type = "upper")
    })
    output$tab1_text <- renderText({"Employment complaints filed by law in 2017 is 4,346; 2018 4,216 (DFEH, 2017, p.9)
        15,832 FOR 2016, PAGE 8 (Of the total complaints received by the Department, 17,041 complaints were formally filed by DFEH in 2016. 
        This number includes 12,242 employment complaints filed along with a request for an immediate Right to Sue letter
        and 4,799 complaints filed as the result of an intake interview conducted by a DFEH investigator.); 2015: PAGE 7 COMPLAINTS RECEIVED 
        Total Employment Complaints Received by Basis in 2015 = 20,505; 2014: In 2014, a total of 17,632 employment and 1,524 housing complaints were filed on the bases shown on the following page."})
    output$ic_table <- renderDataTable({reporting_test[reporting_test$complaint_year==2017,]})
    output$OPlot <- renderPlot({
        if (input$year_o=="2014-2019"){
            overall_time_series(year_full, input$record_type2, input$time_unit)
        }
        else if (input$year_o=="2015-2018"){
            overall_time_series(year_4, input$record_type2, input$time_unit)
        }
            })
    output$basisPlot <- renderPlot({
        if (input$complaint_type=="Full"){
            if (input$year=="2014-2019"){
                time_series(reporting_test, input$basis,year_full,input$y_axis,"Basis")
            }
            else {
                time_series(reporting_test, input$basis,year_4,input$y_axis,"Basis")
            }
        }
        else if (input$complaint_type=="Complaint Only"){
            if (input$year=="2014-2019"){
                time_series(reporting_test[reporting_test$record_type=="Employment",], input$basis,year_full,input$y_axis,"Basis")
            }
            else {
                time_series(reporting_test[reporting_test$record_type=="Employment",], input$basis,year_4,input$y_axis,"Basis")
            }
        }
        else if (input$complaint_type=="Right to Sue Only"){
            if (input$year=="2014-2019"){
                time_series(reporting_test[reporting_test$record_type=="Right to Sue",], input$basis,year_full,input$y_axis,"Basis")
            }
            else {
                time_series(reporting_test[reporting_test$record_type=="Right to Sue",], input$basis,year_4,input$y_axis,"Basis")
            }
        }
    })
    output$harmPlot <- renderPlot({
        if (input$complaint_type2=="Full"){
            if (input$year2=="2014-2019"){
                time_series(reporting_test, input$harm,year_full,input$y_axis2,"Harm")
            }
            else {
                time_series(reporting_test, input$harm,year_4,input$y_axis2, "Harm")
            }
        }
        else if (input$complaint_type2=="Complaint Only"){
            if (input$year2=="2014-2019"){
                time_series(reporting_test[reporting_test$record_type=="Employment",],input$harm,year_full,input$y_axis2,"Harm")
            }
            else {
                time_series(reporting_test[reporting_test$record_type=="Employment",],input$harm,year_4,input$y_axis2, "Harm")
            } 
        }
        else if (input$complaint_type2=="Right to Sue Only"){
            if (input$year2=="2014-2019"){
                time_series(reporting_test[reporting_test$record_type=="Right to Sue",],input$harm,year_full,input$y_axis2,"Harm")
            }
            else {
                time_series(reporting_test[reporting_test$record_type=="Right to Sue",],input$harm,year_4,input$y_axis2, "Harm")
            } 
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

