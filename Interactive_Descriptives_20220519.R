# Yao Xu
# Discrimination Reporting
# Interactive Descriptives
# 5/20/22

# library(shiny)
# RUN FROM REPO
shiny::runGitHub(repo = 'Discrimination_Reporting',username ='yx1441')
# options(shiny.error = function() {
#   stop("An error has occurred")
# })

requiredpkgs <- c("shiny", "tidyverse","devtools","readtext","janitor","cowplot",
                      "ggplot2","reshape2","data.table","openxlsx","rlang","ggpubr",
                      "leaflet","dplyr","corrplot")
newpkgs <- requiredpkgs[!(requiredpkgs %in% installed.packages()[,"Package"])]
if(length(newpkgs)) install.packages(newpkgs)

# LOAD PACKAGES ----------------------
library(shiny);library(tidyverse);library(devtools);library(readtext);library(janitor);library(cowplot)
library(ggplot2);library(reshape2);library(data.table);library(openxlsx);library(rlang);library(ggpubr)
library(leaflet);library(dplyr);library(corrplot)
make_pct<-function(x){x<-x*100}
# PREPARATION----------------------
githubURL<-"https://github.com/yx1441/Discrimination_Reporting/blob/32862e023bc5a74444680df80720b9ab8c82d659/reporting_test_20220512.RData?raw=true"
load(url(githubURL))
reporting_test<- reporting_test %>% dplyr::mutate(record_type2=case_when(reporting_test$record_type=="Employment" ~ "Complaint Only",
                                                                         reporting_test$record_type=="Right to Sue" ~ "Right to Sue Only"))

reporting_test<-reporting_test %>% dplyr::rename(har_denied_envir_free_of_discrim=har_denied_a_work_environment_free_of_discrimination_or_retaliation,
                                                 har_asked_impermissible_questions=har_asked_impermissible_nonjobrelated_questions,
                                                 har_denied_accommodation_disabilit=har_denied_reasonable_accommodation_for_a_disability,
                                                 har_denied_employment_benefit=har_denied_any_employment_benefit_or_privilege,
                                                 har_subjected_to_violence=har_subjected_to_a_threat_of_violence_or_violence_to_self_or_property,
                                                 har_denied_good_faith_process=har_denied_a_good_faith_interactive_process,
                                                 har_unequal_considerations_in_empl_decisions=har_failed_to_give_equal_considerations_in_making_employment_decisions)

ba_all <-colnames(reporting_test)[str_detect(colnames(reporting_test),"ba_")]
ba_all_noUnknown<-ba_all[!str_detect(ba_all,"unknown")]
har_all <-colnames(reporting_test)[str_detect(colnames(reporting_test),"har_")]
y_axis<-c("Percentage","Count")
c_type<-c("Full","Complaint Only","Right to Sue Only")
unit_choice<-c("By ISO weeks","By months", "By quarters", "By years")
year_full<-c(2014:2019)
year_4<-c(2015:2018)
year_choice<-c("2015-2018","2014-2019")
dat_empl<-subset(reporting_test[reporting_test$record_type=="Employment",])
dat_rts<-subset(reporting_test[reporting_test$record_type=="Right to Sue",])
reporting_test_1518<-reporting_test[reporting_test$complaint_year %in% c(2015:2018),]
dat_empl_1518<-subset(dat_empl[dat_empl$record_type=="Employment",])
dat_rts_1518<-subset(dat_rts[dat_rts$record_type=="Right to Sue",])


# 20 counts
get_cooccurences<-function(reporting_test,ba_or_har){
  c <- reporting_test %>% rowwise() %>% mutate(sum = sum(across(starts_with(ba_or_har)), na.rm = T))
  num_co<-data.frame(count=seq(1:21)-1)
  for (i in 1:length(colnames(c)[str_detect(colnames(c),ba_or_har)])){
    data<-NULL
    data<-data.frame(table(c$sum[c[[colnames(c)[str_detect(colnames(c),ba_or_har)][i]]]==1]-1))
    if (nrow(data.frame(table(c$sum[c[[colnames(c)[str_detect(colnames(c),ba_or_har)][i]]]==1])))!=0){
      colnames(data)<-c("count",colnames(c)[str_detect(colnames(c),ba_or_har)][i])
      data[[paste0(colnames(c)[str_detect(colnames(c),ba_or_har)][i],"_pct")]]<-data[[colnames(c)[str_detect(colnames(c),ba_or_har)][i]]]/sum(c[[colnames(c)[str_detect(colnames(c),ba_or_har)][i]]])
      num_co<-merge(num_co,data,by=c("count"),all.x=T) 
    }
  }

  num_co[,colnames(num_co)[!str_detect(colnames(num_co), "pct")]]<-sapply(num_co[,colnames(num_co)[!str_detect(colnames(num_co), "pct")]],as.integer)
  num_co[,colnames(num_co)[str_detect(colnames(num_co), "pct")]]<-sapply(num_co[,colnames(num_co)[str_detect(colnames(num_co), "pct")]],make_pct)
  num_co<-num_co %>% mutate(across(where(is.numeric), ~ round(., 3)))
  return(num_co)
}

full_cooc_ba<-get_cooccurences(reporting_test,"ba_")
empl_cooc_ba<-get_cooccurences(reporting_test[reporting_test$record_type=="Employment",],"ba_")
rts_cooc_ba<-get_cooccurences(reporting_test[reporting_test$record_type=="Right to Sue",],"ba_")

full_cooc_har<-get_cooccurences(reporting_test,"har_")
empl_cooc_har<-get_cooccurences(reporting_test[reporting_test$record_type=="Employment",],"har_")
rts_cooc_har<-get_cooccurences(reporting_test[reporting_test$record_type=="Right to Sue",],"har_")


# Check data inconsistencies w.r.t. annual reports from DFEH
nrow(reporting_test[reporting_test$complaint_year==2019&reporting_test$record_type=="Employment",])
nrow(reporting_test[reporting_test$complaint_year==2017&reporting_test$record_type=="Employment",])
nrow(reporting_test[reporting_test$complaint_year==2018&reporting_test$record_type=="Employment",])
nrow(reporting_test[reporting_test$complaint_year==2016&reporting_test$record_type=="Employment",])
nrow(reporting_test[reporting_test$complaint_year==2016,])

nrow(reporting_test[reporting_test$complaint_year==2015&reporting_test$record_type=="Employment",])
nrow(reporting_test[reporting_test$complaint_year==2015,])


# FUNCTIONS --------------
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
corrplot2 <- function(data,method = "pearson",sig.level = 0.05, order = "original",diag = FALSE, type = "upper",tl.srt = 90,number.font = 1,number.cex = 1,mar = c(0, 0, 0, 0)) {
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

# UI  --------------
ui <- fluidPage(
    navbarPage("Employment Discrimination Reporting",
               tabPanel("About",
                        sidebarLayout(
                            sidebarPanel(
                                h4("Project Progress"),
                                p("Preliminary Data Analysis"),
                                code('install.packages("shiny")'),
                                p("Running this application from local machine"),
                                code('shinyApp(ui = ui, server = server)'),
                                p("Running this application from Github Repository"),
                                code("shiny::runGitHub(repo = 'Discrimination_Reporting',username ='yx1441')"),
                                br(),
                                # img(src = "flowchart.png", height = 200, width = 200),
                                br(),
                                "Note: running from Github may take 7-8 minutes "
                                # span("RStudio", style = "color:blue")
                            ),
                            mainPanel(
                                h3("Introducing the DFEH dataset"),
                                p("The Department of Fair Employment and Housing (DFEH) is responsible for 
                                  enforcing state laws that make it illegal to discriminate against a job applicant 
                                  or employee because of a protected characteristic; visit ", 
                                  a("DFEH annual reports and statistics", 
                                    href = "https://www.dfeh.ca.gov/legalrecords/?content=reports#reportsBody")),
                                br(),
                                p("For current RA Work progress, visit this ",
                                  a("Google Drive Folder", 
                                    href = "https://drive.google.com/drive/folders/1LjMFuztpKktHIwU-FXDCc7xjl7aMEnkA?usp=sharing")),
                                br(),
                                p("For data and code of this application, visit our ",
                                  a("Discrimination Reporting Github Repository", 
                                    href = "https://github.com/yx1441/Discrimination_Reporting")),
                                br(),
                                h3("Features of this application"),
                                p("- Time series figures by bases/harms, record type, year range, and by count/percentage."),
                                p("- Correlation matrices and co-occurence counts tables by bases/harms, record type, and year range."),
                                p("- Links to resources"),
                            )
                        ),
                        # img(src = "flowchart.png", width = "100%")
                        # dataTableOutput(outputId = "sum_table")
               ),
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
                            tabPanel("Bases Only", br(),
                                     selectInput("ba_cor","Select bases to create correlation matrices", ba_all, multiple = TRUE, width = "75%"),
                                     selectInput("year_cor", "Select which years to visualize", choices = year_choice),
                                     radioButtons(
                                         "complaint_type_cor", "Select a case type: ", 
                                         choices = c_type, 
                                         inline = TRUE),
                                     
                                     plotOutput(outputId = "bacorPlot", width = "100%")),
                            
                            tabPanel("Harms Only", br(),
                                     selectInput("har_cor","Select harms to create correlation matrices",
                                                 har_all, multiple = TRUE, width = "75%"),
                                     selectInput("year_cor2", "Select which years to visualize", choices = year_choice),
                                     radioButtons(
                                         "complaint_type_cor2", "Select a case type: ", 
                                         choices = c_type, 
                                         inline = TRUE),
                                     plotOutput(outputId = "harcorPlot", width = "100%")),
                            tabPanel("Bases and Harms", br(),
                                     selectInput("ba_cor2","Select bases to create correlation matrices", ba_all, multiple = TRUE, width = "75%"),
                                     selectInput("har_cor2","Select harms to create correlation matrices", har_all, multiple = TRUE, width = "75%"),
                                     # selectInput("year_cor", "Select which years to visualize", choices = year_choice),
                                     # radioButtons(
                                     #     "complaint_type_cor", "Select a case type: ", 
                                     #     choices = c_type, 
                                     #    inline = TRUE),
                                     plotOutput(outputId = "cocorPlot", width = "100%")
                            ),
                            
                        )
               ),
               tabPanel("Co-occurences",
                        tabsetPanel(
                          tabPanel("Bases", br(),
                                   selectInput("ba_cooc","Select bases to see co-occurence counts and percentages", ba_all, multiple = TRUE, width = "75%"),
                                   #selectInput("year_co", "Select which years to visualize", choices = year_choice),
                                   radioButtons(
                                     "complaint_type_cooc", "Select a case type: ", 
                                     choices = c_type, 
                                     inline = TRUE),
                                   dataTableOutput(outputId = "ic_table_ba")),
                          
                          tabPanel("Harms", br(), 
                                   # HIDING ERROR MESSAGES AT THE BEGINNING
                                   tags$style(type="text/css",
                                              ".shiny-output-error { visibility: hidden; }",
                                              ".shiny-output-error:before { visibility: hidden; }"),
                                   
                                   selectInput("har_cooc","Select harms to see co-occurence counts and percentages",har_all, multiple = TRUE, width = "75%"),
                                   #selectInput("year_cor2", "Select which years to visualize", choices = year_choice),
                                   radioButtons(
                                     "complaint_type_cooc2", "Select a case type: ", 
                                     choices = c_type, 
                                     inline = TRUE),
                                   dataTableOutput(outputId = "ic_table_har")),
                        )
               ),
               tabPanel("Fun",
                        "Throwing in one last figure here:",
                        br(),
                        br(),
                        sliderInput("color",
                                    label = "If you are feeling a little gray, make your heart a different color?",
                                    min = 0,
                                    max = 1,
                                    value = 0,
                                    step = 0.1,
                                    width = "40%"),
                        sliderInput("color2",
                                    label = "",
                                    min = 0,
                                    max = 1,
                                    value = 0,
                                    step = 0.1,
                                    width = "40%"),
                        plotOutput(outputId = "heart", width = "400px", height = "400px")
                        # dataTableOutput(outputId = "ic_table")
               ),
               #mainPanel(plotOutput("distPlot")),
    ))


dat <- data.frame(t = seq(0, 2*pi, by = 0.01))
x <-  function(t) 16 * sin(t)^3
y <- function(t) 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)
dat$y <- y(dat$t)
dat$x <- x(dat$t)

# plotOutput(outputId = "bacorPlot", width = "100%")
# SERVER  --------------
server <- function(input, output, session) {
    output$heart<- renderPlot({ 
      ggplot(dat, aes(x,y)) +geom_polygon(fill = rgb(input$color,input$color2,0.4,0.3)) +theme_classic()+xlab("")+ylab("")
    })
    #output$sum_table <- renderDataTable({reporting_test})
    output$cocorPlot<- renderPlot({ 
        corrplot2(data=reporting_test[,c(input$ba_cor2,input$har_cor2)], order = "original", diag = FALSE, type = "upper")
    })
    output$bacorPlot <- renderPlot({
        if (input$complaint_type_cor=="Full"){
            if (input$year_cor=="2014-2019"){
                corrplot2(data=reporting_test[,input$ba_cor], order = "original", diag = FALSE, type = "upper") 
            }
            else{
                corrplot2(data=reporting_test_1518[,input$ba_cor], order = "original", diag = FALSE, type = "upper") 
            }
        }
        else if(input$complaint_type_cor=="Complaint Only"){
            if (input$year_cor=="2014-2019"){
                corrplot2(data=dat_empl[,input$ba_cor], order = "original", diag = FALSE, type = "upper")
            }
            else{
                corrplot2(data=dat_empl_1518[,input$ba_cor], order = "original", diag = FALSE, type = "upper") 
            }
        }
        else if(input$complaint_type_cor=="Right to Sue Only"){
            if (input$year_cor=="2014-2019"){
                corrplot2(data=dat_rts[,input$ba_cor], order = "original", diag = FALSE, type = "upper")
            }
            else{
                corrplot2(data=dat_rts_1518[,input$ba_cor], order = "original", diag = FALSE, type = "upper")
            }
        }
    })
    output$harcorPlot <- renderPlot({
        if (input$complaint_type_cor2=="Full"){
            if (input$year_cor2=="2014-2019"){
                corrplot2(data=reporting_test[,input$har_cor],order = "original", diag = FALSE, type = "upper") 
            }else{
                corrplot2(data=reporting_test_1518[,input$har_cor], order = "original", diag = FALSE, type = "upper") 
            }
        }
        else if(input$complaint_type_cor2=="Complaint Only"){
            if (input$year_cor2=="2014-2019"){
                corrplot2(data=dat_empl[,input$har_cor], order = "original", diag = FALSE, type = "upper")
            }else{
                corrplot2(data=dat_empl_1518[,input$har_cor], order = "original", diag = FALSE, type = "upper") 
            }
        }
        else if(input$complaint_type_cor2=="Right to Sue Only"){
            if (input$year_cor2=="2014-2019"){
                corrplot2(data=dat_rts[,input$har_cor], order = "original", diag = FALSE, type = "upper")
            }else{
                corrplot2(data=dat_rts_1518[,input$har_cor], order = "original", diag = FALSE, type = "upper")
            }
        }
    })
    output$tab1_text <- renderText({"Employment complaints filed by law in 2017 is 4,346; 2018 4,216 (DFEH, 2017, p.9)
        15,832 FOR 2016, PAGE 8 (Of the total complaints received by the Department, 17,041 complaints were formally filed by DFEH in 2016. 
        This number includes 12,242 employment complaints filed along with a request for an immediate Right to Sue letter
        and 4,799 complaints filed as the result of an intake interview conducted by a DFEH investigator.); 2015: PAGE 7 COMPLAINTS RECEIVED 
        Total Employment Complaints Received by Basis in 2015 = 20,505; 2014: In 2014, a total of 17,632 employment and 1,524 housing complaints were filed on the bases shown on the following page."})
    
    # Outputting the harms co oc table
      output$ic_table_har <- renderDataTable({
      if (input$complaint_type_cooc2=="Full"){
        check2<-NULL
        for (i in 1:length(input$har_cooc)){
          check2[[i]]<-full_cooc_har[,colnames(full_cooc_har)[str_detect(colnames(full_cooc_har),input$har_cooc[i])]]
        }
        cbind(count=full_cooc_har[,1],do.call(cbind,check2))
      }
      else if (input$complaint_type_cooc2=="Complaint Only"){
        check2<-NULL
        for (i in 1:length(input$har_cooc)){
          check2[[i]]<-empl_cooc_har[,colnames(empl_cooc_har)[str_detect(colnames(empl_cooc_har),input$har_cooc[i])]]
        }
        cbind(count=empl_cooc_har[,1],do.call(cbind,check2))
      }
      else if (input$complaint_type_cooc2=="Right to Sue Only") {
        check2<-NULL
        for (i in 1:length(input$har_cooc)){
          check2[[i]]<-rts_cooc_har[,colnames(rts_cooc_har)[str_detect(colnames(rts_cooc_har),input$har_cooc[i])]]
        }
        cbind(count=rts_cooc_har[,1],do.call(cbind,check2))
      }
      # colnames(num_co)[str_detect(colnames(num_co),input$ba_cor3)
    })
    # Reactive UI for when complaint is selected!!
    observeEvent(input$complaint_type_cooc, {
      if (input$complaint_type_cooc=="Complaint Only"){
        updateSelectInput(session, "ba_cooc",label ="Select bases to see co-occurence counts and percentages", ba_all_noUnknown)
        }}) 
    observeEvent(input$complaint_type_cooc, {
      if (input$complaint_type_cooc!="Complaint Only"){
        updateSelectInput(session, "ba_cooc",label ="Select bases to see co-occurence counts and percentages", ba_all)
      }})  
    
    # Outputting the basis co oc table
    output$ic_table_ba <- renderDataTable({
      if (input$complaint_type_cooc=="Full"){
        check2<-NULL
        for (i in 1:length(input$ba_cooc)){
          check2[[i]]<-full_cooc_ba[,colnames(full_cooc_ba)[str_detect(colnames(full_cooc_ba),input$ba_cooc[i])]]
        }
        cbind(count=full_cooc_ba[,1],do.call(cbind,check2))
      }
      else if (input$complaint_type_cooc=="Complaint Only"){
        check2<-NULL
        for (i in 1:length(input$ba_cooc)){
          check2[[i]]<-empl_cooc_ba[,colnames(empl_cooc_ba)[str_detect(colnames(empl_cooc_ba),input$ba_cooc[i])]]
        }
        cbind(count=empl_cooc_ba[,1],do.call(cbind,check2))
      }
      else if (input$complaint_type_cooc=="Right to Sue Only") {
        check2<-NULL
        for (i in 1:length(input$ba_cooc)){
          check2[[i]]<-rts_cooc_ba[,colnames(rts_cooc_ba)[str_detect(colnames(rts_cooc_ba),input$ba_cooc[i])]]
        }
        cbind(count=rts_cooc_ba[,1],do.call(cbind,check2))
      }
        # colnames(num_co)[str_detect(colnames(num_co),input$ba_cor3)
    })
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

# RUN FROM LOCAL----------------
shinyApp(ui = ui, server = server)
# runApp("app.R")
