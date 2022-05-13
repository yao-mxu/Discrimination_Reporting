# Yao Xu
# Discrimination Reporting
# Interactive Descriptives
# 5/12/22
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

