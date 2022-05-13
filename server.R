# Yao Xu
# Discrimination Reporting
# Interactive Descriptives
# 5/12/22

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

