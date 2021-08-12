library(rsconnect)
library(shiny)
library(ggplot2)

ui <- fluidPage(
    
    # Application title
    titlePanel("Chi-square-test for Independence"),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            numericInput("n.row",
                         "Number of rows:",
                         min = 2,
                         value = 3),
            numericInput("n.col",
                         "Number of columns:",
                         min = 2,
                         value = 3),
            numericInput("Chisq.stat",
                         "Chi-square-test statistic:",
                         min = 0,
                         value = 10)
        ),
        # Show a plot 
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        df = (input$n.row - 1) * (input$n.col - 1)
        
        xmax = max(qchisq(1 - 1e-6, df), input$Chisq.stat)
        
        x1 = seq(0, xmax, by = 0.01)
        y1 = dchisq(x1, df)
        data1 = data.frame(x = x1, y = y1)
        
        x2 = seq(input$Chisq.stat, xmax, by = 0.01)
        y2 = dchisq(x2, df)
        data2 = data.frame(x = x2, y = y2)
        
        pvalue = round(1 - pchisq(input$Chisq.stat, df), 6)
        
        ggplot(data = data1, mapping = aes(x = x, y = y)) +
            geom_line() +
            theme_minimal() + 
            geom_vline(xintercept = input$Chisq.stat, linetype = "dashed") +
            geom_ribbon(data = data2, mapping = aes(ymin = 0, ymax = y),
                        colour = "navyblue", fill = "navyblue", alpha = 0.5) +
            # annotate("label", x = max(x1) / 2, y = max(y1), 
            #          label = paste0("P-value = ", pvalue)) +
            annotate("label", x = max(x1) / 2, y = max(y1), 
                     label = paste0("P-value = ", pvalue)) +
            xlab("") +
            ylab("")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
