library(rsconnect)
library(shiny)
library(ggplot2)

ui <- fluidPage(

    # Application title
    titlePanel("F-test for One-way ANOVA"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            numericInput("n.group",
                        "Number of groups:",
                        min = 3,
                        value = 3),
            numericInput("n.obs",
                        "Number of observations:",
                        min = 4,
                        value = 100),
            numericInput("F.stat",
                        "F-test statistic:",
                        min = 0,
                        value = 2)
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
        
        df1 = input$n.group - 1
        df2 = input$n.obs - input$n.group

        xmax = max(qf(1 - 1e-6, df1, df2), input$F.stat)
        
        x1 = seq(0, xmax, by = 0.01)
        y1 = df(x1, df1, df2)
        data1 = data.frame(x = x1, y = y1)
        
        x2 = seq(input$F.stat, xmax, by = 0.01)
        y2 = df(x2, df1, df2)
        data2 = data.frame(x = x2, y = y2)
        
        pvalue = round(1 - pf(input$F.stat, df1, df2), 6)

        ggplot(data = data1, mapping = aes(x = x, y = y)) +
            geom_line() +
            theme_minimal() + 
            geom_vline(xintercept = input$F.stat, linetype = "dashed") +
            geom_ribbon(data = data2, mapping = aes(ymin = 0, ymax = y),
                        colour = 2, fill = 2, alpha = 0.5) +
            annotate("label", x = max(x1) / 2, y = max(y1), 
                     label = paste0("P-value = ", pvalue)) +
            xlab("") +
            ylab("")

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
