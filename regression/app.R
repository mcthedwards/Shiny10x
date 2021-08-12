library(rsconnect)
library(shiny)
library(ggplot2)

ui <- fluidPage(
    
    # Application title
    titlePanel("Simple Linear Regression"),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            numericInput("n",
                         "Number of data points:",
                         min = 3,
                         max = 100,
                         value = 10),
            sliderInput("beta0",
                         "Intercept:",
                         min = -10,
                         max = 10,
                         value = 0,
                        step = 0.1),
            sliderInput("beta1",
                         "Slope:",
                         min = -10,
                         max = 10,
                         value = 1,
                         step = 0.1),
            checkboxInput("show.rss",
                          "Show sum of squared residuals",
                          FALSE),
            checkboxInput("show.fit",
                          "Show least squares regression line",
                          FALSE)
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
        
        set.seed(1)
        
        x = seq(-10, 10, length = input$n)
        beta0.true = rnorm(1, 0, 3)
        beta1.true = rnorm(1, 1, 3)
        sd = 4  
        y.obs = beta0.true + beta1.true * x + rnorm(input$n, 0, sd)
        y.self = input$beta0 + input$beta1 * x
        
        lm.fit = lm(y.obs ~ x)
        coef.fit = coef(lm.fit)
        resid.self = y.obs - y.self
        rss = round(sum(resid.self ^ 2))
        
        data1 = data.frame(x = x,
                           y.obs = y.obs,
                           y.self = y.self,
                           resid.self = resid.self)
        
        gg = ggplot(data = data1, mapping = aes(x = x, y = y.obs)) +
            geom_point(col = 4, size = 4, alpha = 0.5) +
            geom_abline(slope = input$beta1, intercept = input$beta0,
                        size = 1.2) + 
            theme_minimal() + 
            xlab("x") +
            ylab("y") + 
            coord_fixed() 
        
        if (input$show.rss) {
            gg = gg + geom_rect(aes(xmin = pmin(x, x - resid.self), 
                                    ymin = pmin(y.obs, y.self),
                                    xmax = pmax(x, x - resid.self), 
                                    ymax = pmax(y.obs, y.self)), 
                                alpha = 0.5, col = 1, fill = 2) +
                annotate("label", x = 5, y = -5, 
                         label = paste0("RSS = ", rss))
        }
        
        if (input$show.fit) {
            b0 = round(coef.fit[1], 2)
            b1 = round(coef.fit[2], 2)
            gg = gg + geom_abline(slope = coef.fit[2],
                                  intercept = coef.fit[1], 
                                  col = 4,
                                  size = 1.2) +
                annotate("label", x = -5, y = 10,
                         label = glue::glue("y = {b0} + {b1} x"))
        }
        
        gg
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
