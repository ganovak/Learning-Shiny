#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Normal Distribution Demonstration"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("mu", "Mean", 0),
            numericInput("sig", "Variance", 100, min = 0),
            actionButton("go", "Update Distribution")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    params <- eventReactive(input$go, {
                len <- 400
                top <- qnorm(0.025, input$mu, sqrt(input$sig))
                bottom <- qnorm(0.975, input$mu, sqrt(input$sig))
                support <- seq(from = round(top, 2), to = round(bottom, 2), length.out = len)
                dens <- numeric(len)
                    for (i in 1:length(support)) {
                        dens[i] <- dnorm(support[i], input$mu, sqrt(input$sig))
                    }
                list(mu = input$mu, sig = input$sig, dens = dens, support = support)
            })
        
            output$distPlot <- renderPlot({
                plot(params()$support, params()$dens, type = "l",
                     xlab = "x", ylab = "Density",
                     main = paste0("x ~ Norm(", params()$mu, ", ", params()$sig, ")"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
