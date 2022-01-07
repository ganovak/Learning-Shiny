# Binomial Distribution Demonstration App
#
# Function:
#   Visually demonstrate effect of parameters in binomial distribution
# Input:
#   n: integer in [0,100] NOTE: upper bound is arbitrary
#   p: real in [0,1]
# Output:
#   Barplot of PMF of binomial distribution with user specified parameters
#
# rsconnect::deployApp("BinomDemo")
# G Novak 2022-01-03

library(shiny)

ui <- fluidPage(

    # Title
    titlePanel("Binomial Distribution Demonstration"),
    
    sidebarLayout(
        # Parameter selection panel
        sidebarPanel(
            # n
            numericInput("n", "n", value = 10, min = 0, max = 100, step = 1),
            # p
            sliderInput("p", "p", min = 0.0, max = 1.0, value = 0.5),
            # simplify
            checkboxInput("simple", "Use simple labels", value = FALSE)
        ),

        # Output panel
        mainPanel(
           plotOutput("binomPlot")
        )
    )
)


server <- function(input, output) {

    output$binomPlot <- renderPlot({
        
        # calculate pmf
        pmf <- vector(mode = "numeric", length = input$n + 1L)
        for (k in 0:input$n) {
            pmf[k + 1] <- dbinom(k, input$n, input$p)
        }
        
        # draw barplot
        if (input$simple) {
            barplot(pmf,
                    names.arg = 0:input$n,
                    main = paste0("Binom(", input$n, ", ", input$p, ")"),
                    xlab = "k",
                    ylab = "P(X = x)")
        } else {
            barplot(pmf,
                    names.arg = 0:input$n,
                    main = paste0("Binom(n = ", input$n, ", p = ", input$p, ")"),
                    xlab = "Number of successes",
                    ylab = "Probability of observation")
        }
    })
}

# run app 
shinyApp(ui = ui, server = server)
