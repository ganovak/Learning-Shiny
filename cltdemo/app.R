# TODO:
# Server side for sample
# Header
# Fix statement of theorem

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Central Limit Theorem for Means Demonstration"),
    br(),
    
    # statement of theorem
    h3("Given a population with a finite mean, µ, and a finite, non-zero variance, σ^2,
            the sampling distribution of the mean approaches a normal distribution with a
            mean of µ and a variance of σ^2/n as n, the sample size, increases."),
    br(),
    
    # population
    fluidRow(
        column(4,
               br(),
               br(),
               wellPanel(
                   selectInput("popDist", "Population Distribution",
                               c("Normal", "Skewed", "Uniform")),
                   actionButton("newPop", "New Population")
               )
        ),
        column(4, plotOutput("popPlot")),
        column(4, br(), br(), br(), verbatimTextOutput("popParams"))
    ),
    br(),
    
    # sample
    fluidRow(
        column(4, align = "center",
               br(),
               br(),
               wellPanel(
                   numericInput("n", "n:", min = 1, value = 30),
                   actionButton("take1", "Add 1 Sample"),
                   actionButton("take10", "Add 10 Samples"),
                   actionButton("take100", "Add 100 Samples"),
                   actionButton("take1000", "Add 1000 Samples")
               )
        ),
        column(4, plotOutput("sampPlot")),
        column(4, br(), br(), br(), verbatimTextOutput("sampStats"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    popdist <- eventReactive(input$newPop, input$popDist)
    
    specs <- eventReactive(input$newPop, {
        if (input$popDist == "Normal") {
            mu <- sample(25:75, 1)
            s <- sample(1:100, 1)
            list(mean = mu,
                 var = s^2,
                 str = paste0("Norm(mean = ", mu, ", var = ", s^2, ")"))
        } else if (input$popDist == "Skewed") {
            df <- sample(1:10, 1)
            list(mean = df,
                 var = df * 2,
                 str = paste0("ChiSq(df = ", df, ")"))
        } else { # uniform case
            a <- 25
            b <- 75
            list(a = a,
                 b = b,
                 mean = 1/2 * (a + b),
                 var = 1/12 * (b - a)^2,
                 str = paste0("Unif(a = ", a, ", b = ", b, ")"))
        }})
    
    output$popPlot <- renderPlot({
        x <- seq(from = 0, to = 100, by = 0.1)
        if (popdist() == "Normal") {
            plot(x, dnorm(x, specs()$mean, sqrt(specs()$var)), type = "l",
                 main = "Normally Distributed Population",
                 xlab = "Numerical Charactaristic", ylab = "Density")
        } else if (popdist() == "Skewed") {
            plot(x, dchisq(x, specs()$mean), type = "l",
                 main = "Skew Distributed Population",
                 xlab = "Numerical Charactaristic", ylab = "Density")
        } else { # uniform case
            plot(x, dunif(x, specs()$a, specs()$b), type = "l",
                 main = "Uniformly Distributed Population",
                 xlab = "Numerical Charactaristic", ylab = "Density")
        }
    })
    
    output$popParams <- renderPrint({
        cat(paste("Mean =", specs()$mean, "\nVariance =", specs()$var))
    })

    #reactivity situation
    
    #output$sampPlot
    
    #output$sampStats
}

# Run the application 
shinyApp(ui = ui, server = server)
