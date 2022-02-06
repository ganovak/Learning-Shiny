# Central Limit Theorem for Means Demonstration App
#
# Function:
#   Visually demonstrate central limit theorem
#   using user defined population
#   and user controlled gradual sampling
# Input:
#   Population distribution: one of uniform, normal, 
#   or skewed (chisq) with randomized parameters
#   n: Integer > 0
# Output:
#   Plot of population distribution and numerical summaries
#   Gradual CLT demonstration with reactive plot and numerical
#   summaries
#
# rsconnect::deployApp("CLTDemo")
# G Novak 2022-02-06

library(shiny)
library(shinyjs)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    
    titlePanel("Central Limit Theorem Demonstration for Means"),
    
    h2("Given a population with a finite mean, \u03BC, and a finite, non-zero variance, \u03C3\u00B2, the sampling distribution of the mean approaches a normal distribution with a mean of µ and a variance of \u03C3\u00B2/n as n, the sample size, increases."),
    
    ##### Population #####
    fluidRow(
        # Control Panel
        column(width = 2,
               br(), br(), br(), br(), br(), br(), br(),
               selectInput("popSelect",
                           "Population Distribution",
                           c("Uniform", "Normal", "Skewed")),
               actionButton("go", "Begin Demonstration")
               ),
        # Plot
        column(width = 6, plotOutput("popPlot")),
        # Parameter report
        column(width = 4,                
               br(), br(), br(), br(), br(), br(), br(), br(),
               verbatimTextOutput("popParams"))
    ),
    
    ##### Sample #####
    fluidRow(
        # Control Panel
        column(width = 2,
               br(), br(), br(), br(), br(),
               numericInput("n", "n",
                            value = 30,
                            min = 1),
               disabled(actionButton("setN", "Set N")),
               br(),
               h4("Take Samples"),
               disabled(actionButton("ts1", "1")),
               disabled(actionButton("ts10", "10")),
               disabled(actionButton("ts100", "100")),
               disabled(actionButton("ts1000", "1000")),
               br(),
               br(),
               actionButton("reset", "Reset Demonstration")),
        # Plot
        column(width = 6, plotOutput("sampPlot")),
        # Stat report
        column(width = 4,
               br(), br(), br(), br(), br(), br(), br(), br(),
               verbatimTextOutput("sampStats"))
    )
)


#####
server <- function(input, output) {
    
    samps <- reactiveVal()
    samps(vector("numeric"))
    
    ##### Begin demonstration ####
    observeEvent(input$go, {
        disable("popSelect")
        disable("go")
        enable("setN")
        output$popPlot <- renderPlot({
            x <- seq(from = 0, to = 100, by = 0.1)
            if (specs()$dist == "Normal") {
                plot(x, dnorm(x, specs()$mean, sqrt(specs()$var)), type = "l",
                     main = "Normally Distributed Population",
                     xlab = "Numerical Charactaristic", ylab = "Density")
            } else if (specs()$dist == "Skewed") {
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
        })
    
    specs <- eventReactive(input$go, {
            if (input$popSelect == "Normal") {
                mu <- sample(25:75, 1)
                s <- sample(1:100, 1)
                list(dist = "Normal",
                     mean = mu,
                     var = s^2)
            } else if (input$popSelect == "Skewed") {
                df <- sample(1:10, 1)
                list(dist = "Skewed",
                     mean = df,
                     var = df * 2)
            } else { # uniform case
                a <- 25
                b <- 75
                list(dist = "Uniform",
                     a = a,
                     b = b,
                     mean = 1/2 * (a + b),
                     var = 1/12 * (b - a)^2)
            }
        })
    
    ##### Select sample size #####
    n <- eventReactive(input$setN, input$n)
    
    observeEvent(input$setN, {
        disable("n")
        disable("setN")
        enable("ts1")
        enable("ts10")
        enable("ts100")
        enable("ts1000")
    })
    
    ##### Take samples #####
    
    observeEvent(input$ts1, {
        if (input$popSelect == "Normal") {
            samps(append(samps(),
                          mean(rnorm(input$n, specs()$mean, sqrt(specs()$var)))))
        } else if (input$popSelect == "Skewed") {
            samps(append(samps(),
                         mean(rchisq(input$n, specs()$mean))))
        } else { # uniform case
            samps(append(samps(),
                         mean(runif(input$n, specs()$a, specs()$b))))
        }
        
        maxDens <- dnorm(specs()$mean, specs()$mean, sqrt(specs()$var/input$n))
        
        output$sampPlot <- renderPlot({
            x <- seq(from = 0, to = 100, by = 0.1)
            hist(samps(), freq = FALSE,
                 main = "Sampling Distribution of x\u0305",
                 xlab = "Sample Mean of Numeric Charactaristic",
                 ylim = c(0, maxDens + 0.025))
            lines(x, dnorm(x, specs()$mean, sqrt(specs()$var/input$n)))
        })
        output$sampStats <- renderPrint({
            cat(paste("N = ", input$n,
                      "\nSamples = ", length(samps()),
                      "\nMean =", mean(samps()),
                      "\nVariance =", var(samps()),
                      "\n\u03C3\u00B2/n = ", specs()$var/input$n))
        })
    })
    
    observeEvent(input$ts10, {
        for (s in 1:10) {
            if (input$popSelect == "Normal") {
                samps(append(samps(),
                             mean(rnorm(input$n, specs()$mean, sqrt(specs()$var)))))
            } else if (input$popSelect == "Skewed") {
                samps(append(samps(),
                             mean(rchisq(input$n, specs()$mean))))
            } else { # uniform case
                samps(append(samps(),
                             mean(runif(input$n, specs()$a, specs()$b))))
            }
        }
        
        maxDens <- dnorm(specs()$mean, specs()$mean, sqrt(specs()$var/input$n))
        
        output$sampPlot <- renderPlot({
            x <- seq(from = 0, to = 100, by = 0.1)
            hist(samps(), freq = FALSE,
                 main = "Sampling Distribution of x\u0305",
                 xlab = "Sample Mean of Numeric Charactaristic",
                 ylim = c(0, maxDens + 0.025))
            lines(x, dnorm(x, specs()$mean, sqrt(specs()$var/input$n)))
        })
        output$sampStats <- renderPrint({
            cat(paste("N = ", input$n,
                      "\nSamples = ", length(samps()),
                      "\nMean =", mean(samps()),
                      "\nVariance =", var(samps()),
                      "\n\u03C3\u00B2/n = ", specs()$var/input$n))
        })
    })
    
    observeEvent(input$ts100, {
        for (s in 1:100) {
            if (input$popSelect == "Normal") {
                samps(append(samps(),
                             mean(rnorm(input$n, specs()$mean, sqrt(specs()$var)))))
            } else if (input$popSelect == "Skewed") {
                samps(append(samps(),
                             mean(rchisq(input$n, specs()$mean))))
            } else { # uniform case
                samps(append(samps(),
                             mean(runif(input$n, specs()$a, specs()$b))))
            }
        }
        
        maxDens <- dnorm(specs()$mean, specs()$mean, sqrt(specs()$var/input$n))
        
        output$sampPlot <- renderPlot({
            x <- seq(from = 0, to = 100, by = 0.1)
            hist(samps(), freq = FALSE,
                 main = "Sampling Distribution of x\u0305",
                 xlab = "Sample Mean of Numeric Charactaristic",
                 ylim = c(0, maxDens + 0.025))
            lines(x, dnorm(x, specs()$mean, sqrt(specs()$var/input$n)))
            })
        output$sampStats <- renderPrint({
            cat(paste("N = ", input$n,
                      "\nSamples = ", length(samps()),
                      "\nMean =", mean(samps()),
                      "\nVariance =", var(samps()),
                      "\n\u03C3\u00B2/n = ", specs()$var/input$n))
        })
    })
    
    observeEvent(input$ts1000, {
        for (s in 1:1000) {
            if (input$popSelect == "Normal") {
                samps(append(samps(),
                             mean(rnorm(input$n, specs()$mean, sqrt(specs()$var)))))
            } else if (input$popSelect == "Skewed") {
                samps(append(samps(),
                             mean(rchisq(input$n, specs()$mean))))
            } else { # uniform case
                samps(append(samps(),
                             mean(runif(input$n, specs()$a, specs()$b))))
            }
        }
        
        maxDens <- dnorm(specs()$mean, specs()$mean, sqrt(specs()$var/input$n))
        
        output$sampPlot <- renderPlot({
            x <- seq(from = 0, to = 100, by = 0.025)
            hist(samps(), freq = FALSE,
                 main = "Sampling Distribution of x\u0305",
                 xlab = "Sample Mean of Numeric Charactaristic",
                 ylim = c(0, maxDens + 0.025))
            lines(x, dnorm(x, specs()$mean, sqrt(specs()$var/input$n)))
        })
        output$sampStats <- renderPrint({
            cat(paste("N = ", input$n,
                      "\nSamples = ", length(samps()),
                      "\nMean =", mean(samps()),
                      "\nVariance =", var(samps()),
                      "\n\u03C3\u00B2/n = ", specs()$var/input$n))
        })
    })
    
    ##### Reset demo ####
    observeEvent(input$reset, {
        output$popPlot <- NULL
        output$popParams <- NULL
        output$sampStats <- NULL
        output$sampPlot <- NULL
        disable("setN")
        disable("ts1")
        disable("ts10")
        disable("ts100")
        disable("ts1000")
        enable("popSelect")
        enable("go")
        enable("n")
    })
}

# Run the demo
shinyApp(ui = ui, server = server)
