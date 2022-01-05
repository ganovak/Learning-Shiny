# Basic Discrete Distributions Demo
#
# Objective:
#   Visually demonstrate effect of parameters on support and PMF of commonly used
#   discrete distributions
# Input:
#   Discrete Uniform:
#       A, B: integer in [0, 100] NOTE: bounds are arbitrary
#   Binomial:
#       n: integer in [0,100] NOTE: upper bound is arbitrary
#       p: real in [0,1]
#   Poisson:
#       λ: real in (0, ∞)
# Output:
#   Barplot of PMF of specified distribution with user specified parameters
#
# G Novak 2022-01-04

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Basic Discrete Distributions Demo"),
        tabsetPanel(
            tabPanel("Intro",
                     value = "discrete",
                     br(),
                     h1("Objective:"),
                     h3("Explore the effect of parameters on the support
                        and PMF of commonly used discrete distributions."),
                     br(),
                     br(),
                     h1("Background:"),
                     h3("What is a probability model?"),
                     p("An attempt to mathematically explain the behavior of a random system.
                       Models have two important components:", strong("parameters"),
                       "(inputs) and", strong("support"), "(possible outputs). The parameters
                       allow the model to take into account the specifics of the situation
                       and the support describes all possible
                       outcomes of the situation."),
                     br(),
                     h3("What is a probability distribution?"),
                     p("A specific mathematical representation of the model and its
                       charactaristics. Some distributions are considered standard. These are
                       named, thei behavior well-studied, and often applicable to a wide
                       range of models."),
                     h3("What does a distribution being discrete mean?"),
                     p("The elements in the support are countable. Often this means they are
                       integers but this is not a requirement."),
                     br(),
                     h3("Do discrete distributions need discrete parameters?"),
                     p("Not necessarily. Each parameter has its own definition for legal
                       values. The discrete descignation specifically describes the support."),
                     br(),
                     h3("What is a probability mass function (PMF)?"),
                     p("A function that assigns a probability of occurance or observation
                     to each element of the support.")
                     ),
            tabPanel("Discrete Uniform",
                     value = "unif",
                     br(),
                     sidebarLayout(
                         # Parameter selection panel
                         sidebarPanel(
                             sliderInput("ab", "A, B", min = 0, max = 100, value = c(1, 10)),
                             # simplify
                             checkboxInput("s_unif", "Use simple labels", value = FALSE)
                         ),
                         # Output panel
                         mainPanel(
                             plotOutput("unifPlot")
                         )
                     )
            ),
            tabPanel("Binomial",
                     value = "binom",
                     br(),
                     sidebarLayout(
                         # Parameter selection panel
                         sidebarPanel(
                             # n
                             numericInput("n", "n", value = 10, min = 0, max = 100, step = 1),
                             # p
                             sliderInput("p", "p", min = 0.0, max = 1.0, value = 0.5),
                             # simplify
                             checkboxInput("s_binom", "Use simple labels", value = FALSE)
                         ),
                         # Output panel
                         mainPanel(
                             plotOutput("binomPlot")
                         )
                     )
                     ),
            tabPanel("Poisson",
                     value = "pois",
                     br(),
                     sidebarLayout(
                         # Parameter selection panel
                         sidebarPanel(
                             # lambda
                             numericInput("lambda", "λ", value = 1, min = 0.1, step = 0.1),
                             # simplify
                             checkboxInput("s_pois", "Use simple labels", value = FALSE)
                         ),
                         # Output panel
                         mainPanel(
                             textOutput("poisNote"),
                             plotOutput("poisPlot")
                         )
                     )
                     )
        ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$unifPlot <- renderPlot({
        # calculate pmf
        pmf <- c(dunif(input$ab[1]-1, input$ab[1], input$ab[2]),
                 rep(dunif((input$ab[2]+input$ab[1])/2, input$ab[1], input$ab[2]),
                     input$ab[2]-input$ab[1]+1),
                 dunif(input$ab[2]+1, input$ab[1], input$ab[2]))
        # draw barplot
        if (input$s_unif) {
            barplot(pmf,
                    names.arg = (input$ab[1]-1):(input$ab[2]+1),
                    main = paste0("Unif(", input$ab[1], ", ", input$ab[2], ")"),
                    xlab = "k",
                    ylab = "P(X = k)")
        } else {
            barplot(pmf,
                    names.arg = (input$ab[1]-1):(input$ab[2]+1),
                    main = paste0("Unif(A = ", input$ab[1], ", B = ", input$ab[2], ")"),
                    xlab = "Number of occurances",
                    ylab = "Probability of observation")
        }
    })
    output$binomPlot <- renderPlot({
        # calculate pmf
        pmf <- vector(mode = "numeric", length = input$n + 1L)
        for (k in 0:input$n) {
            pmf[k + 1] <- dbinom(k, input$n, input$p)
        }
        # draw barplot
        if (input$s_binom) {
            barplot(pmf,
                    names.arg = 0:input$n,
                    main = paste0("Binom(", input$n, ", ", input$p, ")"),
                    xlab = "k",
                    ylab = "P(X = k)")
        } else {
            barplot(pmf,
                    names.arg = 0:input$n,
                    main = paste0("Binom(n = ", input$n, ", p = ", input$p, ")"),
                    xlab = "Number of sucesses",
                    ylab = "Probability of observation")
        }
    })
    output$poisNote <- renderText({
        if (input$s_pois) {
            "NOTE: k \u2208 [0,\u221E); {k | P(X\u2264k) \u2264 0.98} displayed"
        } else {
            "NOTE: Support is 0 to \u221E, only lower 98% displayed"
        }
    })
    output$poisPlot <- renderPlot({
        # determine subset of support to display
        kmax <- qpois(0.98, input$lambda)
        # calculate pmf
        pmf <- vector(mode = "numeric", length = kmax + 1L)
        for (k in 0:kmax) {
            pmf[k + 1] <- dpois(k, input$lambda)
        }
        
        # draw barplot
        if (input$s_pois) {
            barplot(pmf,
                    names.arg = 0:kmax,
                    main = paste0("Pois(", input$lambda, ")"),
                    xlab = "k",
                    ylab = "P(X = k)")
        } else {
            barplot(pmf,
                    names.arg = 0:kmax,
                    main = paste0("Pois(λ = ", input$lambda, ")"),
                    xlab = "Number of occurances",
                    ylab = "Probability of observation")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
