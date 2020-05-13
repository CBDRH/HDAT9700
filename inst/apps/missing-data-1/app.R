#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring systematic missing data bias"),

sidebarLayout(
    sidebarPanel(
    h3("Instructions"),
        p(
            "This app simulates a scenario where we would like to estimate the average height
            in a population based on a sample of 1,000 individuals, where a certain proportion
            of our sample is missing data on height. By design, the population mean of height is
            set to 170cm, so we can explore how our estimate varies from this known value based
            on two variable factors:"),
    tags$ol(
        tags$li("The correlation between the true value of height and the probability of height being unobserved."),
        tags$li("The proportion of missing values.")
        ),
    br(),
    h4("Using the app can you demonstrate the following three points?"),

    tags$ol(
        tags$li("If the correlation between the true value of height and the probability of height being unobserved is zero, then the estimate of height based on the observed data is unbiased regardless of the proportion of missing data."),
        tags$li("As the magnitude of the correlation increases, the bias increases (i.e. the gap between the known sample mean and the observed data estimate of the mean)."),
        tags$li("A negative correlation results in a positive bias and a positive correlation results in a negative bias."),
    )

    ),


mainPanel(
    column(width=6,
           plotOutput("plot1")
    ),

    column(width=6,
           plotOutput("plot2")
    ),
    column(width = 12, h3("")),
    column(width = 12,
           shinydashboard::valueBoxOutput("info1"),
           shinydashboard::valueBoxOutput("info2"),
           shinydashboard::valueBoxOutput("info3")
    ),
    column(width = 12, h3("")),
    column(width = 5,
           sliderInput("rho", "Correlation between height and the chances of height being unobserved", min = -0.5, max = 0.5, step = .01, value = 0, animate = TRUE),
           sliderInput("pct", "Proportion of missing data", min = 0, max = .5, step=.01, value = .1, animate = TRUE)
    ),
    column(width = 7,
           br(),
           helpText("A positive correlation between height and the chances of height being observed means that tall individuals are more likley to have missing data. As a result the estimate for average height will be biased downwards (lower than the known sample value)."),
           br(),
           helpText("A negative correlation between height and the chances of height being observed means that short individuals are more likley to have missing data. As a result the estimate for average height will be biased upwards (higher than the known sample value).")
    )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Set up data s random draws simulating height
    heights <- rnorm(1000,170,10)
    x <- rnorm(1000)
    dt <- data.frame(heights, x)

    # Ampute data based on user input
    test <- reactive({
        mice::ampute(dt,
                     prop = input$pct,
                     patterns = c(0,1),
                     freq = c(1),
                     mech = "MNAR",
                     weights = c(input$rho*2, 1 - abs(input$rho*2)))
    })

    # Create a datframe indicating the missing values of height
    df <- reactive({
        data.frame(height = test()$data$heights, Missing = is.na(test()$amp$heights))
    })

    est <- reactive({
        data.frame(
            e = "Mean (95% CI)",
            mean = mean(df()$height[!df()$Missing]),
            se = sd(df()$height[!df()$Missing])/sqrt(sum(!df()$Missing))
        )
    })

    # histograms for complete and missing data
    output$plot1 <- renderPlot({

        ggplot(data=df(), aes(x=height, fill=Missing)) +
            geom_histogram(alpha = 0.6, position = "identity") +
            scale_fill_manual(values=c("#fdae6b", "darkseagreen2")) +
            geom_vline(xintercept = mean(df()$height[df()$Missing]), color = "darkseagreen4", size = 1.5, linetype = "dashed") +
            geom_vline(xintercept = mean(df()$height[!df()$Missing]), color = "#e6550d", size = 1.5, linetype = "dashed") +
            coord_cartesian(ylim=c(0,100), xlim=c(140,200)) +
            theme(legend.position = c(0.15, 0.85)) +
            labs(x = "Height (cm)",
                 y = "Count",
                 title = "Distribution of height for observed and missing observations",
                 subtitle = "Based on a sample of 1000 individuals")

    })

    # Plot of estimate + 95% CI
    output$plot2 <- renderPlot({

        ggplot(data = est(), aes(x = e, y = mean)) +
            geom_linerange(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), size=1.25) +
            geom_point(size = 3) +
            geom_hline(yintercept = mean(df()$height), color = "red") +
            coord_cartesian(ylim=c(160,180)) +
            labs(y = "Height (cm)",
                 x = "Observed data estimate",
                 title = "Observed data estimate for average height",
                 subtitle = "Compared to known sample value")

    })

    # Calcumate summary measures to pass to value boxes
    bias <- reactive({
        round(mean(df()$height) - mean(df()$height[!df()$Missing]), digits = 1)
    })

    height1 <- reactive({
        round(mean(df()$height), digits = 1)
    })

    height2 <- reactive({
        round(mean(df()$height[!df()$Missing]), digits = 1)
    })

    # Create value boxes to summarise bias
    output$info1 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(value = height1(), subtitle = "Height in whole sample (cm)",
                                 icon = shiny::icon("bar-chart"), color = "green", width = 4)
    })


    output$info2 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(value = height2(), subtitle = "Height in observed data (cm)",
                                 icon = shiny::icon("bar-chart"), color = "green", width = 4)
    })


    output$info3 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(value = bias(), subtitle = "Bias (cm)",
                                 icon = shiny::icon("bar-chart"), color = "green", width = 4)
    })

}

# Run the application
shinyApp(ui = ui, server = server)
