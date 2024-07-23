###################################################################
# Program:  Master of Science in Health Data Science, UNSW Sydney
# Course:   HDAT9700
# Chapter:  Missing data and multiple imputation
# Topic:    Missing data bias
# Author:   Mark Hanly
# Note:     This version strips away annotation for embedding directly in context
###################################################################

library(shiny)
library(shinydashboard)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(

    sidebarPanel(width=3,
      sliderInput("rho", "Correlation between height and the chances of height being unobserved", min = -0.5, max = 0.5, step = .01, value = 0, animate = TRUE),
      sliderInput("pct", "Proportion of missing data", min = 0, max = .5, step=.01, value = .1, animate = TRUE)
    ),


    mainPanel(width=9,
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

        ggplot(data=df(), aes(x=height, fill=Missing, color=Missing)) +
            geom_density(alpha = 0.8, linewidth=1.2) +
            scale_fill_manual(NULL, labels = c("Observed", "Missing"), values=c("darkseagreen2", "#fdae6b")) +
            scale_color_manual(NULL, labels = c("Observed", "Missing"), values=c("darkseagreen2", "#fdae6b")) +
            geom_vline(xintercept = mean(df()$height[df()$Missing]), color = "#e6550d", size = 1.5, linetype = "dashed") +
            geom_vline(xintercept = mean(df()$height[!df()$Missing]), color = "darkseagreen4", size = 1.5, linetype = "dashed") +
            coord_cartesian(xlim=c(140,200)) +
            theme(legend.position = c(0.15, 0.85)) +
            labs(x = "Height (cm)",
                 y = "Count",
                 title = "Distribution of height for observed and missing observations",
                 subtitle = "Based on a sample of 1000 individuals")

    })

    # Plot of estimate + 95% CI
    output$plot2 <- renderPlot({

        ggplot(data = est(), aes(x = e, y = mean)) +
            geom_hline(yintercept = mean(df()$height), color = "#e6550d") +
            geom_linerange(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), size=1.25) +
            geom_point(size = 3) +
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
