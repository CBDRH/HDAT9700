###################################################################
# Program:  Master of Science in Health Data Science, UNSW Sydney
# Course:   HDAT9700
# Chapter:  Matching for causal inference
# Topic:    Matching removes model dependency
# Author:   Mark Hanly
###################################################################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(MatchIt)
library(dplyr)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    #withMathJax(), # Allows for maths equations

    # Application title
    dashboardHeader(title = "Model dependency"),

    # Sidebar with a slider input for number of bins

        dashboardSidebar(width = 350,
            h3("Overview"),
            p(HTML(paste0(
                "This applet provides an interactive demonstration of model dependency and matching, following the example given by Prof Gary King in the chapter pre-reading available ",
                tags$a(href="https://youtu.be/rBv39pK1iEs?t=199", target="_blank", "here"), " (see from about 3:20 - 8:00 mins)."))),
            hr(),
            h3("Aims"),
            p("Using the applet, demonstrate the following points:"),
            tags$ol(
                tags$li("The treatment estimate varies depending on the model specification in the unmatched data."),
                tags$li("The treatment estimate is less dependent on the model specification in the matched data."),
                tags$li("The covariate (Education) is more balanced in the matched data."),
                tags$li("Over repeated samples, the estimates based on matched data are unbiased regardless of model specification, whereas the estimates
                        based on matched data are biased when the model is mis-specified")
            ),
            hr(),
            h3("Controls"),
            actionButton("button", "Sample", width = '50%', icon = icon('flag-checkered')),
            helpText("Click run to sample a new set of data"),
            actionButton("reset", "Reset", width = '50%', icon = icon('redo-alt')),
            helpText("Click reset to clear the previous estimates"),
            hr(),
            h3("Estimating the treatment effect"),
            helpText("Note the known, fixed treatment effect is 2.0"),
            radioButtons("data", "Choose data", choices = c("All data" = 1, "Matched data" = 2), inline = TRUE),
            radioButtons("shape", "Model specification", choices = c("Linear", "Quadratic"), inline = TRUE)
        ),

        # Show a plot of the generated distribution
    dashboardBody(fluidRow(img(src='UNSW_2017_Big_Data_landscape.jpg', align = "right", height = '25%', width = '25%')),
                  br(),
        column(width = 6,
               box(width = '100%',
                   plotOutput("plot")

               )
        ),

        column(width = 6,
               box(width = '100%',

                   plotOutput("balance")

               )
               ),

        #uiOutput("eq1"),
        br(),

            uiOutput("estBoxl0"),
            uiOutput("estBoxq0"),
            uiOutput("estBoxl1"),
            uiOutput("estBoxq1"),

        plotOutput("est")

        )

)

#########################################################
# Server logic
#########################################################

server <- function(input, output) {


    # Number of individuals to simulate
    n <- 200

    # Covariate X1
    x1 <- runif(n, 12, 28)

    # Probability of treatment
    #p <- ifelse(x1 < 16, 0.20, ifelse(x1 < 25, .3, 0.80))
    p <- ifelse(x1 < 15, 0.00, ifelse(x1 < 26, .5, 0.05))

    # Treatment indicator
    tx <- rbinom(n, 1, p)

    # Outcome
    #y <- ifelse(x1 < 20, -2 + 2*tx -12 + 1.2*x1, 2*tx + 18 - .6*x1 ) + rnorm(n, 0, 1)
    y <- 4 + 2*tx + .2*(x1) - 0.075*(x1-20)^2 + rnorm(n, 0, 1)

    # As a data.frame
    d0 <- data.frame(x1, tx, y) %>%
        mutate(lab = ifelse(tx, "T", "C"))

    # Find matches
    match1 <- matchit(tx ~ x1, data=d0, method = "nearest", distance = "logit", discard = 'both')

    # Add match indicator to data frame
    d <- d0 %>%
        mutate(
            match = match1$weights
        )


    ## Estimate models
    mods.l0 <- lm(y ~ tx + x1, data = d)
    mods.q0 <- lm(y ~ tx + x1 + I(x1^2) , data = d)
    mods.l1 <-  lm(y ~ tx + x1, data = d[d$match==1, ])
    mods.q1 <- lm(y ~ tx + x1 + I(x1^2), data = d[d$match==1, ])


    ## Set up reactive dataframes
    data <- reactiveValues()


    ## Extract data frame of point estimates
    data$est <- data.frame(
       allDataLinear = mods.l0$coefficients["tx"],
       allDataQuadratic = mods.q0$coefficients["tx"],
       matchDataLinear = mods.l1$coefficients["tx"],
       matchDataQuadratic = mods.q1$coefficients["tx"]
    )


    # Initialise the dataframe
    data$d <- d

observeEvent(input$button, {

    # Covariate X1
    x1 <- runif(n, 12, 28)

    # Probability of treatment
    #p <- ifelse(x1 < 16, 0.20, ifelse(x1 < 25, .3, 0.80))
    p <- ifelse(x1 < 15, 0, ifelse(x1 < 26, .5, 0.05))

    # Treatment indicator
    tx <- rbinom(n, 1, p)

    # Outcome
    #y <- ifelse(x1 < 20, -2 + 2*tx -12 + 1.2*x1, 2*tx + 18 - .6*x1 ) + rnorm(n, 0, 1)
    y <- 4 + 2*tx + .2*(x1) - 0.075*(x1-20)^2 + rnorm(n, 0, 1)

    # As a data.frame
    d0 <- data.frame(x1, tx, y) %>%
        mutate(lab = ifelse(tx, "T", "C"))

    # Find matches
    match1 <- matchit(tx ~ x1, data=d0, method = "nearest", distance = "logit", discard = 'both')

    d <- d0 %>%
        mutate(
            match = match1$weights
        )

    data$d <- d


    ## Estimate models
    mods.l0 <- lm(y ~ tx + x1, data = d)
    mods.q0 <- lm(y ~ tx + x1 + I(x1^2) , data = d)
    mods.l1 <-  lm(y ~ tx + x1, data = d[d$match==1, ])
    mods.q1 <- lm(y ~ tx + x1 + I(x1^2), data = d[d$match==1, ])

    # Add new point estimates

    data$est <- rbind(data$est,
                      data.frame(
                        allDataLinear = mods.l0$coefficients["tx"],
                        allDataQuadratic = mods.q0$coefficients["tx"],
                        matchDataLinear = mods.l1$coefficients["tx"],
                        matchDataQuadratic = mods.q1$coefficients["tx"]
                    )
    )

})

    # Choose raw data or matched data
    df <- reactive({

        if (input$data == 1) {
            return(data$d)
        }
        else if (input$data == 2) {
            return(data$d %>% filter(match == 1))
        }

    })


    # Choose linear or quadratic model based on user input
    model <- reactive({

        if (input$shape == "Linear") {
            model <- lm(y ~ tx + x1, data = df())
        }

        else if (input$shape == "Quadratic") {
            model <- lm(y ~ tx + x1 + I(x1^2), data = df())
        }

    })

    # The reactive plot of data with model fit lines
    output$plot <- renderPlot({

        ggplot(data=df(), aes(x = x1, y = y, label = lab, color = lab)) +
            geom_text(size=6) +
            scale_x_continuous(name = "Education (Years)", limits = c(12, 28), breaks = seq(12, 28, 2), minor_breaks = NULL) +
            scale_y_continuous(name = "Outcome", limits = c(0, 12), breaks = seq(0, 12, 2), minor_breaks = NULL) +
            scale_color_manual(values=c('dodgerblue','tomato2')) +
            geom_line(aes(x=x1, y=predict(model()))) +
            labs(title = "Scatterplot of education versus outcome for control and treated individuals") +
            theme(legend.position = "none", axis.text=element_text(size=14), axis.title=element_text(size=14), plot.title=element_text(size=16))


    })



    # Plot the balance
    output$balance <- renderPlot({

        ggplot(data = df(), aes(x=x1, fill=lab)) +
            geom_density(alpha=0.6) +
            scale_fill_manual(name = "Group", values=c('dodgerblue','tomato2'), label = c('Control', "Treated")) +
            scale_x_continuous(name = "Education (Years)", limits = c(12, 28), breaks = seq(12, 28, 2), minor_breaks = NULL) +
            scale_y_continuous(name = "Density", limits = c(0, 0.30), breaks = seq(0, .30, 0.10), minor_breaks = NULL) +
            theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
            labs(title = "Distribution of education in control and treated individuals") +
            theme(legend.position = "none", axis.text=element_text(size=14), axis.title=element_text(size=14), plot.title=element_text(size=16))

    })


    output$hano <- renderText(input$button)

    # The point estimates
    est <- reactive({
        paste0(
            format(model()$coefficients["tx"], digits=2, nsmall=1), " (",
            format(confint(model())["tx", 1], digits=2, nsmall=1), "-",
            format(confint(model())["tx", 2], digits=2, nsmall=1), ")")
    })




    # New facet label names
    est.labs <- c(
                allDataLinear = "All data | Linear model",
                allDataQuadratic = "All data | Quadratic model",
                matchDataLinear = "Matched data | Linear model",
                matchDataQuadratic = "Matched data | Quadratic model"
                )

    # Plot the estimates
    output$est <- renderPlot({

        data$est %>% tidyr::gather() %>%
            ggplot(aes(x = value, fill = key)) +
            geom_vline(xintercept = 2.0, size  = 2) +
            geom_dotplot(dotsize = 2, binwidth = 0.05) +
            scale_color_manual(values=c("#D81B60", "#F012BE", "#3D9970", "#01FF70")) +
            scale_fill_manual(values=c("#D81B60", "#F012BE", "#3D9970", "#01FF70")) +
            scale_x_continuous(name = "Point estimate", limits = c(0, 4), breaks = seq(0, 4, 1), minor_breaks = NULL) +
            scale_y_continuous(name = "Count", limits = c(0, 10), breaks = seq(0, 10, 1), minor_breaks = NULL) +
            facet_wrap(facets = vars(key), nrow = 1, labeller = labeller(key = est.labs)) +
            theme(legend.position = "none", axis.text=element_text(size=14), axis.title=element_text(size=14), strip.text.x=element_text(size=16))

    })


    # Reset estimates data on quest
        observeEvent(input$reset, {
            data$est <- data.frame(
                allDataLinear = mods.l0$coefficients["tx"],
                allDataQuadratic = mods.q0$coefficients["tx"],
                matchDataLinear = mods.l1$coefficients["tx"],
                matchDataQuadratic = mods.q1$coefficients["tx"]
            )
        })


    output$test <- renderText(nrow(data$est))


    # Info boxes

    # All data linear
    output$estBoxl0 <- renderUI({
        shinydashboard::infoBox(
            subtitle = "Estimated treatment effect",
            title = "All data | Linear fit",
            value = format(data$est[nrow(data$est), 1], digits=2, nsmall=1),
            icon = icon('hand-lizard'),
            width = 3,
            color = "maroon",
            fill = ifelse(input$data==1 & input$shape == "Linear", TRUE, FALSE)
        )
    })

    # All data quadratic
    output$estBoxq0 <- renderUI({
        shinydashboard::infoBox(
            subtitle = "Estimated treatment effect",
            title = "All data | Quadratic fit",
            value = format(data$est[nrow(data$est), 2], digits=2, nsmall=1),
            icon = icon('hand-lizard'),
            width = 3,
            color = "fuchsia",
            fill = ifelse(input$data==1 & input$shape == "Quadratic", TRUE, FALSE)
        )
    })

    # Matched data linear
    output$estBoxl1 <- renderUI({
        shinydashboard::infoBox(
            subtitle = "Estimated treatment effect",
            title = "Matched data | Linear fit",
            value = format(data$est[nrow(data$est), 3], digits=2, nsmall=1),
            icon = icon('hand-lizard'),
            width = 3,
            color = "olive",
            fill = ifelse(input$data==2 & input$shape == "Linear", TRUE, FALSE)
        )
    })

    # Matched data quadratic
    output$estBoxq1 <- renderUI({
        shinydashboard::infoBox(
            subtitle = "Estimated treatment effect",
            title = "Matched data | Quadratic fit",
            value = format(data$est[nrow(data$est), 4], digits=2, nsmall=1),
            icon = icon('hand-lizard'),
            width = 3,
            color = "lime",
            fill = ifelse(input$data==2 & input$shape == "Quadratic", TRUE, FALSE)
        )
    })

    # The math equations
    output$eq1 <- renderUI({
        if (input$shape=="Linear")
            withMathJax(
                h4('$$\\text{log}(Outcome) = \\beta_{0} + \\beta_{1}Treatment + \\beta_{2}Education $$')
            )

        else if (input$shape=="Quadratic")
            withMathJax(
                h4('$$\\text{log}(Outcome) = \\beta_{0} + \\beta_{1}Treatment + \\beta_{2}Education + \\beta_{2}Education^2 $$')

            )
    })

output$heni <- renderText(paste(data$est))

}

# Run the application
shinyApp(ui = ui, server = server)















