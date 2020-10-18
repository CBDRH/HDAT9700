###################################################################
# Program:  Master of Science in Health Data Science, UNSW Sydney
# Course:   HDAT9700
# Chapter:  Multilevel modelling I
# Topic:    Visualising multilevel models
# Author:   Mark Hanly
###################################################################

library(shiny)
library(shinydashboard)
library(dplyr)
library(lme4)
library(ggplot2)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Visualising multilevel models", titleWidth = 500),

    # Sidebar with a slider input for number of bins

        dashboardSidebar(width = 500,
            h3("Data generation"),
            hr(),
            h4("Sample size"),
                fluidRow(
                    column(width = 6,
                           sliderInput("n2", "N level 2 groups", min = 4, max = 12, step = 1, value = 6)
                           ),
                    column(width = 6,
                           sliderInput("n1", "N level 1 units per group", min = 5, max = 50, step = 1, value = 20)
                    )
                ),
            hr(),

            h4("Fixed parameters"),
            fluidRow(
                column(width = 6,
                       sliderInput("beta0", HTML(paste0("Grand intercept ", "&beta;", tags$sub("0"))), min = -2, max = 2, step = .01, value = 0)
                ),
                column(width = 6,
                       sliderInput("beta1", HTML(paste0("Grand slope ", "&beta;", tags$sub("1"))), min = -2, max = 2, step = .01, value = 1)
                )
            ),

            hr(),
            h4("Random parameters"),
            radioButtons("modType", "Data generation model type",
                         choiceNames = c("Random intercept", "Random slope", "Random intercept and slope"),
                         choiceValues = c(1,2,3),
                         selected = 1,
                         inline = TRUE ),

               conditionalPanel(
                   condition = "input.modType == 1",
                   sliderInput("sigsqu0", HTML(paste0("Level 2 intercept variance ", "&sigma;", tags$sup("2"), tags$sub("u0"))), min = .1, max = 2, step = 0.01, value = .4)
               ),
               conditionalPanel(
                   condition = "input.modType == 2",
                   sliderInput("sigsqu1", HTML(paste0("Level 2 slope variance ", "&sigma;", tags$sup("2"), tags$sub("u1"))), min = .1, max = 1, step = 0.01, value = .3)
               ),
               conditionalPanel(
                   condition = "input.modType == 3",
                   sliderInput("sigsqu0x", HTML(paste0("Level 2 intercept variance ", "&sigma;", tags$sup("2"), tags$sub("u0"))), min = .1, max = 2, step = 0.01, value = .2),
                   sliderInput("sigsqu1x", HTML(paste0("Level 2 slope variance ", "&sigma;", tags$sup("2"), tags$sub("u1"))), min = .1, max = 1, step = 0.01, value = .4),
                   sliderInput("sig01", HTML(paste0("Level 2 intercept-slope covariance variance ", "&sigma;", tags$sub("u01"))), min = -1, max = 1, step = 0.01, value = .1)
               ),
            sliderInput("sigsqe", HTML(paste0("Level 1 variance ", "&sigma;", tags$sup("2"), tags$sub("e"))), min = .1, max = 2, step = 0.01, value = .2)

    ), # Closes dashboard sidebar

        # Show a plot of the generated distribution
    dashboardBody(withMathJax(),
                  fluidRow(
                      column(width = 10,
                      box(width = 12, title = "Information", collapsible = TRUE, solidHeader = TRUE, status = "primary",
                        p("Use the lefthand panel to generate multilvel data. Choose from a random intercept ,
                        random slope or random intercept and random slope data generation model."),

                        p("The plot below visualises the raw data as well as fitted group lines based on a model of your choice.
                            The fitted model can match the data generation mechansim or can be different;
                          you should see the AIC drop when the data generation and fitted models are the same."),

                        p("Make sure you can pick out the key model parameters from the R model summary output.")
                        )

                      ),
                      column(width = 2,
                             img(src='UNSW_2017_Big_Data_landscape.jpg', align = "right", height = '100%', width = '100%')
                             )

                  ),

                  fluidRow(
                      column(width = 5,
                             box(width = 12, title = "Data generation model", collapsible = TRUE, solidHeader = TRUE, status = "info",
                                 uiOutput("eq1")
                             )
                      ),

                      column(width = 5,
                             box(width = 12, title = "Estimated model", collapsible = TRUE, solidHeader = TRUE, status = "info",
                                 uiOutput("eq2")
                             )
                      ),
                      column(width=2,
                             uiOutput("info1"),
                             uiOutput("info2")
                      )
                  ),

        fluidRow(
            column(width = 5,
                   box(width = 12, title = "Model specification", collapsible = TRUE,
                       radioButtons("modFit", "Type of multilevel model to fit",
                                    choiceNames = c("Random intercept", "Random slope", "Random intercept and slope"),
                                    choiceValues = c(1,2,3),
                                    selected = 1, inline = TRUE),

                       # checkboxGroupInput("stdVars", label = "Model the standardised model variables?",
                       #                    choices = c("x", "y"), inline = TRUE
                       # ),

                    plotOutput("plot"),
                    textOutput("syntax")
                   )
                ),

            column(width = 5,
                   box(width = 12, title = "Model fit summary", collapsible = TRUE,
                    verbatimTextOutput("summary")
                    )
                ),

            column(width=2,
                   uiOutput("beta0"),
                   uiOutput("beta1"),

                   conditionalPanel(
                       condition = "input.modFit == 1",
                   uiOutput("sigsqu0")
                   ),

                   conditionalPanel(
                       condition = "input.modFit == 2",
                       uiOutput("sigsqu1")
                   ),

                   conditionalPanel(
                       condition = "input.modFit == 3",
                       uiOutput("sigsqu0x"),
                       uiOutput("sigsqu1x"),
                       uiOutput("sigu01"),
                   ),

                 uiOutput("sigsqe")
                 )
            )

        ) # Closes dashboard body

) # Closes dashboard page

#########################################################
# Server logic
#########################################################

server <- function(input, output) {

# Define the group colours
colVec <- reactive(brewer.pal(input$n2, "Set3" ))

# Define the total numberof observations
n <- reactive(input$n1*input$n2)

# Reactive values for model parameters
params <- reactiveValues(u0j=0, u1j=0, eij=0)



# Reactive values for model variables
var <- reactiveValues(x="x", y="y")
observeEvent(input$stdVars, {
    var$x <- ifelse("x" %in% input$stdVars, "xstd", "x")
    var$y <- ifelse("y" %in% input$stdVars, "ystd", "y")
}, ignoreNULL = FALSE, ignoreInit = FALSE)

# Reactive values for axis limits
lim <- reactiveValues(ymin = 0, ymax = 25, xmin = 6, xmax = 18)

dataFixed <- reactive({
    dt1 <- data.frame(
        j = factor(sort(rep(seq(1, input$n2), input$n1))),
        i = rep(seq(1, input$n1), input$n2),
        x = rnorm(n(), 4, 2),
        eij = rnorm(n(), 0, sqrt(input$sigsqe))
    )
})

data <- reactive({

    if (input$modType==1) {
        params$u0j <- rnorm(input$n2, 0, sqrt(input$sigsqu0))
        params$u1j <- 0
    }

    else if (input$modType==2) {
        params$u0j <- 0
        params$u1j <- rnorm(input$n2, 0, sqrt(input$sigsqu1))
    }

    else if (input$modType==3) {

        # Define the variance - covariance matrix
        varcovarMat <- matrix(c(input$sigsqu0x, input$sig01, input$sig01, input$sigsqu1x), nrow=2)
        uDraws <- MASS::mvrnorm(input$n2, c(0,0), Sigma = varcovarMat)

        params$u0j <- uDraws[,1]
        params$u1j <- uDraws[,2]
    }


    dt2 <- data.frame(
        j = factor(seq(1, input$n2)),
        u0j = params$u0j,
        u1j = params$u1j
    )

    dt3 <- left_join(dataFixed(), dt2, by = "j") %>%
        mutate(y = input$beta0 + input$beta1*x + u0j + u1j*x + eij,
               xstd = (x - mean(x))/sd(x),
               ystd = (y - mean(y))/sd(y)
        )

    # Calculate the lims reactively
    lim$xmin <- ifelse("x" %in% input$stdVars, min(dt3$xstd), min(dt3$x))
    lim$xmax <- ifelse("x" %in% input$stdVars, max(dt3$xstd), max(dt3$x))
    lim$ymin <- ifelse("y" %in% input$stdVars, min(dt3$ystd), min(dt3$y))
    lim$ymax <- ifelse("y" %in% input$stdVars, max(dt3$ystd), max(dt3$y))

    return(dt3)

})


mod <- reactiveValues()
model <- reactive({

    if (input$modFit==1) {lvl2Spec <- "(1|j)"}
    if (input$modFit==2) {lvl2Spec <- paste("(0 +", var$x, "|j)")}
    if (input$modFit==3) {lvl2Spec <- paste("(1 +", var$x, "|j)")}

    lmer(as.formula(paste(var$y, "~", var$x, "+", lvl2Spec)), data = data())

    })


output$summary <- renderPrint(
    summary(model())
)

output$test <- renderText(
    paste(mean(predict(model())))
)



output$plot <- renderPlot({

data() %>%
    mutate(pf = predict(model(), re.form = NULL),
           p = predict(model())) %>%
        ggplot() +
            geom_smooth(aes_string(x=var$x, y="pf"), method='lm', formula = y ~ x, se=FALSE, color = "black", size = 1) +
            geom_point(aes_string(x=var$x, y=var$y, color='j')) +
            geom_point(aes_string(x=var$x, y=var$y, color='j')) +
            geom_line(aes_string(x=var$x, y='p', color='j')) +
            scale_color_manual("Legend", values = colVec()) +
            scale_x_continuous(name = "Exposure (X)", limits = c(lim$xmin, lim$xmax)) +
            scale_y_continuous(name = "Outcome (Y)", limits = c(lim$ymin, lim$ymax)) +
            theme_dark() +
            theme(legend.position = "none") +
            labs(title = "Raw data and predicted group lines")

})

output$eq1 <- renderUI({

        if (input$modType == 1) {
            eqn1 <- paste0('$$\\text{Y} \\sim ', input$beta0, ' + ', input$beta1, 'X_{ij} + u_{0j} + e_{ij} $$')
            eqn2 <- paste0('$$u_{0j} \\sim \\text{N}(0, ', input$sigsqu0, ') $$')
        }

        if (input$modType == 2) {
            eqn1 <- paste0('$$\\text{Y} \\sim ', input$beta0, ' + ', input$beta1, 'X_{ij} + u_{1j}X_{ij} + e_{ij} $$')
            eqn2 <- paste0('$$u_{1j} \\sim \\text{N}(0, ', input$sigsqu1, ') $$')
        }

        if (input$modType == 3) {
            eqn1 <- paste0('$$\\text{Y} \\sim ', input$beta0, ' + ', input$beta1, 'X_{ij} + u_{0j} + u_{1j}X_{ij} + e_{ij} $$')
            eqn2 <- paste0('$$ {u_{0j} \\choose u_{1j}}
                           \\sim \\text{MVN} \\left[
                           {0 \\choose 0},
                           {\\begin{array}{cc}',
                           input$sigsqu0x, '&', input$sig01, '\\end{array} \\choose \\begin{array}{cc} ',
                           input$sig01, '&', input$sigsqu1x, '\\end{array}}
                           \\right]',
                           '$$')
        }
        eqn3 <- paste0('$$e_{ij} \\sim \\text{N}(0, ', input$sigsqe, ') $$')

        withMathJax(
            h4(eqn1),
            h4(eqn2),
            h4(eqn3)
        )
        })

output$eq2 <- renderUI({

    if (input$modFit == 1) {
        eqn1 <- paste0('$$\\text{Y} \\sim \\beta_{0} + \\beta_{1}X_{ij} + u_{0j} + e_{ij} $$')
        eqn2 <- paste0('$$u_{0j} \\sim \\text{N}(0, \\sigma^{2}_{u0}) $$')
    }

    if (input$modFit == 2) {
        eqn1 <- paste0('$$\\text{Y} \\sim \\beta_{0} + \\beta_{1}X_{ij} + u_{1j}X_{ij} + e_{ij} $$')
        eqn2 <- paste0('$$u_{1j} \\sim \\text{N}(0, \\sigma^{2}_{u1}) $$')
    }

    if (input$modFit == 3) {
        eqn1 <- paste0('$$\\text{Y} \\sim \\beta_{0} + \\beta_{1}X_{ij} + u_{0j} + u_{1j}X_{ij} + e_{ij} $$')
        eqn2 <- paste0('$$ {u_{0j} \\choose u_{1j}}
                           \\sim \\text{MVN} \\left[
                           {0 \\choose 0},
                           {\\begin{array}{cc} \\sigma^{2}_{u0} & \\sigma_{u01} \\end{array} \\choose
                       \\begin{array}{cc} \\sigma_{u01} & \\sigma^{2}_{u1} \\end{array}}
                           \\right]',
                       '$$')
    }
    eqn3 <- paste0('$$e_{ij} \\sim \\text{N}(0, \\sigma^{2}_{e}) $$')

    withMathJax(
        h4(eqn1),
        h4(eqn2),
        h4(eqn3)
    )
})


output$syntax <- renderText({

    if (input$modFit == 1) {
        x <- "lmer(y ~ 1 + x + (1 | groupvar), data = dt)"
    }

    if (input$modFit == 2) {
        x <- "lmer(y ~ 1 + x + (0 + x | groupvar), data = dt)"
    }

    if (input$modFit == 3) {
        x <- "lmer(y ~ 1 + x + (1 + x | groupvar), data = dt)"
    }

    print(paste("R Syntax:", x))

})

output$info1 <- renderUI({
    aic <- format(AIC(model()), digits = 0)
    shinydashboard::valueBox(value = aic, subtitle = "Model AIC",
                             icon = shiny::icon("bar-chart"), color = "aqua", width = '50%')
})

output$info2 <- renderUI({
    mod0 <- summary(lmer(y ~ 1 + (1|j) , data = data()))
    lvl1 <- (mod0$sigma)^2
    lvl2 <- as.numeric(mod0$varcor[1])
    vpc <- paste(format(100*lvl2/(lvl1+lvl2), digits = 2), "%")
    shinydashboard::valueBox(value = vpc, subtitle = "Variance partition coefficient",
                             icon = shiny::icon("bar-chart"), color = "aqua", width = '50%')
})

output$beta0 <- renderUI({
    beta0 <- format(summary(model())$coefficients[1], digits = 2, nsmall = 1)
    shinydashboard::valueBox(value = beta0, subtitle = HTML(paste0("Grand intercept ", "&beta;", tags$sub("0"))),
                             icon = shiny::icon("bar-chart"), color = "blue", width = '50%')
})

output$beta1 <- renderUI({
    beta1 <- format(summary(model())$coefficients[2], digits = 2, nsmall = 1)
    shinydashboard::valueBox(value = beta1, subtitle = HTML(paste0("Grand slope ", "&beta;", tags$sub("1"))),
                             icon = shiny::icon("bar-chart"), color = "blue", width = '50%')
})

output$sigsqu0 <- renderUI({
    sigsqu0 <- format(as.data.frame(summary(model())$varcor)[1,4], digits = 2, nsmall = 1)
    shinydashboard::valueBox(value = sigsqu0, subtitle = HTML(paste0("Level 2 intercept variance ", "&sigma;", tags$sup("2"), tags$sub("u0") )),
                             icon = shiny::icon("bar-chart"), color = "blue", width = '50%')
})

output$sigsqu1 <- renderUI({
    sigsqu1 <- format(as.data.frame(summary(model())$varcor)[1,4], digits = 2, nsmall = 1)
    shinydashboard::valueBox(value = sigsqu1, subtitle = HTML(paste0("Level 2 slope variance ", "&sigma;", tags$sup("2"), tags$sub("u1") )),
                             icon = shiny::icon("bar-chart"), color = "blue", width = '50%')
})

output$sigsqu0x <- renderUI({
    sigsqu0 <- format(as.data.frame(summary(model())$varcor)[1,4], digits = 2, nsmall = 1)
    shinydashboard::valueBox(value = sigsqu0, subtitle = HTML(paste0("Level 2 intercept variance ", "&sigma;", tags$sup("2"), tags$sub("u0") )),
                             icon = shiny::icon("bar-chart"), color = "blue", width = '50%')
})

output$sigsqu1x <- renderUI({
    sigsqu1 <- format(as.data.frame(summary(model())$varcor)[2,4], digits = 2, nsmall = 1)
    shinydashboard::valueBox(value = sigsqu1, subtitle = HTML(paste0("Level 2 slope variance ", "&sigma;", tags$sup("2"), tags$sub("u1") )),
                             icon = shiny::icon("bar-chart"), color = "blue", width = '50%')
})

output$sigu01 <- renderUI({
    sigu01 <- format(as.data.frame(summary(model())$varcor)[3,5], digits = 2, nsmall = 1)
    shinydashboard::valueBox(value = sigu01, subtitle = HTML(paste0("Level 2 intecept-slope covariance ", "&sigma;", tags$sup("2"), tags$sub("u1") )),
                             icon = shiny::icon("bar-chart"), color = "blue", width = '50%')
})

output$sigsqe <- renderUI({
    sigsqe <- format(summary(model())$sigma^2, digits = 2, nsmall = 1)
    shinydashboard::valueBox(value = sigsqe, subtitle = HTML(paste0("Level 1 residual variance ", "&sigma;", tags$sup("2"), tags$sub("e") )),
                             icon = shiny::icon("bar-chart"), color = "blue", width = '50%')
})

}

# Run the application
shinyApp(ui = ui, server = server)















