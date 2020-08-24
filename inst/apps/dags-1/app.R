###################################################################
# Program:  Master of Science in Health Data Science, UNSW Sydney
# Course:   HDAT9700
# Chapter:  DAGS
# Topic:    Confounding bias
# Author:   Mark Hanly
###################################################################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dagitty)
library(ggdag)
library(dplyr)
library(DT)
library(sjPlot)

# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(), # Allows for maths equations

    # Application title
    titlePanel("Same three variables | Different assumptions | Different DAGS | Different models"),

    sidebarLayout(
        sidebarPanel(width=4,
            h3("Instructions"),
            p("The aim of this app is to illustrate how DAGS can be used to (i) represent assumptions about our data, and (ii) inform the selection of model variables."),
            p("The app allows you to estimate the effect of aspirin use (tx) on the risk of experiencing a pulmonary embolism (pe) given a third variable,
              high blood pressure (hbp). Two datasets containing the same three variables are randomly generated to reflect two distinct scenarios"),
            tags$ol(
                tags$li("A randomised control trial"),
                tags$li("Observational data")
            ),

            p("The data generation parameters are summarised below, and some of these can be adjusted. Importantly, there is one key difference between the two scenarios.
                In Scenario A (the RCT), aspirin use is randomised, therefore there is no association between high blood pressure and aspirin use.
                In Scenario B, there is an association between high blood pressure and aspirin use, reflecting that in an observational data setting,
                people regularly taking aspirin may be doing so becuase they have an underlying risk factor like high blood pressure."),

            br(),
            h4("Using the app can you demonstrate the following two points?"),

            tags$ol(
                tags$li("Using the dataset that mimics a randomised control trial, the estimated effect of aspirin use is the same, regardless of whether or not high blood pressure is
                        included in the model"),
                tags$li("Using the dataset that mimics observational data, the estimated effect of aspirin use will be biased, unless you control for high blood pressure."),
            ),
            hr(),
            h4("Simulation parameters"),
            p(HTML(paste0("Relative effect of high blood pressure on risk of aspirin use (hbp", "&rarr;", "tx)"))),
            helpText("This parameter applies to Scenario 2 only. When aspirin use is randomised there is no relationship between high blood pressure and aspirin use."),
            numericInput("hbp2tx", NULL,
                        value = 3.0, min = 1.0, max = 3.0, step = .5, width = '20%'),
            p(HTML(paste0("Relative effect of aspirin use on risk of pulmonary embolism (tx", "&rarr;", "pe)"))),
            helpText("This is the 'true' effect of the exposure of interest that we should be able to reproduce in our models."),
            numericInput("tx2pe", NULL,
                        value = 0.5, min = 0.25, max = 1.0, step = .05, width = '20%'),
            p(HTML(paste0("Relative effect of high blood pressure on risk of pulmonary embolism (hbp", "&rarr;", "pe)"))),
            helpText("Higher values result in a stronger relationship. A value of 1 indicates no association."),
            numericInput("hbp2pe", NULL,
                        value = 1.50, min = 1.0, max = 3.0, step = 0.5, width = '20%')

        ),


        mainPanel(width=8,
            column(width = 6,
                   h3("Scenario A: Randomised Control Trial"),
                   h4("Data generation mechanism"),
                   p("Scenario A reflects a randomised control trial exploring the association between aspirin use and blood pressure.
                            Participants have been randomised to receive antibiotics (treated) or a placebo pill (control)"),
                   br(),
                   p("The DAG below represents this data generation mechanism. Because aspirin use is under controlled of the researchers,
                            and randomised to a large number of subjects, we can assume there is causal effect of high blood pressure on aspirin usage."),
                   hr(),
                   tabsetPanel(
                       tabPanel(title = "Can you draw the DAG?", icon = icon("pen-nib"),
                                helpText("Try drawing a DAG to represent the assumptions about the variables under this scenario. Your DAG should include the
                                three variables: aspirin use (exposure), pulmonary embolism (outcome) and high blood pressure (covariate).")),
                       tabPanel("See the solution", icon = icon("project-diagram"),
                                fluidRow(
                                    column(width=7,
                                           plotOutput("dag1", width = '100%')
                                    ),
                                    column(width=5, br(),
                                           helpText("This DAG is consistent with the data generation mechansism under Scenario A (reflecting a RCT).
                                     Because aspirin use is randomised, we know that in a large sample there is no relationship between high blood pressure & aspirin use.
                                     Accordingly, there is no arrow from hbp to tx."))
                                )
                                ),
                       tabPanel(title = "View model estimates", icon = icon("table"),
                                h4("Covariate selection:"),
                                checkboxInput("inclhbp1", "Control for high blood pressure?", value = FALSE),
                                h4("Model summary:"),
                                uiOutput("eq1"),
                                h4("Estimated parameters:"),
                                htmlOutput("tab1")
                       ),
                       tabPanel(title = "View raw data", icon = icon("file-excel"),
                                DT::dataTableOutput("data1"))
                   )
            ),

            column(width = 6,
                   h3("Scenario B: Observational data"),
                   h4("Data generation mechanism"),
                   p("Pr(pe = 1 | tx = 0) = 0.1"),
                   p("Scenario A reflects a randomised control trial exploring the association between aspirin use and blood pressure.
                            Participants have been randomised to receive antibiotics (treated) or a placebo pill (control)"),

                   hr(),
                   tabsetPanel(
                       tabPanel(title = "Can you draw the DAG?", icon = icon("pen-nib"),
                                helpText("Try drawing a DAG to represent the assumptions about the variables under this scenario. Your DAG should include the
                                three variables: aspirin use (exposure), pulmonary embolism (outcome) and high blood pressure (covariate).")),
                       tabPanel("See the solution", icon = icon("project-diagram"),
                                fluidRow(
                                    column(width=7,
                                           plotOutput("dag2", width = '100%')
                                    ),
                                    column(width=5, br(),
                                           helpText(HTML(paste0("This DAG is consistent with the data generation mechanism under Scenario B (observational data).
                                       We know there is a relationship between high blood pressure & aspirin use so the DAG includes an arrow from hbp to tx.
                                       This results in a backdoor path between our treatment tx and exposure pe (the path tx", "&rarr;", "hbp", "&rarr;", "pe).",
                                                                br(), br(), "This backdoor path will introduce bias on our estimate of the effect of aspirin use
                                                   on pulmonary embolism (tx", "&rarr;", "pe). To avoid this bias we must close the backdoor path by
                                                   controlling for high blood pressure.")))
                                           )
                                )
                            ),
                       tabPanel(title = "View model estimates", icon = icon("table"),
                                h4("Covariate selection:"),
                                checkboxInput("inclhbp2", "Control for high blood pressure?", value = FALSE),
                                h4("Model summary:"),
                                uiOutput("eq2"),
                                h4("Estimated parameters:"),
                                htmlOutput("tab2")
                            ),
                       tabPanel(title = "View raw data", icon = icon("file-excel"),
                                DT::dataTableOutput("data2"))
                   )
                   )

        )
    )
)


#########################################################
# Server logic
#########################################################

server <- function(input, output) {

# Data for scenario 1 (RCT)
tx1 <- rbinom(10000, 1, .5)
hbp1 <- rbinom(10000, 1, .2)
df1 <- reactive(
    data.frame(tx = tx1,
              hbp = hbp1) %>%
        mutate(pe = case_when(
                tx==0 & hbp==0 ~ rbinom(10000, 1, .10),
                tx==1 & hbp==0 ~ rbinom(10000, 1, .10*input$tx2pe),
                tx==0 & hbp==1 ~ rbinom(10000, 1, .10*input$hbp2pe),
                tx==1 & hbp==1 ~ rbinom(10000, 1, .10*input$tx2pe*input$hbp2pe)
               )
        )
)

# Data for scenario 2 (Observational)
hbp2 <- rbinom(10000, 1, .2)
df2 <- reactive(
    data.frame(hbp = hbp2) %>%
        mutate(
            tx = case_when(
                hbp==0 ~ rbinom(10000, 1, .2),
                hbp==1 ~ rbinom(10000, 1, .2*input$hbp2tx)
            ),
            pe = case_when(
                tx==0 & hbp==0 ~ rbinom(10000, 1, .10),
                tx==1 & hbp==0 ~ rbinom(10000, 1, .10*input$tx2pe),
                tx==0 & hbp==1 ~ rbinom(10000, 1, .10*input$hbp2pe),
                tx==1 & hbp==1 ~ rbinom(10000, 1, .10*input$tx2pe*input$hbp2pe)
        )
        )
)


# Dataset 1
output$data1 <- DT::renderDataTable(df1())
output$data2 <- DT::renderDataTable(df2())

# Dag 1
output$dag1 <- renderPlot({

    coords <- list(
        x = c(tx = 0, pe = 1, hbp = 0),
        y = c(tx = 2, pe = 1, hbp = 0)
    )

    dagify(pe ~ tx, pe ~ hbp,
           labels = c(tx="aspirin", pe="pulmonary embolism", hbp="high blood pressure"),
           coords = coords) %>%
        ggdag(use_labels = "label") +
        geom_dag_edges_link(label= c(rep(input$hbp2pe,100), rep(input$tx2pe, 100)),
                            hjust = c(rep(-2,100), rep(-2,100)),
                            edge_colour = "red") +
        theme_dag()

})

# Dag 2
output$dag2 <- renderPlot({

    coords <- list(
        x = c(tx = 0, pe = 1, hbp = 0),
        y = c(tx = 2, pe = 1, hbp = 0)
    )

    dagify(pe ~ tx, pe ~ hbp, tx ~ hbp,
           labels = c(tx="aspirin", pe="pulmonary embolism", hbp="high blood pressure"),
           coords = coords) %>%
        ggdag(use_labels = "label") +
        geom_dag_edges_link(label= c(rep(input$hbp2pe,100), rep(input$hbp2tx, 100), rep(input$tx2pe, 100)),
                            hjust = c(rep(-2,100), rep(-1,100), rep(-2,100)),
                            edge_colour = "red") +
        theme_dag()

})

# The math equations
output$eq1 <- renderUI({
    if (input$inclhbp1==FALSE)
    withMathJax(
        h4('$$\\text{log}(pe) = \\beta_{0} + \\beta_{1}tx $$')
    )

    else if (input$inclhbp1==TRUE)
    withMathJax(
        h4('$$\\text{log}(pe) = \\beta_{0} + \\beta_{1}tx + \\beta_{2}hbp $$')

    )
})

# The math equations
output$eq2 <- renderUI({
    if (input$inclhbp2==FALSE)
        withMathJax(
            h4('$$\\text{log}(pe) = \\beta_{0} + \\beta_{1}tx $$')
        )

    else if (input$inclhbp2==TRUE)
        withMathJax(
            h4('$$\\text{log}(pe) = \\beta_{0} + \\beta_{1}tx + \\beta_{2}hbp $$')

        )
})


# Model results for scenario 1
output$tab1 <- renderText({
    mod1 <- if (input$inclhbp1==FALSE) {
           glm(pe ~ tx, binomial(link = "log"), data=df1()) } else {
           glm(pe ~ tx + hbp, binomial(link = "log"), data=df1()) }
    tab <- tab_model(mod1, dv.labels = "Estimated effect of aspirin use on risk of pulmonary embolism", show.r2 = FALSE)
    HTML(tab$knitr)
})

# Model results for scenario 2
output$tab2 <- renderText({
    mod2 <- if (input$inclhbp2==FALSE) {
        glm(pe ~ tx, binomial(link = "log"), data=df2()) } else {
            glm(pe ~ tx + hbp, binomial(link = "log"), data=df2()) }
    tab <- tab_model(mod2, dv.labels = "Estimated effect of aspirin use on risk of pulmonary embolism", show.r2 = FALSE)
    HTML(tab$knitr)
})

}
# Run the application
shinyApp(ui = ui, server = server)
