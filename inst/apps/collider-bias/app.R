###################################################################
# Program:  Master of Science in Health Data Science, UNSW Sydney
# Course:   HDAT9700
# Chapter:  DAGS
# Topic:    Collider bias
# Author:   Mark Hanly
###################################################################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dagitty)
library(ggdag)
library(ggpubr)
library(dplyr)
library(DT)
library(sjPlot)

# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(), # Allows for maths equations

    # Application title
    titlePanel("Collider bias | The dog that didn't bark"),

    sidebarLayout(
        sidebarPanel(width=4,
                     h3("Instructions"),
                     p("The aim of this app is to illustrate how controlling for a collider introduces bias."),
                     p("The app allows you to estimate the effect of RSV infection during infancy on the
                     risk of being diagnosed with asthma given a third variable,
                          number of Emergency Department admissions. The data generation is fixed such that the
                       risk of having asthma is 20% for children who did not experienced RSV infection and 40% for those who
                       did experience RSV infection during early childhood. These numbers are artificially high to
                       help illustrate the point, but don't worry about that, the key thing is that we know the answer:
                       the relative risk for asthma comparing RSV to no RSV is 2.0."),
                     br(),
                     h3("Task"),
                     tags$ol(
                         tags$li("Demonstrate that controling for a collider biases the estimate of interest.")
                     ),

                     h4("Simulation parameters"),
                     p(HTML(paste0("Effect of RSV infection during infancy on number of ED admissions (RSV infection", "&rarr;", "ED)"))),
                     helpText("Setting this parameter to 1 means that RSV infection has no impact on ED admissions."),
                     numericInput("rsv2ed", NULL, value = 3.0, min = 1.0, max = 3.0, step = .5, width = '20%'),
                     hr(),
                     p(HTML(paste0("Effect of asthma on number of ED admissions (Asthma", "&rarr;", "ED)"))),
                     helpText("Setting this parameter to 1 means that asthma has no impact on ED admissions."),
                     numericInput("ast2ed", NULL, value = 3.0, min = 1.0, max = 3.0, step = .5, width = '20%'),
        ),


        mainPanel(

            fluidRow(
                column(width=6,
                       h4("ED admissions by RSV infection and asthma status"),
                       plotOutput("plot"),
                       textOutput("cor1"),
                       textOutput("cor2")
                       ),
                column(width = 6,
                       h4("DAG implied by choices of parameters"),
                       plotOutput("dags"),
                       p("Remember, ED is a collider if there are two arrows pointing towards it.")
                       )
            ),

            hr(),
            tabsetPanel(

                tabPanel(title = "View model estimates", icon = icon("table"),

                         fluidRow(
                             column(width=6,
                                    h4("Covariate selection:"),
                                    checkboxInput("inclhbp1", "Control for number of ED admissions?", value = FALSE),
                                    helpText("If ED is a collider, adding it to the model will introduce bias on the estimated effect of RSV."),
                                    hr(),
                                    h5("Model summary:"),
                                    uiOutput("eq1")),
                             column(width=6,
                                    h4("Estimated parameters:"),
                                    helpText("Remember, the confidence intervals for rsv should be centred on 2!"),
                                    htmlOutput("tab1")
                                    )
                         )
                         ),
                tabPanel("View raw data", icon = icon("file-excel"),
                         DT::dataTableOutput("data1")
                         )


            )

        )

))

#########################################################
# Server logic
#########################################################

server <- function(input, output) {

    # 1
    coords <- list(
        x = c(X = 0, Y = 2.5, M = 2),
        y = c(X = 2, Y = 3, M = 0)
    )

    dag1 <- dagify(Y ~ X, M ~ X, M ~ Y,
                   labels = c(X="RSV Infection", Y="Asthma", M="ED"),
                   coords = coords) %>%
        ggdag(use_labels = "label", text=FALSE, node_size = 8) +
        geom_dag_edges_link(arrow = grid::arrow(length = unit(10, "pt"), type = 'closed'), edge_colour = "red") +
        theme_dag() +
        border() +
        scale_y_continuous(limits=c(-1, 4)) +
        scale_y_continuous(limits=c(-1, 4)) +
        labs(title = "ED influenced by infection & asthma")

    # 3
    dag2 <- dagify(Y ~ X, M ~ Y,
                   labels = c(X="RSV Infection", Y="Asthma", M="ED admissions"),
                   coords = coords) %>%
        ggdag(use_labels = "label", text=FALSE, node_size = 8) +
        geom_dag_edges_link(arrow = grid::arrow(length = unit(10, "pt"), type = 'closed'), edge_colour = "red") +
        theme_dag() +
        border() +
        scale_y_continuous(limits=c(-1, 4)) +
        scale_y_continuous(limits=c(-1, 4)) +
        labs(title = "ED influenced by asthma only")

    # 3
    dag3 <- dagify(Y ~ X, M ~ X,
                   labels = c(X="RSV Infection", Y="Asthma", M="ED admissions"),
                   coords = coords) %>%
        ggdag(use_labels = "label", text=FALSE, node_size = 8) +
        geom_dag_edges_link(arrow = grid::arrow(length = unit(10, "pt"), type = 'closed'), edge_colour = "red") +
        theme_dag() +
        border() +
        scale_y_continuous(limits=c(-1, 4)) +
        scale_y_continuous(limits=c(-1, 4)) +
        labs(title = "ED influenced by infection only")

    # 4
    dag4 <- dagify(Y ~ X, M ~ 1,
                   labels = c(X="RSV Infection", Y="Asthma", M="ED admissions"),
                   coords = coords) %>%
        ggdag(use_labels = "label", text=FALSE, node_size = 8) +
        geom_dag_edges_link(arrow = grid::arrow(length = unit(10, "pt"), type = 'closed'), edge_colour = "red") +
        theme_dag() +
        border() +
        scale_y_continuous(limits=c(-1, 4)) +
        scale_y_continuous(limits=c(-1, 4)) +
        labs(title = "ED independent of infection & asthma")



df <- reactive({
    req(input$rsv2ed, input$ast2ed)

    rsv = rbinom(10000, 1, .2)
    df <- data.frame(rsv = rsv) %>%
        mutate(
            ast = case_when(
                rsv==0 ~ rbinom(10000, 1, .2),
                rsv==1 ~ rbinom(10000, 1, .4)
            ),
            ed = case_when(
                rsv==0 & ast==0 ~ rpois(10000, 1),
                rsv==1 & ast==0 ~ rpois(10000, input$rsv2ed),
                rsv==0 & ast==1 ~ rpois(10000, input$ast2ed),
                rsv==1 & ast==1 ~ rpois(10000, input$rsv2ed*input$ast2ed)
            ),
            group = interaction(rsv, ast)
        )
df
})


# Reactive bar plot
output$plot <- renderPlot({
    req(input$rsv2ed, input$ast2ed)

    df() %>%
        ggplot(aes(x=ed, fill = group)) +
        geom_bar(alpha = .9) +
        facet_grid(rsv~ast)  +
        scale_fill_discrete(name = NULL,
                            labels = c("No infection, no asthma",
                                       "Infection, no asthma",
                                       "No infection, Asthma",
                                       "No asthma, no infection")) +
        guides(fill = guide_legend(nrow=2)) +
        theme(legend.position="top") +
        labs(y = "Count", x = "Number of ED admissions")

})


# Correlations
cor1 <- reactive(cor(df()$rsv, df()$ed))
cor2 <- reactive(cor(df()$ast, df()$ed))

output$cor1 <- renderText({
    paste("Correlation between RSV and ED admission:", format(cor1(), digits = 2))
    })

output$cor2 <- renderText({
    paste("Correlation between asthma and ED admission:", format(cor2(), digits = 2))
})


# The DAG, chosen reactively
output$dags <- renderPlot({
    req(input$rsv2ed, input$ast2ed)

    if (input$rsv2ed != 1 & input$ast2ed != 1) {
        dag1
    }

    else if (input$rsv2ed == 1 & input$ast2ed != 1) {
        dag2
    }

    else if (input$rsv2ed != 1 & input$ast2ed == 1) {
        dag3
    }

    else if (input$rsv2ed == 1 & input$ast2ed == 1) {
        dag4
    }

})

output$data1 <- DT::renderDataTable(df())

# The math equations
output$eq1 <- renderUI({
    if (input$inclhbp1==FALSE)
        withMathJax(
            h4('$$\\text{log}(Asthma) = \\beta_{0} + \\beta_{1}Infection $$')
        )

    else if (input$inclhbp1==TRUE)
        withMathJax(
            h4('$$\\text{log}(Asthma) = \\beta_{0} + \\beta_{1}Infection + \\beta_{2}ED $$')

        )
})

# Model results for scenario 1
output$tab1 <- renderText({
    mod1 <- if (input$inclhbp1==FALSE) {
        glm(ast ~ rsv, poisson(link = "log"), data=df()) } else {
            glm(ast ~ rsv + ed, poisson(link = "log"), data=df()) }
    tab <- tab_model(mod1, dv.labels = "Estimated effect of RSV infection on asthma", show.r2 = FALSE)
    HTML(tab$knitr)
})


}
# Run the application
shinyApp(ui = ui, server = server)
