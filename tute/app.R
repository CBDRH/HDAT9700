library(shiny)
library(dplyr)
library(ggplot2)
library(dagitty)
library(ggdag)
source(here::here("tute/global.R"))

# Define UI
ui <- fluidPage(

    # Application title

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

        radioButtons("tuteID", "Tutorial", choiceValues = 1:nExamples, choiceNames = letters[1:nExamples]),
        checkboxGroupInput("tuteAns0", "Choose variables to control for:", choiceNames = "No adjustment needed", choiceValues = "NULL", inline = FALSE),
        checkboxGroupInput("tuteAns", NULL, choices = c('Z'), inline = FALSE, selected = NULL),
        actionButton("tuteSubmit", "Submit answer", icon = icon('share-square'), width = 140),
        actionButton("tuteReveal", "Reveal solution", icon = icon('project-diagram'), width = 140),
        textOutput('test')

        ),

        # Show a plot of the generated distribution
        mainPanel(

        plotOutput('tute',
                   click = "tute_click"),
        tags$div(style = 'text-align: center;',
            actionButton('previous', "Previous", icon = icon("arrow-left")),
            actionButton('advance', "Next", icon = icon("arrow-right"))
        ),
        plotOutput('solutionTute')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

tol <- .05

control <- reactiveValues()
pointer <- reactiveValues()

observe({
    req(input$tute_click$x, input$tute_click$y)
    pointer$x <- input$tute_click$x
    pointer$y <- input$tute_click$y
})

clickedVar <- reactive({

    req(pointer$x, pointer$y)

    tidyDagBase()$data %>%
        filter(between(x, pointer$x - tol, pointer$x + tol)) %>%
        filter(between(y, pointer$y - tol, pointer$y + tol)) %>%
        select(name) %>%
        filter(name != 'X') %>%
        filter(name != 'Y') %>%
        unlist() %>%
        unique()
})

observeEvent(input$tute_click, {

    req(clickedVar())

    if (clickedVar() %in% control$vars) {
        control$vars <- control$vars[! control$vars %in% clickedVar()]
    }
    else {
        control$vars <- append(control$vars, clickedVar()) %>% unique()
    }

})

# Define button values
button <- reactiveValues(submitTute = 0, revealTute = 0)

# Reveal button logic
observeEvent(input$tuteReveal, {
    button$revealTute <- abs(button$revealTute - 1)
})

# Define baseline values for tutorial menu
tute <- reactiveValues(P1 = 0, P2 = 0, P3 = 0, P4 = 0, P5 = 0, P6 = 0,
                       M1 = as.character(0), M2 = as.character(0), M3 = as.character(0), M4 = as.character(0), M5 = as.character(0), M6 = as.character(0),
                       C1 = as.character(0), C2 = as.character(0), C3 = as.character(0), C4 = as.character(0), C5 = as.character(0), C6 = as.character(0))

observe({
    if(tute$P1==1) {
        tute$M1 <- 'fa-check'
        tute$C1 <- '#67C067'
    }
    if(tute$P1==2) {
        tute$M1 <- 'fa-times'
        tute$C1 <- '#D04736'
    }

})

tidyDagBase <- reactive({
        dag <- eval(as.name(paste0('g', input$tuteID)))
        labs <- eval(as.name(paste0('label', input$tuteID)))

        dag %>%
            dagitty() %>%
            dag_label(labels = labs) %>%
            node_status
})

output$tute <- renderPlot({

    p <- tidyDagBase() %>%
            adjust_for(control$vars) %>%
            ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) +
            geom_dag_point(aes(color = status)) +
            geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed")) +
            geom_dag_collider_edges(color = 'pink') +
            geom_dag_text() +
            theme_dag()  +
            scale_adjusted() +
            geom_dag_label_repel(aes(label = label, fill = status), show.legend = FALSE, box.padding = 4, segment.color = 'grey80') +
            scale_color_manual("Status",
                               values = c('exposure' = exposureCol, 'outcome' = outcomeCol),
                               labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome'),
                               na.value = naCol) +
            scale_fill_manual("Status",
                              values = c('exposure' = exposureCol, 'outcome' = outcomeCol),
                              labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome'),
                              na.value = naCol) +
            scale_shape_manual(NULL, guide = 'none',
                               values = c(unadjusted = 'circle', adjusted = 'square'))

        return(p)
    })




observeEvent(input$previous, {
    prevTute = max(1, as.numeric(input$tuteID) - 1)
    updateRadioButtons(session, "tuteID", selected = prevTute)
    button$revealTute <- 0 # Hide solution
    control$vars <- NULL
    pointer$x <- NULL
    pointer$y <- NULL
})

observeEvent(input$advance, {
    nextTute = min(nExamples, as.numeric(input$tuteID) + 1)
    updateRadioButtons(session, "tuteID", selected = nextTute)
    button$revealTute <- 0 # Hide solution
    control$vars <- NULL
    pointer$x <- NULL
    pointer$y <- NULL
})

choiceNames <- reactive({
    list(
    tags$div(HTML(paste0('a <i class="fa ', tute$M1, '" style = "color:', tute$C1, ';"></i>'))),
    tags$div(HTML(paste0('b <i class="fa ', tute$M2, '" style = "color:', tute$C2, ';"></i>'))),
    tags$div(HTML(paste0('c <i class="fa ', tute$M3, '" style = "color:', tute$C3, ';"></i>'))),
    tags$div(HTML(paste0('d <i class="fa ', tute$M4, '" style = "color:', tute$C4, ';"></i>'))),
    tags$div(HTML(paste0('e <i class="fa ', tute$M5, '" style = "color:', tute$C5, ';"></i>'))),
    tags$div(HTML(paste0('f <i class="fa ', tute$M6, '" style = "color:', tute$C6, ';"></i>')))
    )
})

observe({
    updateRadioButtons(session, 'tuteID', choiceValues = 1:nExamples, choiceNames = choiceNames())
})

object <- reactiveValues()

observeEvent(input$tuteID, {
    # Update the possible control variables
    object$tuteChoice <- tidy_dagitty(eval(as.name(paste0('g', input$tuteID))))$data %>%
        filter(! name %in% c('X', 'Y')) %>%
        select(name) %>%
        distinct() %>%
        purrr::flatten_chr()
    updateCheckboxGroupInput(session, "tuteAns", choices = object$tuteChoice, selected = NULL)
    updateCheckboxGroupInput(session, "tuteAns0", choiceNames = "No adjustment needed", choiceValues = "NULL", selected = NULL)
    object$tuteMarks <- NULL
    object$tuteSol <- eval(as.name(paste0("sol", input$tuteID)))
    button$revealTute <- 0 # Hide solution
    control$vars <- NULL
    pointer$x <- NULL
    pointer$y <- NULL
})

# Unclick covariate radio buttons when no adjustment needed is selected
observeEvent(input$tuteAns0, {
    updateCheckboxGroupInput(session, "tuteAns", choices = object$tuteChoice, selected = "NULL")
})

# Unclick 'no adjustment needed' if any covariates are selected
observeEvent(input$tuteAns, {
    updateCheckboxGroupInput(session, "tuteAns0", selected = character(0))
})


# Reveal the solution
output$solutionTute <- renderPlot({

    sol <- eval(as.name(paste0('sol', input$tuteID)))
    # sol <- NULL
    # if (length(sol0) ==1 & sol0 != "NULL"){
    #     sol <- sol0
    #     }

    p <- tidyDagBase() %>%
        adjust_for(sol) %>%
        ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) +
        geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed")) +
        geom_dag_collider_edges(color = 'pink') +
        geom_dag_point(aes(color = status)) +
        geom_dag_text() +
        theme_dag()  +
        scale_adjusted() +
        geom_dag_label_repel(aes(label = label, fill = status), show.legend = FALSE, box.padding = 4, segment.color = 'grey80') +
        scale_color_manual("Status",
                           values = c('exposure' = exposureCol, 'outcome' = outcomeCol),
                           labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome'),
                           na.value = naCol) +
        scale_fill_manual("Status",
                          values = c('exposure' = exposureCol, 'outcome' = outcomeCol),
                          labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome'),
                          na.value = naCol) +
        scale_shape_manual(NULL, guide = 'none',
                           values = c(unadjusted = 'circle', adjusted = 'square'))

    if (button$revealTute==1) {
        p
    }

})

object$tuteMarks <- NULL
object$tuteSol <- c("Well head")

# Calculate marks
# observeEvent(input$tuteSubmit,{
#
#     submission <- c(input$tuteAns0, input$tuteAns)
#     object$tuteMarks <- ifelse(all.equal(submission, object$tuteSol) == TRUE, TRUE, FALSE)
#     eval(as.name(paste0('tute$P', input$tuteID))) <- ifelse(object$tuteMarks, 1, 2)
#
# })


df <- reactive({
    tibble(g = 1:nExamples)
})

output$test <- renderText({
    # paste("x =", pointer$x, "y =", pointer$y)
    control$vars
                    })

}

# Run the application
shinyApp(ui = ui, server = server)
