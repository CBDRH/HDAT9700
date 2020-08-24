###################################################################
# Program:  Master of Science in Health Data Science, UNSW Sydney
# Course:   HDAT9700
# Chapter:  Matching for causal inference
# Topic:    Matching removes model dependency
# Author:   Mark Hanly
###################################################################

library(shiny)
library(dagitty)
library(ggdag)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Backdoor paths, alright!"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("nodes", "Choose the number of nodes", value = 5, min = 3, max = 7, step = 1),
            sliderInput("connectivity", "Set the degree of connectivity", value = 0.5, min = 0.2, max = 0.8, step = 0.1),
            helpText("Choosing a low number of nodes with low connectivity will produce a trivial or boring DAG. On the other hand,
                     choosing a high number of nodes with high connectivity will result in a very complex DAG"),
            actionButton("userGenerate", "Generate DAG"),
            uiOutput("answerSet"),
            hr(),
            actionButton("userSubmit", "Submit answer"),
            uiOutput("marks"),
            actionButton("userReveal", "Reveal solution")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            uiOutput("instructions"),
           plotOutput("question"),
           uiOutput("solutionTitle"),
           textOutput("solutionText"),
           plotOutput("solutionDAG"),
           uiOutput("solution2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

# Update the outcome based on the chosen number of nodes
outcome <- reactive({
    paste0('x', input$nodes)
})


# Update instructions
output$instructions <- renderUI({

 text <- paste("Specify a minimal sufficient adjustment set that will close any backdoor paths and allow
               you to identify the direct effect of x1 (exposure) on", outcome(), "(outcome).")

 h3(text)

})

# reactive values for object
object <- reactiveValues()

button <- reactiveValues(generate = 1, submit = 0, reveal = 0)

# Actions when user hits generate button
observeEvent(input$userGenerate, {

    button$submit <- 0
    button$reveal <- 0
    button$generate <- 1

    object$rd <- dagitty::randomDAG(input$nodes, input$connectivity)

    # This text will ractively update to specify the largest node as the outcome
    sub <- paste0( "x1 -> ", outcome(), "\n x1 [exposure] ", outcome(), "[outcome] }\n")

    # This updates the text of the Random DAG specification to include exposure and outcome specified in dagitty syntax
    object$rd0 <- gsub("}\n", sub, object$rd) %>% dagitty() %>% adjust_for(NULL)

    # This finds the minimal viable adjustment sets
    object$adjSets <- adjustmentSets(object$rd, exposure = 'x1', outcome = outcome(), type = 'minimal', effect = 'direct')

    # This counts and stores the number of solutions
    object$nSets <- length(object$adjSets)

    # This plots the puzzle, with exposure and outcome labelled
    object$question <- ggdag_status(object$rd0) + theme_dag() + ggpubr::border()

    # Update the possible control variables
    object$choiceNames <- c("No adjustment needed", paste0('x', seq(2,input$nodes-1)))
    object$choiceValues <- c("NULL", paste0('x', seq(2,input$nodes-1)))
    updateCheckboxGroupInput(session, "ans", choiceNames = object$choiceNames, choiceValues = object$choiceValues)

    # Update the selected solutions (always revert to 1)
    updateRadioButtons(session, "solutionID", selected = 'Solution 1')

})

# Submit button logic
observeEvent(input$userSubmit, {
    button$submit <- abs(button$submit - 1)
})

# Reveal button logic
observeEvent(input$userReveal, {
    button$reveal <- abs(button$reveal - 1)
})


# Plot the randomly generated DAG
output$question <- renderPlot({
    object$question
})

# Show the solution options
output$answerSet <- renderUI({
    if (button$generate == 1) {
        checkboxGroupInput("ans", "Choose variables to control for:", choices = NULL)
    }

    else (
        return(NULL)
    )
})

# Reveal the solution
output$solutionDAG <- renderPlot({

    req(object$rd0)

    index <- max(1, as.numeric(input$solutionID))

    x <- object$adjSets[[index]]
    df <- object$rd0 %>% adjust_for(x)
    p <- ggdag_adjust(df, exposure = 'x1', outcome = outcome()) + theme_dag()

    if (button$reveal==1) {
        p
    }

})

# Show solution options if more than 1 solution
output$solution2 <- renderUI({

    req(object$nSets)

    if (object$nSets >=2 & button$reveal==1 ) {
        # Update the possible solution to view
        choiceSolutions <- c(paste0('Solution', seq(1, object$nSets)))
        radioButtons("solutionID",
                     "There was more than one valid solution, choose which to view:",
                     choiceNames = choiceSolutions,
                     choiceValues = seq(1:object$nSets))
    }

    else {
        return(NULL)
    }

})

output$solutionTitle <- renderUI({
    if (button$reveal ==1) {
        h4("Solution:")
    }

    else {
        return(NULL)
    }
})

output$solutionText <- renderText({

    if (button$reveal ==1) {

        if (length(object$adjSets[[1]]) == 0) {
            text <- "No adjustment required!"
        }

        else if (object$nSets==1) {
            text <- as.character(object$adjSets[[1]])
        }

        else if (object$nSets>1) {
            index <- max(1, input$solutionID)
            text <- object$adjSets[[index]]
        }

        text

    }

    else {
        return(NULL)
    }


})



marks <- reactive({
    if (button$submit == 1) {

        mark <- "Incorrect :("

        if (length(object$adjSets[[1]]) == 0 & all(input$ans == "NULL")) {
            mark <- "Correct!"
        }

        else if (length(object$adjSets[[1]]) > 0) {

            for (i in 1:object$nSets) {

                if (all(input$ans == object$adjSets[[i]])) {
                    mark <- "Correct!"
                }

            }

        }

mark

    }

    else {
        return(NULL)
    }
})

# Text to determine if a valid solution was chosen
output$marks <- renderUI({
  h3(marks())
})




}

# Run the application
shinyApp(ui = ui, server = server)



## Order from Thai Paragon ##

# Prawn cake
# Chilli peppercorn tofu (extra chilli)
# Large egg-fried rice
# Coconut ice-cream with sticky rice
