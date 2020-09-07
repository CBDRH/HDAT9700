###################################################################
# Program:  Master of Science in Health Data Science, UNSW Sydney
# Course:   HDAT9700
# Chapter:  Matching for causal inference
# Topic:    Exploring the MatchIt package
# Author:   Mark Hanly
###################################################################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(MatchIt)
library(dplyr)
library(tidyr)
library(loldata)
library(tibble)
library(knitr)
library(kableExtra)
library(ggpubr)
library(DT)
library(sjPlot)

# Country-level data from loldata package
df <- loldata::worldrankings %>%
    select(-c("income", "literacy", "smartphone_adoption", "internet_speed", "gun_deaths", "happiness", "web_index", "discrimination_index")) %>%
    mutate(gini = as.numeric(gini),
           high_ge = gender_equality > 0.75) %>%
    filter(gini>=0) %>%
    filter(mice::cci(.)) %>%
    tibble::rownames_to_column() %>% as.data.frame()

# subset of data excluding outcome and country name
dfMatch <- df %>% select(-c('country', 'gender_equality', 'high_ge', 'satisfaction', 'rowname'))


ui <- dashboardPage(

    dashboardHeader(title = "MatchIt"),


    dashboardSidebar(

        sidebarMenu(
            menuItem("Run matching", tabName = "dashboard", icon = icon("th")),
            menuItem("Visualise balance", tabName = "balance", icon = icon("th")),
            menuItem("Estimate effect", tabName = "outcome", icon = icon("th")),
            menuItem("Raw Data", tabName = "rawData", icon = icon("dashboard")),
            menuItem("Matched Data", tabName = "matchedData", icon = icon("dashboard"))
        )

    ),


    dashboardBody(
        tabItems(

        tabItem(tabName = "dashboard",
            fluidRow(

            column(width = 5,

                box(title = "MatchIt options", width = 12, solidHeader = TRUE,

                    helpText(
                        HTML(
                            paste("The ", tags$code('matchit()'), "function takes the general form", tags$strong('matchit(formula, data, method = "nearest", distance = "logit"...)',),
                                  ". Some of these arguments can be modified below."
                                )
                            )
                        ),

                    hr(),

                     # Choose variables to match on
                    varSelectizeInput("matchVars",
                                      "Choose variables to match on",
                                      data=dfMatch,
                                      multiple = TRUE,
                                      selected = c('gini', 'education')),
                    helpText(HTML(paste("This is the", tags$code('formula'), "argument."))),
                    br(),

                    # Chooose matching method
                    selectInput("method",
                                "Choose a matching method",
                                choices = c('Exact matching' = 'exact', 'Nearest neighbour matching' = 'nearest', 'Optimal matching' = 'optimal', 'Coarsened exact matching' = 'cem'),
                                selected = 'nearest'),
                    helpText(HTML(paste("This is the", tags$code('method'), "argument."))),
                    br(),
                    # Reactive options based on choice of matching method
                    conditionalPanel(
                        condition = "input.method=='optimal' || input.method == 'nearest'", ## uses the Java Script 'OR' operator: ||


                        # Distance metric
                        selectInput("distance",
                                    "Specify a distance metric",
                                    choices = c('Propensity score (using logistic regression)' = 'logit', 'Mahalanobis distance' = 'mahalanobis'),
                                    selected = 'nearest'),
                        helpText(HTML(paste("This is the", tags$code('distance'), "argument."))),
                        br(),

                        # Matching ratio
                        numericInput("ratio", "choose the matching ratio (1:N)",
                                     value=1, min=1, max = 4),
                        helpText(HTML(paste("This is the", tags$code('ratio'), "argument."))),
                        br(),

                        # Caliper
                        sliderInput("caliper",
                                    "Set the caliper distance",
                                    min = 0, max = 3, step = 0.1, value = 0),
                        helpText(HTML(paste("This is the", tags$code('caliper'), "argument.")))


                    ),

                    br(),
                    # Conditional panel if a caliper is specified
                    conditionalPanel(
                        condition = "(input.method=='optimal' || input.method == 'nearest') & input.caliper > 0",

                        # Matching ratio
                        checkboxInput("calClosest", "Take the nearest available match if no matches are available within the caliper?",
                                      value=FALSE),
                        helpText(HTML(paste("This is the", tags$code('calclosest'), "argument.")))
                    ),
                    br(),
                    # Button to run matching
                    actionButton("runMatch", "Match!", width = '100%', icon = icon("random"))


                    ),

                # Output from matchit function
                box(title = "MatchIt output", width = 12, solidHeader = TRUE,
                    helpText(HTML(paste("Below are the results of calling", tags$code("summary()"), "on a", tags$code("matchit"), "object."))),
                    verbatimTextOutput("matchSum")
                )

                ),

            # Right-hand side column
            column(width = 7,
                box(title = "Overview", width = 12, solidHeader = TRUE, collapsible = TRUE,

                    h4("MatchIt::matchit()"),
                    helpText(
                        HTML(
                            paste(
                                "The", tags$code('MatchIt::matchit()'), "function implements numerous matching methods, with a variety of options and arguments
                             available for each method.", 'This applet allows you to explore a small subset of those methods and options.',
                                br(), br(), "Enter", tags$code('?matchit'), "at the console, or check out the documentation paper below to find out more about
                                the methods and options available."
                            )
                        )
                    ),

                    hr(),

                    h4("Data"),
                    helpText(
                        HTML(
                            paste0(
                                "The data available in this applet include a variety of measures and indices recorded for 97 countries and taken from the",
                                tags$code("worldrankings"), " dataset from the",
                                tags$code("loldata"), " package. You can read more about these data ", tags$a(href="https://github.com/tadaadata/loldata", "here"), ".",
                                br(), br(),
                                "For the purposes of this exerices, the ", tags$strong('exposure'), " has been chosen as 'high gender equality', a binary variable defined as a national
                                gender gap index above 0.70. This is recorded in the variable ", tags$code("high_ge"), ". Fifteen out of 97 countries have achieved this status,
                                including Norway, South Africa and Nicaragua. You can read more about the gender gap index ", tags$a(href = 'https://en.wikipedia.org/wiki/Global_Gender_Gap_Report', 'here'), ".",
                                br(), br(),
                                "Also for the purpose of this exercise, we will consider 'satisfaction with life' as the ", tags$strong('outcome'), " of interest. This is a
                                continuous variable named ", tags$code('satisfaction'), "based on the ", tags$a(href = 'https://en.wikipedia.org/wiki/Satisfaction_with_Life_Index', 'satisfaction with life index'),
                                ". Thus, our basic aim is to estimate the effect of achiving high gender equality on national satisfaction. We can imagine there will be an effect, but it is also easy to see how this effect could be confounded. For example,
                                countries with higher levels of education may be more likely to promote gender equality but education may also impact national satisfaction through a separate pathway (what would
                                the DAG look like for this relationship?).",
                                br(),br(),
                                'This is just an example to get you started with the ', tags$code('matchit()'), " function, so don't worry too much about the choice of variables. But remember - just
                                like when choosing control variables - just because a variable is available for matching doesn't mean you should match on it!"
                            )
                        )
                    ),

                    hr()

                ),

                # Interactive box with scrollable documentation pdf
                box(title = "MatchIt documentation", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,

                    tags$a(href = "https://cran.r-project.org/web/packages/MatchIt/vignettes/matchit.pdf", "Stuart, Elizabeth A., Gary King, Kosuke Imai, and Daniel Ho. MatchIt: nonparametric preprocessing for parametric causal inference. Journal of Statistical software (2011)."),

                    tags$iframe(style="height:800px; width:100%; scrolling=yes", src="https://cran.r-project.org/web/packages/MatchIt/vignettes/matchit.pdf")
                ),


                # Summary of matched countries
                box(title = "Matched countries", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,

                    helpText("For 1:N distance-based matching methods, a table of matched countries will appear below"),
                    htmlOutput("matchedCountries")

                )



                ) # Closes right-hand column
            ) # closes fluidRow
        ),

        # Sumamry of variable balance
        tabItem(tabName = "balance",
                radioButtons("balancePlotVar", "Choose a variable to compare the balance in the matched and unmatched data", choices = c('a')),
                  plotOutput("matchPlot")
        ),



        # Raw data
        tabItem(tabName = "rawData",
                DT::dataTableOutput("rawData")
        ),

        # Matched data
        tabItem(tabName = "matchedData",

                helpText({
                   HTML(paste(
                       "You can access the matched data by calling", tags$code('match.data()'), "on a", tags$code('matchit'), "object."
                   ))
                }),
                hr(),
                DT::dataTableOutput("matchedData")
        ),


        # Outcome
        tabItem(tabName = "outcome",
                h3("Remember, in practice you would only check your treatment effect once you are happy with the balance of matching variables!"),
                box(title = "Estimated treatment effect in the raw data",
                    htmlOutput("tab1")
                    ),

                box(title = "Estimated treatment effect in the matched data",
                    htmlOutput("tab2")
                    )
        )

        ) # Closes tab items
    ) # Close dashboard body
) # Close dashboard page


#############################################################

### Server

#############################################################

server <- function(input, output, session) {

# Data based on selected variables
data <- reactive({
    req(input$matchVars)
    return(df %>% select(c("high_ge", !!!input$matchVars)))
    })

# List of selected vars
observe({
    updateRadioButtons(session, "balancePlotVar", choices = c(input$matchVars, "NULL"))
})


match <- eventReactive(input$runMatch, {

    if (input$method %in% c("exact", "cem")) {

        matchit(formula = as.formula(paste("high_ge~", paste(input$matchVars, collapse = '+'))),
                data = df,
                method = input$method)

    }

    else if (input$method %in% c("nearest", "optimal")) {
    matchit(formula = as.formula(paste("high_ge~", paste(input$matchVars, collapse = '+'))),
            data = df,
            method = input$method,
            ratio = input$ratio,
            caliper = input$caliper,
            calclosest = input$calClosest)
    }

})

matchSum <- reactive({
    summary(match())
    })

output$matchSum <- renderPrint({
    matchSum()[-1]
    })

matchPlot <- reactive({
    req(match())

    d <- df %>%
        mutate(wt = match()$weights)

    p1 <- ggplot(data = d, aes_string(x = input$balancePlotVar, fill = 'high_ge')) +
            geom_density(alpha = 0.60) +
            scale_fill_manual(name = NULL, values=c('dodgerblue','tomato2'), label = c('Control', "Treated")) +
            labs(title = "Unmatched data")
    p2 <- ggplot(data = d, aes_string(x = input$balancePlotVar, fill = 'high_ge', weight = 'wt')) +
        geom_density(alpha = 0.60) +
        scale_fill_manual(name = NULL, values=c('dodgerblue','tomato2'), label = c('Control', "Treated")) +
        labs(title = "Unmatched data")
    p3 <- ggpubr::ggarrange(p1, p2, common.legend = TRUE, legend = 'top')

    return(p3)

})


output$matchPlot <- renderPlot({
    matchPlot()
})

# Create a table of the matched countries

matchedCountries <- eventReactive(input$runMatch, {

    mm <- as.data.frame(match()$match.matrix) %>% tibble::rownames_to_column()

    x1 <- mm %>%
        pivot_longer(cols = names(mm)) %>%
        left_join(df, by = c('value' = 'rowname')) %>%
        select(c('name', 'country'))

    x2 <- pivot_wider(x1, names_from = 'name', values_from = 'country')

    x3 <- do.call(cbind, lapply(x2, function(x) as.character(unlist(x)))) %>% as.data.frame()

    kable(x3, col.names = c('Treated country', paste("Match", seq(1:input$ratio)))) %>%
        kable_styling(
            font_size = 15,
            bootstrap_options = c("striped", "hover", "condensed")
        )
})

# Output of matched countries
output$matchedCountries <- renderText(matchedCountries())

# OPutput the raw data
output$rawData <- DT::renderDataTable({
    DT::datatable(df[-1], options = list(lengthMenu = c(10, 20, 50, 100), pageLength = 100))
})

# Output the matched dataset
output$matchedData <- DT::renderDataTable({
    DT::datatable(match.data(match())[-1], options = list(lengthMenu = c(10, 20, 50, 100), pageLength = 100))
})

# Raw tratment effect estimates
output$tab1 <- renderText({
    mod <- lm(satisfaction ~ high_ge, data=df)
    tab <- tab_model(mod, dv.labels = "Model predicting satisifaction", show.r2 = FALSE)
    HTML(tab$knitr)
})

# Treatment effect estimates
output$tab2 <- renderText({
    mod <- lm(satisfaction ~ high_ge, data=match.data(match()), weights = weights)
    tab <- tab_model(mod, dv.labels = "Model predicting satisifaction", show.r2 = FALSE)
    HTML(tab$knitr)
})

}

shinyApp(ui, server)

