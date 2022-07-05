library(shiny)
library(tidyverse)
library(jsonlite)
library(DT)
library(anytime)
library(shinythemes)
library(plotly)
library(ltm)

source('clean.R')

ui <- fluidPage(
    titlePanel("Results of Doenet Experiment"),
    #shinythemes::themeSelector(),
    theme = shinythemes::shinytheme('cerulean'),
    sidebarLayout(sidebarPanel(
        #helpText("Upload your files from your doenet logs")
        helpText(
            "This presents results from the Doenet experiment at Ithaca College. Please select a tab to the right to see different analyses. Doenet is a collaborative project involving the University of Minnesota, the Ohio State University, and Ithaca College, with support from the National Science Foundation (DUE-1915294, DUE-1915363, DUE-1915438). Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation."
        )
        # fileInput(
        #     'attemptsfile',
        #     'Choose Attempts file',
        #     accept = c('text/csv', 'text/comma-separated-values,text/plain')
        # ),
        # fileInput(
        #     'eventsfile',
        #     'Choose Events file',
        #     accept = c('text/csv', 'text/comma-separated-values,text/plain')
        # )
    ),
    mainPanel(
        tabsetPanel(
            tabPanel(
                'Class Results Graphs',
                "This set of graphs shows the distribution of total scores over the entire document.",
                plotlyOutput('class_hist'),
                plotlyOutput('class_bw')
            ),
            tabPanel(
                'Class Results Summary Table',
                "This is a summary table of each student's id, the version of the page they recieved, and their final scores on each page.",
                DT::DTOutput('summary_table')
            ),
            tabPanel(
                't-test',
                "This t-test demonstrates whether there is a significant different in the final scores in students based on the verion of the page they recieved.",
                verbatimTextOutput('ttest1')
            ),
            tabPanel(
                'Individual Progress Over Time',
                "These plots demonstrate the amount of time to achieve a given amount of credit on each of the problem on the page (introducotry questions, activity, post-activiy questions).",
                plotlyOutput('p1'),
                plotlyOutput('p2'),
                plotlyOutput('p3')
            ),
            tabPanel(
                'Post vs Pre',
                "This plot demonstrates the relationship between the score on the pre-activity questions and the post-activity questions, separated by experimental treatment. By looking at which line has a higher expected post-activity score for a given pre-activity score, one could suggest a version best suited to that student's background.",
                plotlyOutput('postvpre')
            ),
            tabPanel(
                'Document Credit',
                "This plot shows the amount of time each student took to achieve a given score on the entire lesson.",
                plotlyOutput('doccredit')),
            tabPanel(
                'IRT',
                "This panel provides plots and parameter estimates for item response theory models of the questions on the page. In this plot, each page is collapsed into one question, using a cutoff of 50% for correctness. Because the number of students is small, the particular values should be taken with a grain of salt, but this demonstrates what can be done with these values.",
                br(),br(),
                uiOutput("model_num"),
                column(7, uiOutput('irt')),
                column(5, uiOutput('irt_table'))
            )
        )
    ))
)

server <- function(input, output) {
    source("./data_input.R")
    
    output$model_num <- renderUI({
        selectInput(
            "selected_model",
            "Select IRT Model",
            choices = c("Rasch", "1PL", '2PL', "3PL"),
            selected = "Rasch"
        )
    })
    
    model_rasch <- reactive({
        rasch(irt_data_final(), constraint = cbind(ncol(irt_data_final()) + 1, 1))
    })
    
    model_1pl <- reactive({
        rasch(irt_data_final())
    })
    
    model_2pl <- reactive({
        ltm(irt_data_final() ~ z1)
    })
    
    model_3pl <- reactive({
        tpm(irt_data_final())
    })
    
    output$irt <- renderUI({
        if (input$selected_model == "Rasch") {
            output$plot1 <- renderPlot({
                plot(model_rasch())
            })
            plotOutput("plot1")
        }
        
        else if (input$selected_model == "1PL") {
            output$plot2 <- renderPlot({
                plot(model_1pl())
            })
            plotOutput("plot2")
        }
        
        else if (input$selected_model == "2PL") {
            output$plot3 <- renderPlot({
                plot(model_2pl())
            })
            plotOutput("plot3")
        }
        
        else if (input$selected_model == "3PL") {
            output$plot4 <- renderPlot({
                plot(model_3pl())
            })
            plotOutput("plot4")
        }
    })
    
    output$irt_table <- renderUI({
        if (input$selected_model == "Rasch") {
            output$table1 <- renderPrint({
                coef(model_rasch(),
                     prob = TRUE,
                     order = TRUE)
            })
            verbatimTextOutput("table1")
        }
        
        else if (input$selected_model == "1PL") {
            output$table2 <- renderPrint({
                coef(model_1pl(),
                     prob = TRUE,
                     order = TRUE)
            })
            verbatimTextOutput("table2")
        }
        
        else if (input$selected_model == "2PL") {
            output$table3 <- renderPrint({
                coef(model_2pl(),
                     prob = TRUE,
                     order = TRUE)
            })
            verbatimTextOutput("table3")
        }
        
        else if (input$selected_model == "3PL") {
            output$table4 <- renderPrint({
                coef(model_3pl(),
                     prob = TRUE,
                     order = TRUE)
            })
            verbatimTextOutput("table4")
        }
        
    })
    
    
    
    
    output$summary_table <- DT::renderDT({
        summary_data()
    })
    
    output$class_hist <- renderPlotly({
        ggplot(data = attempts_clean(), aes(x = credit, fill = version)) +
            geom_histogram(position = "dodge") +
            labs(y = "Count", x = "Total Credit")
    })
    
    output$class_bw <- renderPlotly({
        fig <-
            ggplot(attempts_clean(),
                   aes(
                       y = as.numeric(credit),
                       x = as.factor(version),
                       fill = as.factor(version)
                   )) +
            geom_boxplot() + coord_flip() + labs(y = "Total Credit", x = "Version of Page")
        fig <- ggplotly(fig)
        fig
    })
    #p <- ggplot(dat, aes(x=cond, y=rating, fill=cond)) + geom_boxplot()
    #fig <- ggplotly(p)
    #fig
    
    output$ttest1 <- renderPrint({
        t.test(attempts_clean()$credit ~ attempts_clean()$version)
    })
    
    output$postvpre <- renderPlotly({
        ggplot(data = summary_data(), aes(
            x = pre,
            y = post,
            color = version
        )) +
            geom_jitter(height = 0.1, width = 0.1) +
            geom_smooth(method = "lm", se = F) +
            labs(x = "Pre-activity score", y = "Post-activity score")
    })
    
    output$doccredit <- renderPlotly({
        events_clean() %>%
            filter(!is.na(documentCreditAchieved)) %>%
            ggplot(aes(
                x = time,
                y = documentCreditAchieved,
                color = userId
            )) +
            geom_line() +
            facet_wrap(~ version)+
            labs(x = "Time", y = "Total lesson credit")+
            theme(legend.position = "none")
    })
    output$p1 <- renderPlotly({
        events_clean() %>%
            filter(!is.na(credit.prob.1)) %>%
            ggplot(aes(
                x = time,
                y = credit.prob.1,
                color = userId
            )) +
            geom_line() +
            facet_wrap(~ version) +
            labs(title = "Problem 1",
                 y = "Credit",
                 x = "Time")
    })
    output$p2 <- renderPlotly({
        events_clean() %>%
            filter(!is.na(credit.prob.2)) %>%
            ggplot(aes(
                x = time,
                y = credit.prob.2,
                color = userId
            )) +
            geom_line() +
            facet_wrap(~ version) +
            labs(title = "Problem 2",
                 y = "Credit",
                 x = "Time")
    })
    output$p3 <- renderPlotly({
        events_clean() %>%
            filter(!is.na(credit.prob.3)) %>%
            ggplot(aes(
                x = time,
                y = credit.prob.3,
                color = userId
            )) +
            geom_line() +
            facet_wrap(~ version) +
            labs(title = "Problem 3",
                 y = "Credit",
                 x = "Time")
    })
    output$model <- renderText({
        summary_data() %>%
            filter(pre != "-Inf", activity != "-Inf", post != "-Inf") %>%
            lm(data = ., post ~ pre + version) %>%
            summary()
    })
    
    # irt stuff
    
    irt_data <- reactive({
        events_clean() %>%
            dplyr::select(userId,
                          version,
                          credit.prob.1,
                          credit.prob.2,
                          credit.prob.3)
    })
    
    p1 <- reactive({
        irt_data() %>%
            filter(!is.na(credit.prob.1)) %>%
            dplyr::select(-credit.prob.2,-credit.prob.3) %>%
            distinct() %>%
            slice_max(n = 1, order_by = credit.prob.1)
    })
    
    p2 <- reactive({
        irt_data() %>%
            filter(!is.na(credit.prob.2)) %>%
            dplyr::select(-credit.prob.1,-credit.prob.3) %>%
            distinct() %>%
            slice_max(n = 1, order_by = credit.prob.2)
    })
    
    p3 <- reactive({
        irt_data() %>%
            filter(!is.na(credit.prob.3)) %>%
            dplyr::select(-credit.prob.1,-credit.prob.2) %>%
            distinct() %>%
            slice_max(n = 1, order_by = credit.prob.3)
    })
    
    irt_data_joined <- reactive({
        left_join(p1(), p2()) %>%
            left_join(p3())
    })
    
    irt_data_final <- reactive({
        irt_data_joined() %>%
            mutate(
                p1 =
                    case_when(credit.prob.1 >= 0.5 ~ 1,
                              TRUE ~ 0),
                p2 =
                    case_when(credit.prob.2 >= 0.5 ~ 1,
                              TRUE ~ 0),
                p3 =
                    case_when(credit.prob.3 >= 0.5 ~ 1,
                              TRUE ~ 0)
            ) %>%
            ungroup() %>%
            dplyr::select(p1, p2, p3)
    })
    
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
