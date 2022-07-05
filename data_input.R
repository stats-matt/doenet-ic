events <- reactive({
  data <- read.csv('experiment1Events.csv')
  return(data)
})
attempts <- reactive({
  data <- read.csv('experiment1AssignmentAttempts.csv')
  return(data)
})

# events <- reactive({
#     inevents <-
#         input$eventsfile
#     if (is.null(inevents)) {
#         # User has not uploaded a file yet
#         return(NULL)
#     }
#     read.csv(inevents$datapath)
# })
events_clean <- reactive({
  data <- events()
  data <- clean_events(data)
  return(data)
})
# attempts <- reactive({
#     infile <- input$attemptsfile
#     if (is.null(infile)) {
#         # User has not uploaded a file yet
#         return(NULL)
#     }
#     read.csv(infile$datapath)
# })
attempts_clean <- reactive({
  attempts() %>% clean_attempts()
  })

summary_data <- reactive({
  events_clean() %>%
    group_by(userId, version) %>%
    summarise(
      pre = max(credit.prob.1, na.rm = T),
      activity = max(credit.prob.2, na.rm = T),
      post = max(credit.prob.3, na.rm = T)
    )
})